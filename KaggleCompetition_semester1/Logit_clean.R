library(data.table)
library(caret)
library(rpart)
library(dplyr)
library(plyr)


rm(list =ls())
set.seed(1234567)
par(mar=c(3,3,3,3))
options('scipen' = 100, 'digits' = 4)

train <- data.frame(fread('BADS_WS1920_known.csv', stringsAsFactors = FALSE, header = TRUE))

test <- data.frame(fread('BADS_WS1920_unknown.csv', stringsAsFactors = FALSE, header = TRUE))

#DATES
library(lubridate)

##finding time period between order and delivery 
train <- train%>% mutate(duration_order_delivery = as.numeric(as.period(interval(as.POSIXlt(train$order_date), as.POSIXlt(train$delivery_date)))$day))
train$duration_order_delivery <- ifelse(train$duration_order_delivery<0, 0, train$duration_order_delivery)
train$duration_order_delivery <- ifelse(is.na(train$duration_order_delivery)==TRUE, 0, train$duration_order_delivery)

test <- test%>% mutate(duration_order_delivery = as.numeric(as.period(interval(as.POSIXlt(test$order_date), as.POSIXlt(test$delivery_date)))$day))
test$duration_order_delivery <- ifelse(test$duration_order_delivery<0, 0, test$duration_order_delivery)
test$duration_order_delivery <- ifelse(is.na(test$duration_order_delivery)==TRUE, 0, test$duration_order_delivery)

##finding age at order 
age_train <- as.period(interval(as.POSIXlt(train$user_dob), as.POSIXlt(train$order_date)))
train <- train %>% mutate(approx_age_at_order = round(age_train$year + age_train$month/12))
train$approx_age_at_order <- ifelse(train$approx_age_at_order < 10, 10, train$approx_age_at_order) 
train$approx_age_at_order <- ifelse(train$approx_age_at_order >60, 60, train$approx_age_at_order) 
train$approx_age_at_order <- ifelse(is.na(train$approx_age_at_order) == TRUE, 
                                    mean(train$approx_age_at_order, na.rm =TRUE), train$approx_age_at_order)

age_test <- as.period(interval(as.POSIXlt(test$user_dob), as.POSIXlt(test$order_date)))
test <- test %>% mutate(approx_age_at_order = round(age_test$year + age_test$month/12))
test$approx_age_at_order <- ifelse(test$approx_age_at_order < 10, 10, test$approx_age_at_order) 
test$approx_age_at_order <- ifelse(test$approx_age_at_order >60, 60, test$approx_age_at_order) 
test$approx_age_at_order <- ifelse(is.na(test$approx_age_at_order) == TRUE, 
                                   mean(train$approx_age_at_order, na.rm =TRUE), test$approx_age_at_order)

##membership duration as loyalty

interval_reg_ord_train <- as.period(interval(as.POSIXlt(train$user_reg_date), as.POSIXlt(train$order_date)))
train <- train %>% mutate(loyalty = (interval_reg_ord_train$year)*12 +interval_reg_ord_train$month + interval_reg_ord_train$day/30)
train["reg_after_order"] <- ifelse(train$loyalty<0, 1,0)

interval_reg_ord_test <- as.period(interval(as.POSIXlt(test$user_reg_date), as.POSIXlt(test$order_date)))
test <- test %>% mutate(loyalty = (interval_reg_ord_test$year)*12 +interval_reg_ord_test$month + interval_reg_ord_test$day/30)
test["reg_after_order"] <- ifelse(test$loyalty<0, 1,0)

#NOT DELIVERED

train["not_delivered"] <- ifelse(is.na(train$delivery_date) == TRUE, 1, 0)
test["not_delivered"] <- ifelse(is.na(test$delivery_date) == TRUE, 1, 0) 

#BRAND OUTLIERS

a<-c(10,27,123,125,126,32,62,130,132,65, 67,134,136,
     83,85,86,110,111,112,114)
b<- c(25,58,81,88,89,92,96,113,120)

for(i in 1:length(a)) {
  train["brand_outliers"] <- ifelse(train$brand_id == a[i] , "low", "normal")
}
for(i in 1:length(a)) {
  test["brand_outliers"] <- ifelse(test$brand_id == a[i] , "low", "normal")
}
for(i in 1:length(b)) {
  train["brand_outliers"] <- ifelse(train$brand_id == b[i] , "high", train$brand_outliers)
}
for(i in 1:length(b)) {
  test["brand_outliers"] <- ifelse(test$brand_id == b[i] , "high", train$brand_outliers)
}

#USER FREQUENCY

count_train <- count(train, "user_id")
train <- merge(train, count_train, by = "user_id")
colnames(train)[21] <- "user_freq"
head(train, 2)

count_test <- count(test, "user_id")
test <- merge(test, count_test, by ="user_id")
colnames(test)[20] <- "user_freq"
head(test,2)


#ITEM FREQUENCY

count_train <- count(train, "item_id")
train <- merge(train, count_train, by = "item_id")
colnames(train)[22]<- "item_freq"
head(train,2)

count_test <- count(test, "item_id")
test <- merge(test, count_test, by = "item_id")
colnames(test)[21] <- "item_freq"
head(test, 2)

#SPLIT DATA

idx <- sample(nrow(train), 70000)
train_idx <- train[idx,]
val <- train[-idx, ]
table(train_idx$return)


#MODELLING

logit_idx <- glm(return ~ item_price + not_delivered +user_freq +brand_outliers +approx_age_at_order + 
               duration_order_delivery + reg_after_order +item_freq
             ,data = train_idx, family = binomial(link = "logit"))

pred_logit_val <- predict(logit_idx, newdata = val, type = "response")
pred_logit_val <- pred_logit_val 

h <- hmeasure::HMeasure(true.class = val$return, scores = pred_logit_val)
h$metrics["AUC"]

logit_test <- glm(return ~ item_price + not_delivered +user_freq +brand_outliers +approx_age_at_order + 
               duration_order_delivery + reg_after_order +item_freq,
             data = train, family = binomial(link = "logit"))

pred_logit_test <- predict(logit_test, newdata = test, type = "response")

prediction <- data.frame("order_item_id" = test$order_item_id, "return" = pred_logit_test)

write.csv(prediction, file = "logit_test_prediction.csv", row.names = FALSE)

prediction <- prediction %>% arrange(order_item_id)

head(prediction)
