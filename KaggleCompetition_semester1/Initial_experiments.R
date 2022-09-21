library(data.table)
library(caret)
library(rpart)
library(dplyr)
library(plyr)
library(ggplot2)



rm(list =ls())
set.seed(123456)
par(mar=c(3,3,3,3))
options('scipen' = 100, 'digits' = 4)

train <- data.frame(fread('BADS_WS1920_known.csv', stringsAsFactors = FALSE, header = TRUE))
train <- train[, c(1,14,2,3,4,5,6,7,8,9,10,11,12,13)]
train <- train %>% arrange(order_item_id)
train$return <- as.factor(train$return)

test <- data.frame(fread('BADS_WS1920_unknown.csv', stringsAsFactors = FALSE, header = TRUE))
head(test)


#USER TITLE

str(unique(train$user_title))

train$user_title <- ifelse(train$user_title == "Mrs", 0, train$user_title)
train$user_title <- ifelse(train$user_title =="Family", 1, train$user_title)
train$user_title <- ifelse(train$user_title == "not reported", 2, train$user_title)
train$user_title <- ifelse(train$user_title == "Mr", 3, train$user_title)
train$user_title<- ifelse(train$user_title == "Company", 4, train$user_title)
train$user_title <- as.factor(train$user_title)

train$user_title <- as.factor(train$user_title)
test$user_title <- as.factor(test$user_title)

hist(train$user_title)
table(train$return, train$user_title)

knitr::kable(train$user_title)

results <- fastDummies::dummy_cols(train$user_title)

knitr::kable(results)

train$user_title <- as.factor(train$user_title)
test$user_title <- as.factor(test$user_title)
levels(train$user_title)
head(train,2)


#USER STATE

train$user_state <- as.factor(train$user_state)
test$user_state <- as.factor(test$user_state)
levels(train$user_state)
str(unique(train$user_state))

q <- table(train$return, train$user_state)

q[1,]/q[2,]

#DATES
library(lubridate)
colnames(train)

##finding time period between order and delivery 
train <- train%>% mutate(duration_order_delivery = as.numeric(as.period(interval(as.POSIXlt(train$order_date), as.POSIXlt(train$delivery_date)))$day))

train$duration_order_delivery <- ifelse(train$duration_order_delivery<0, 0, train$duration_order_delivery)

train$duration_order_delivery <- ifelse(is.na(train$duration_order_delivery)==TRUE, 0, train$duration_order_delivery)

#sum(is.na(train$duration_order_delivery))
head(train,2)

test <- test%>% mutate(duration_order_delivery = as.numeric(as.period(interval(as.POSIXlt(test$order_date), as.POSIXlt(test$delivery_date)))$day))
test$duration_order_delivery <- ifelse(test$duration_order_delivery<0, 0, test$duration_order_delivery)
test$duration_order_delivery <- ifelse(is.na(test$duration_order_delivery)==TRUE, 0, test$duration_order_delivery)


sum(is.na(train$duration_order_delivery))

###test <- test[which(test$duration_order_delivery >=0),] #subset negative duration



##finding age at order 

library(lubridate)
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


###train <- train[train$loyalty>=0,]
train["reg_after_order"] <- ifelse(train$loyalty<0, 1,0)


interval_reg_ord_test <- as.period(interval(as.POSIXlt(test$user_reg_date), as.POSIXlt(test$order_date)))
test <- test %>% mutate(loyalty = (interval_reg_ord_test$year)*12 +interval_reg_ord_test$month + interval_reg_ord_test$day/30)
test["reg_after_order"] <- ifelse(test$loyalty<0, 1,0)


###tail(train[which(train$loyalty <0),])



#NOT DELIVERED

train["not_delivered"] <- ifelse(is.na(train$delivery_date) == TRUE, 1, 0)
test["not_delivered"] <- ifelse(is.na(test$delivery_date) == TRUE, 1, 0) 



str(unique(train$not_delivered))

     
#ITEM_SIZE



#ITEM COLOR

train$item_color <- as.character(train$item_color)
test$item_color <- as.character(test$item_color)


brown <- c("mocca", "khaki", "brwon", 
           "copper coin", "almond", "cognac", "caramel", 
           "habana", "terracotta", "mahagoni", "beige", "brown")

for(i in 1:length(brown)) {
  train$item_color[train$item_color == brown[[i]]] <- "brown"
}
for(i in 1:length(brown)) {
  test$item_color[test$item_color == brown[[i]]] <- "brown"
}
train$item_color <- ifelse(train$item_color == "brown", 1, train$item_color)
test$item_color <- ifelse(test$item_color == "brown", 1, test$item_color)


blue <- c("blue", "turquoise", "navy", "petrol", "denim", "aquamarine", "azure", "blau", "dark denim", "darkblue", "baltic blue", "aqua", "cobalt blue", "dark navy")

for(i in 1:length(blue)) {
  train$item_color[train$item_color == blue[[i]]] <- "blue"
}
for(i in 1:length(blue)) {
  test$item_color[test$item_color == blue[[i]]] <- "blue"
}
train$item_color <- ifelse(train$item_color == "blue", 2, train$item_color)
test$item_color <- ifelse(test$item_color == "blue", 2, test$item_color)


grey <- c("ash", "silver", "dark grey", "basalt", "grey")

for(i in 1:length(grey)) {
  train$item_color[train$item_color == grey[[i]]] <- "grey"
}
for(i in 1:length(grey)) {
  test$item_color[test$item_color == grey[[i]]] <- "grey"
}
train$item_color <- ifelse(train$item_color == "grey", 3, train$item_color)
test$item_color <- ifelse(test$item_color == "grey", 3, test$item_color)


red <- c("berry", "dark garnet", "red")

for(i in 1:length(red)) {
  train$item_color[train$item_color == red[[i]]] <- "red"
}
for(i in 1:length(red)) {
  test$item_color[test$item_color == red[[i]]] <- "red"
}



purple <- c("magenta", "bordeaux", "aubergine", "amethyst", "currant purple", "fuchsia")

for(i in 1:length(purple)) {
  train$item_color[train$item_color == purple[[i]]] <- "purple"
}
for(i in 1:length(purple)) {
  test$item_color[test$item_color == purple[[i]]] <- "purple"
}



black <- c("black", "anthracite", "ebony")
for(i in 1:length(black)) {
  train$item_color[train$item_color == black[[i]]] <- "black"
}
for(i in 1:length(black)) {
  test$item_color[test$item_color == black[[i]]] <- "black"
}

white <- c("white", "ivory")
for(i in 1:length(white)) {
  train$item_color[train$item_color == white[[i]]] <- "white"
}
for(i in 1:length(white)) {
  test$item_color[test$item_color == white[[i]]] <- "white"
}


green <- c("green", "olive", "nature", "mint", "avocado", "dark olive", "dark oliv", "jade")
for(i in 1:length(green)) {
  train$item_color[train$item_color == green[[i]]] <- "green"
}
for(i in 1:length(green)) {
  test$item_color[test$item_color == green[[i]]] <- "green"
}


yellow <- c("yellow", "ecru", "ocher", "champagne", "curry", "mango", "apricot", "creme", "champagner")
for(i in 1:length(yellow)) {
  train$item_color[train$item_color == yellow[[i]]] <- "yellow"
}
for(i in 1:length(yellow)) {
  test$item_color[test$item_color == yellow[[i]]] <- "yellow"
}

pink <- c("pink", "antique pink")
for(i in 1:length(pink)) {
  train$item_color[train$item_color == pink[[i]]] <- "pink"
}
for(i in 1:length(pink)) {
  test$item_color[test$item_color == pink[[i]]] <- "pink"
}

others <- c("opal", "floral", "striped", "pallid", "curled", "gold", "hibiscus", "ancient", "aviator", "kanel", "?")
for(i in 1:length(others)) {
  train$item_color[train$item_color == others[[i]]] <- "others"
}
for(i in 1:length(others)) {
  test$item_color[test$item_color == others[[i]]] <- "others"
}

orange <- c("coral", "orange") 
for(i in 1:length(orange)) {
  train$item_color[train$item_color == orange[[i]]] <- "orange"
}
for(i in 1:length(orange)) {
  test$item_color[test$item_color == orange[[i]]] <- "orange"
}

table(train$return, train$item_color)

train$item_color <- as.factor(train$item_color)
test$item_color <- as.factor(test$item_color)

levels(train$item_color)
train$item_color <- droplevels(train$item_color)
test$item_color <- droplevels(test$item_color)

#ITEM PRICE

train <- train[which(train$item_price >=0), ]
nrow(train_complete)

#BRAND ID 
class(train$brand_id)
train$brand_id <- as.factor(train$brand_id)
test$brand_id <- as.factor(test$brand_id)

q<-table(train$return, train$brand_id)
q[2,]/(q[1,]+q[2,])




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



#USER ID


count_train <- count(train, "user_id")
train <- merge(train, count_train, by = "user_id")
colnames(train)[21] <- "user_freq"
head(train, 2)


count_test <- count(test, "user_id")
test <- merge(test, count_test, by ="user_id")
colnames(test)[20] <- "user_freq"
head(test,2)
#ITEM ID


library(dplyr)
library(plyr)

count_train <- count(train, "item_id")
train <- merge(train, count_train, by = "item_id")
colnames(train)[22]<- "item_freq"
head(train,2)

count_test <- count(test, "item_id")
test <- merge(test, count_test, by = "item_id")
colnames(test)[21] <- "item_freq"
head(test, 2)



#SPLIT TRAIN AND VALIDATION 

library(caret)
idx <- sample(nrow(train), 70000)
train_idx <- train[idx,]
val <- train[-a, ]
table(train_idx$return)


idx <- createDataPartition(train$return, p = 0.7, list = FALSE)
train_idx <- train[idx, ]
val <- train[-idx,]

#GGPLOT

library(ggplot2)

cor(train_idx$return, train_idx$item_price)


#PARTIAL DEPENDENCE PLOT, XGBOOST, FEATEXP
ggplot(data = train, aes(x=loyalty, y=return)) + geom_point()

library(Hmisc)

featexp(train_feature = train_idx)

library(pdp)

#mlr and xgboost

library('mlr')
library('xgboost')

train_idx_numeric <- train_idx

train_idx_numeric$return <- as.numeric(train_idx_numeric$return)-1
colnames(train_idx)

trained_all <- train_idx[, c()]
knitr::kable(trained_all)


task <- makeClassifTask(data = trained, target = "return", positive = 1)

xgb.mlr <- makeLearner("classif.xgboost", predict.type = "prob", 
                       par.vals = list("nrounds" = 100, "verbose" = 0, "max_depth" = 4, "eta" = 0.15, 
                                       "gamma" = 0, "colsample_bytree" = 0.8, "min_child_weight" = 1, "subsample" = 0.8))

xgb <- mlr::train(xgb.mlr, task = task)

xgb.importance(model = xgb$learner.model, feature_names = colnames(task$env$data))

getFeatureImportance(xgb)

featureImportance = list()

featureImportance[["xgb"]] <- unlist(getFeatureImportance(xgb)$res)

pred_xgb <- data.frame(predict(xgb, newdata = validated))
head(pred_xgb,2)

h <- hmeasure::HMeasure(true.class = validated$return, scores = pred_xgb[,2])
h$metrics["AUC"]

#HISTOGRAMS

hist(train$item_color, probability = TRUE)

#MODELLING
colnames(train_idx)
trained <- train_idx[, c(3,9,10,12,14,15,16,17,18,19,20,21,22)]
factors_train <- train[, c("brand_outliers", "user_title")]



q<-table(train$return, train$user_title)
q[2,]/(q[1,]+q[2,])

validated <- val[,c(3,4,10,15,16,17,18,19,21,22)]
colnames(train_idx)

vars <- c("duration_order_delivery", "approx_age_at_order", "loyalty", "reg_after_order", "not_delivered",
          "brand_outliers", "user_freq", "item_freq")

for(i in 1:length(vars)) {
  logit <- glm(return ~ item_price + not_delivered + get(vars[i]) ,data = train_idx, family = binomial(link = "logit"))
  pred_logit <- predict(logit, newdata = val, type = "response")
  h <- hmeasure::HMeasure(true.class = val$return, scores = pred_logit)
  logit_results[i] <- h$metrics["AUC"]
}

logit <- glm(return ~ item_price + not_delivered +user_freq +brand_outliers +approx_age_at_order + 
               duration_order_delivery + reg_after_order +item_freq + user_freq
             ,data = train_idx, family = binomial(link = "logit"))

pred_logit_val <- predict(logit, newdata = val, type = "response")
h <- hmeasure::HMeasure(true.class = val$return, scores = pred_logit)
h$metrics["AUC"]

pred_logit_test <- predict(logit, newdata = test, type = "response")

prediction <- data.frame("order_item_id" = test$order_item_id, "return" = pred_logit_test)

# Save predictions
write.csv(prediction, file = "logit_prediction.csv", row.names = FALSE)

sum(is.na(prediction$return))

coef(summary(logit))[,4]

prediction <- data.frame("order_item_id" = test$order_item_id, "return" = pred_logit)
write.csv(prediction, file = "glm_prediction.csv", row.names = FALSE)


sum(is.na(train$approx_age_at_order))
sum(is.na(prediction$return))


dt <- rpart(return ~ item_price + user_title + 
              user_state + duration_order_delivery + approx_age_at_order + loyalty + user_id +
              brand_id + item_id, data = train, cp = 0.0016)

pred_dt <- predict(dt, newdata = val, type = "prob")[,2]
h <- hmeasure::HMeasure(true.class = val$return, scores = pred_dt)
h$metrics["AUC"]


pred_dt <- predict(dt, newdata = test, type = "prob")[,2]
prediction <- data.frame("order_item_id" = test$order_item_id, "return" = pred_dt)
write.csv(prediction, file = "dt1_prediction.csv", row.names = FALSE)

head(pred_dt)

sum(is.na(dt))

colnames(train)

train_sub <- train[,c(1,2,7,8,9,11,13,15,16,17)]
colnames(train_sub)
test_sub <- test[, c(1,6,7,8,10,12,14,15,16)]
colnames(test_sub)
train <- train[complete.cases(train),]



library(randomForest)
rf <- randomForest(return ~ ., data=train_idx,
                   ntree=1000, mtry= 4, 
                   sampsize = 300, replace=TRUE, node = 10) # Limit tree size
rf_pred <- predict(rf, val, type = 'prob')[,2]

?randomForest
pred <- predict(rf, newdata = test, type = "prob")[,2]

ModelMetrics::auc(actual= val$return, 
                  predicted=rf_pred)

prediction <- data.frame("order_item_id" = test$order_item_id, "return" = pred)
write.csv(prediction, file = "rf1_prediction.csv", row.names = FALSE)

sum(is.na(prediction))

test[is.na(test),]
head(prediction)

table(train$return)

for(i in 1:length(prediction$return)) {
  if(is.na(prediction$return[i])) {
    prediction$return[i] <- 1
  }
}
sum(is.na(prediction$return))
is.na(prediction$return[7])


write.csv(prediction, file = "rf_prediction.csv", row.names = FALSE)

sum(is.na(test))

library(mlr)
colnames(train)
subs_train <- train[, c(1,2,5,7,8,9,10,11,13,15,16,17)]
subs_test <- test[, c()]
colnames(test)
colnames(subs_train)

levels(test$item_color)
tuning$x

task <- makeClassifTask(data = trained, target = "return", positive = "1")

rf <- makeLearner("classif.randomForest", 
                  predict.type = "prob", # prediction type needs to be specified for the learner 
                  par.vals = list("replace" = TRUE, "importance" = FALSE))

rf.parms <- makeParamSet(
  # The recommendation for mtry by Breiman is squareroot number of columns
  makeIntegerParam("mtry", lower = 4, upper = 8), # Number of features selected at each node, smaller -> faster
  makeDiscreteParam("sampsize", values = c(200,300)), # bootstrap sample size, smaller -> faster
  makeIntegerParam("ntree", lower = 300, upper = 1000) # Number of tree, smaller -> faster
) 




tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE) # !!!!resolution!

rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)


tuning <- tuneParams(rf, task = task, resampling = rdesc,
                     par.set = rf.parms, control = tuneControl, measures = mlr::auc)

tuning$x

# Investigate the results in detail
tuning_results <- generateHyperParsEffectData(tuning, partial.dep = TRUE)

# Get the performance for each parameter combination
tuning_results$data

tapply(tuning_results$data$auc.test.mean, INDEX = c(tuning_results$data$mtry), mean) # the fewer the mtry the higher AUC
#Reminder: mtry=Number of variables randomly sampled as candidates at each split
# Update the learner to the optimal hyperparameters
rf
rf_tuned <- setHyperPars(rf, par.vals = tuning$x)
rf_tuned
modelLib <- list()
yhat <- list()

modelLib[["rf"]] <- mlr::train(rf_tuned, task = task)

# Make prediction on test data
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = val)

yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = test)

pred <- data.frame(yhat["rf"])[,2]
pred

pred <- data.frame("order_item_id" = test_sub$order_item_id, "return" = pred)

pred <- data.frame(yhat)
head(pred)

sum(is.na(pred$return))

str(yhat[["rf"]])

prediction <- data.frame("order_item_id" = test_sub$order_item_id, "return" = as.numeric(pred[,2]))
# Save predictions

write.csv(prediction, file = "rf_prediction.csv", row.names = FALSE)

head(prediction)

# Calculate AUC performance on test set 
mlr::performance(yhat[["rf"]], measures = mlr::auc)

class(prediction$return)
train_idx_noneg <- train_idx[train_idx$loyalty>=0, ]
colnames(train_idx)
head(test)
logit <- glm(return ~ item_price, family = binomial(link = "logit"))

pred_logit <- predict(logit,newdata = test,type = "response")

pred_logit <- predict(logit, newdata = val, type = "response")
h <- hmeasure::HMeasure(true.class = val$return, scores = pred_logit)
h$metrics["AUC"]

sum(is.na(pred_logit))
sum(is.na(test$item_price))

model.svm <- svm(return ~ item_price + item_color)


h <- hmeasure::HMeasure(true.class = train$return, scores = pred_logit)

pred_dt <- predict(dt, newdata = train, type = "prob")[,2]
pred_dt_test <- predict(dt, newdata = test, "prob")[,2]

prediction <- data.frame("order_item_id" = test$order_item_id, "return" = pred_dt_test)



write.csv(prediction, file = "prediction_dt.csv", row.names = FALSE)

h <- hmeasure::HMeasure(true.class =  train$return, scores = pred_dt)
h$metrics["AUC"]

colnames(train)

a <- vector(mode = "character", length = 10)
a<- as.vector(rep("brown", 10))
