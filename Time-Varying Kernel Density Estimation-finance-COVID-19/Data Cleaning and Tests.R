rm(list = ls())

library(rjson)
library(dplyr)
library(tidyr)
library(imputeTS)

setwd('D:/Academics/Academics/MEMS/Semester 3/Non-Semi PM/Research/Time varying kernel densities')


#reading data

data_dax <- read.csv('data/DAX.csv', head = TRUE)
data_dax <- data_dax %>% mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = 'day')) %>% as.data.frame()

data_btc <- read.csv('data/btc.csv', head = TRUE)

data_dow <- read.csv('data/dowjones.csv', head = TRUE)
data_dow <- data_dow[which(data_dow$Adj.Close != 'null'),]
data_dow$Open <- as.numeric(data_dow$Open)
data_dow$High <- as.numeric(data_dow$High)
data_dow$Low <- as.numeric(data_dow$Low)
data_dow$Close <- as.numeric(data_dow$Close)
data_dow$Adj.Close <- as.numeric(data_dow$Adj.Close)
data_dow$Volume <- as.numeric(data_dow$Volume)
data_dow <- data_dow %>% mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = 'day')) %>% as.data.frame()

data_nq <- read.csv('data/nasdaq.csv', head = TRUE)
data_nq <- data_nq[which(data_nq $Adj.Close != 'null'),]
data_nq$Open <- as.numeric(data_nq$Open)
data_nq$High <- as.numeric(data_nq$High)
data_nq$Low <- as.numeric(data_nq$Low)
data_nq$Close <- as.numeric(data_nq$Close)
data_nq$Adj.Close <- as.numeric(data_nq$Adj.Close)
data_nq$Volume <- as.numeric(data_nq$Volume)
data_nq <- data_nq %>% mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = 'day')) %>%
  as.data.frame()


data_snp <- read.csv('data/snp500.csv', head = TRUE)
data_snp <- data_snp[which(data_snp$Adj.Close != 'null'),]
data_snp <- data_snp %>% mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = 'day')) %>%
  as.data.frame()


result<- jsonlite::fromJSON('data/crix.json')
data_crix <- as.data.frame(result)
data_crix = data_crix[1616:2346,]



#imputation
imputed_dax <- sapply(data_dax[,2:7], na_ma, k=4)
data_dax[,2:7] <- imputed_dax
head(data_dax)
imputed_dow <- sapply(data_dow[,2:7], na_ma, k=4)
data_dow[,2:7] <- imputed_dow

imputed_nq <- sapply(data_nq[,2:7], na_ma, k=4)
data_nq[,2:7] <- imputed_nq

imputed_snp <- sapply(data_snp[,2:7], na_ma, k=4)
data_snp[,2:7] <- imputed_snp



#computing returns
data_crix$returns = 0
for(i in 2:nrow(data_crix)) {
  data_crix$returns[i]<-(data_crix$price[i]-data_crix$price[i-1])/data_crix$price[i-1]*100
}

data_crix =data_crix %>% mutate(returns_abs=abs(returns-mean(returns)), 
                        returns_sq = (returns - mean(returns))^2,
                        returns_cube = (returns -mean(returns))^3)

returns <- function(data) {
  data$returns <- 0
  for(i in 2:nrow(data)) {
    data$returns[i]<-(data$Adj.Close[i]-data$Adj.Close[i-1])/data$Adj.Close[i-1]*100
  }
  data <- data %>% mutate(returns_abs = abs(returns-mean(returns)), 
                          returns_sq = (returns - mean(returns))^2,
                          returns_cube = (returns -mean(returns))^3)
  
  return(data)
}
data_dax <- returns(data_dax)
data_btc <- returns(data_btc)
data_nq <- returns(data_nq)
data_dow <- returns(data_dow)
data_snp <- returns(data_snp)


#saving amended datasets
write.csv(data_dax,'returns_dax.csv')
write.csv(data_btc, 'returns_btc.csv')
write.csv(data_crix, 'returns_crix.csv')
write.csv(data_dow, 'returns_dow.csv')
write.csv(data_snp, 'returns_snp500.csv')
write.csv(data_nq, 'returns_nq.csv')


jpeg('acf_dax.jpg')
acf_dax <- acf(data_dax$returns, main = 'ACF for daily DAX returns')
dev.off()

jpeg('acf_dow.jpg')
acf_dow <- acf(data_dow$returns, main = 'ACF for daily DOW returns')
dev.off()

jpeg('acf_snp.jpg')
acf_snp <- acf(data_snp$returns, main = 'ACF for daily S&P500 returns')
dev.off()

jpeg('acf_nq.jpg')
acf_nq <- acf(data_nq$returns, main = 'ACF for daily NASDAQ returns')
dev.off()

jpeg('acf_btc.jpg')
acf_btc <- acf(data_btc$returns, main = 'ACF for daily BTC returns')
dev.off()

jpeg('acf_crix.jpg')
acf_crix <- acf(data_crix$returns, main = 'ACF for daily CRIX returns')
dev.off()


jpeg('acf2_dax.jpg')
acf_dax <- acf(data_dax$returns_sq, main = 'ACF for daily DAX squared returns')
dev.off()

jpeg('acf2_dow.jpg')
acf_dow <- acf(data_dow$returns_sq, main = 'ACF for daily DOW squared returns')
dev.off()

jpeg('acf2_snp.jpg')
acf_snp <- acf(data_snp$returns_sq, main = 'ACF for daily S&P500 squared returns')
dev.off()

jpeg('acf2_nq.jpg')
acf_nq <- acf(data_nq$returns_sq, main = 'ACF for daily NASDAQ squared  returns')
dev.off()

jpeg('acf2_btc.jpg')
acf_btc <- acf(data_btc$returns_sq, main = 'ACF for daily BTC squared  returns')
dev.off()

jpeg('acf2_crix.jpg')
acf_crix <- acf(data_crix$returns_sq, main = 'ACF for daily squared CRIX returns')
dev.off()




n <- 1000

lambda <- 5

x <- rexp(n, lambda)

y = 1- exp(-lambda*x)
F <- cumsum(table(y)/sum(table(y)))

plot(x= seq(0,1, length.out = 1000),F)
F
class(F)
hist(F)


x <- runif(100)
z <- runif(100)

y <- ecdf(x)
y<-y(x)


F <- cumsum(table(y)/sum(table(y))) 
plot(F)

library(dgof)

ks.test(x,z)
