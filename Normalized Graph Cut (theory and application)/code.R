rm(list = ls())
set.seed(123)
setwd('D:/Academics/Academics/MEMS/Semester 3/MVA/Project')

#loading relevant packages
#install.packages(c('kernlab','mlbench', 'NCutYX', 'ggplot2', 'tidyverse', 'factoextra', 'Spectrum'), dependencies = TRUE)


library(kernlab)
library(kknn)
library(mlbench)
library(NCutYX)
library(ggplot2)
library(tidyverse)
library(factoextra)

##spirals

#jpeg('spiral.jpg')
#obj <- mlbench.spirals(100, 1, 0.025)
#plot(obj)
#dev.off()


##simulating concentric cirlces
d <- 2
N <- 300
R <- 4
x1 <- 1:N %>% map(.f = ~ runif(n = d - 1, min =  - R, max = R))

y1 <- x1 %>% map(.f = ~ c(., sign(runif(n = 1, min = - 1, max = 1))*sqrt(R^2 - norm(., type = '2')^2))) 

df1 <- y1 %>% reduce(.f = ~ rbind(.x, .y)) %>% 
  as.tibble %>% 
  rename(x = V1, y = V2)

sd <- 0.1
y1 <- y1 %>% map(.f =  ~ . + rnorm(n = d, mean = 0, sd = sd))
df1 <- y1 %>% reduce(.f = ~ rbind(.x, .y)) %>%
  as.tibble %>% 
  rename(x = V1, y = V2)

#ggplot() + geom_point(data=df1, mapping = aes(x = x, y = y)) 

d <- 2
N <- 300
R <- 1
x2 <- 1:N %>% map(.f = ~ runif(n = d - 1, min =  - R, max = R))

y2 <- x2 %>% map(.f = ~ c(., sign(runif(n = 1, min = - 1, max = 1))*sqrt(R^2 - norm(., type = '2')^2))) 

df2 <- y2 %>% reduce(.f = ~ rbind(.x, .y)) %>% 
  as.tibble %>% 
  rename(x = V1, y = V2)

sd <- 0.1

y2 <- y2 %>% map(.f =  ~ . + rnorm(n = d, mean = 0, sd = sd))
df2 <- y2 %>% reduce(.f = ~ rbind(.x, .y)) %>%
  as.tibble %>% 
  rename(x = V1, y = V2)


#ggplot() + geom_point(data=df1, mapping = aes(x = x, y = y),color = 'red') +
#  geom_point(data = df2, mapping = aes(x=x, y=y), color = 'blue')

N <- 300
R <- 2.5
x3 <- 1:N %>% map(.f = ~ runif(n = d - 1, min =  - R, max = R))

y3 <- x3 %>% map(.f = ~ c(., sign(runif(n = 1, min = - 1, max = 1))*sqrt(R^2 - norm(., type = '2')^2))) 

df3 <- y3 %>% reduce(.f = ~ rbind(.x, .y)) %>% 
  as.tibble %>% 
  rename(x = V1, y = V2)

sd <- 0.1

y3 <- y3 %>% map(.f =  ~ . + rnorm(n = d, mean = 0, sd = sd))
df3 <- y3 %>% reduce(.f = ~ rbind(.x, .y)) %>%
  as.tibble %>% 
  rename(x = V1, y = V2)


#jpeg('concentric circles.jpg')

#ggplot() + geom_point(data=df1, mapping = aes(x = x, y = y),color = 'red') +
#  geom_point(data = df2, mapping = aes(x=x, y=y), color = 'blue')+
#  geom_point(data=df3, mapping = aes(x=x, y=y), color = 'green')
#dev.off()



#loading data
data <- read.csv('FahadAhmed.csv', header = TRUE)
data <- scale(data, center = TRUE, scale = TRUE)
data <- data.frame(data)
summary(data)
head(data)


                                    
#ncut algorithm

## For K = 2 (2 clusters)


##using correlation as kernel
ncut <- NCutYX::ncut(data,
             K=2,
             B= 100,
             dist='correlation',
             N = 500,
             scale=FALSE,
             q=0.2,
             sigma=1)
corr_2 = data.frame('K' = 2, 'dist' = 'correlation', 'iter' = 1:100, 'ncut' = ncut$quantile)

##using euclidean distance as kernel
ncut <- NCutYX::ncut(data,
                     K=2,
                     B= 100,
                     dist='euclidean',
                     N = 500,
                     scale=FALSE,
                     q=0.2,
                     sigma=1)
euc_2 <- data.frame('K' = 3, 'dist' = 'euclidean', 'iter' = 1:100, 'ncut' = ncut$quantile)

##using gaussan function as kernel
ncut <- NCutYX::ncut(data,
                     K=2,
                     B= 100,
                     dist='gaussian',
                     N = 500,
                     scale=FALSE,
                     q=0.2,
                     sigma=1)
gauss_2 <-  data.frame('K' = 3, 'dist' = 'gaussian', 'iter' = 1:100, 'ncut' = ncut$quantile)

jpeg('p.jpg')
ggplot() + 
  geom_line(data = corr_2, aes(x = iter, y = ncut), color = 'blue') + 
  geom_line(data = euc_2, aes(x = iter, y = ncut), color = 'red') +
  geom_line(data= gauss_2, aes(x=iter, y=ncut), color = 'black') + ggtitle('K = 2') 
dev.off()


# K= 3 (3 clusters)


#the algorithm should be run multiple times in case if it does 
#not work for the first time due to convergence issues in approximation


##using correlation as kernel

ncut <- NCutYX::ncut(data,
                     K=3,
                     B= 100,
                     dist='correlation',
                     N = 500,
                     scale=FALSE,
                     q=0.2,
                     sigma=1)
corr_3 = data.frame('K' = 3, 'dist' = 'correlation', 'iter' = 1:100, 'ncut' = ncut$quantile)


##using euclidean distance as kernel

ncut <- NCutYX::ncut(data,
                     K=3,
                     B= 100,
                     dist='euclidean',
                     N = 500,
                     scale=FALSE,
                     q=0.2,
                     sigma=1)
euc_3 <- data.frame('K' = 3, 'dist' = 'euclidean', 'iter' = 1:100, 'ncut' = ncut$quantile)


##using gaussan function as kernel

ncut <- NCutYX::ncut(data,
                     K=3,
                     B= 100,
                     dist='gaussian',
                     N = 500,
                     scale=FALSE,
                     q=0.2,
                     sigma=1)
gauss_3 <-  data.frame('K' = 3, 'dist' = 'gaussian', 'iter' = 1:100, 'ncut' = ncut$quantile)


#jpeg('s.jpg')
#ggplot() + 
#  geom_line(data = corr_3, aes(x = iter, y = ncut), color = 'blue') +
#  geom_line(data = euc_3, aes(x = iter, y = ncut), color = 'red') +
#  geom_line(data= gauss_3, aes(x=iter, y=ncut), color = 'black')   + ggtitle('K = 3')
#dev.off()


#relaxing Ncut algorithm to perform normalised spectral clustering 

##first, some clustering analysis to show the advantage of 
#spectral clustering algorithms over other algorithms

#jpeg('spec.jpg')
#spec <- specc(spirals, centers = 2)
#plot(spirals, col = spec, pch = 4)
#points(spirals, col= obj$classes, pch = 5)
#dev.off()

#jpeg('kmeans.jpg')
#spec <- kmeans(spirals, centers = 2)
#plot(spirals, col = spec$cluster, pch = 4)
#dev.off()

##now normalised version

spec <- function(X, # matrix of data points
                                nn = 10, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(X, nn = 10)
  {
    D <- as.matrix( dist(X) ) # matrix of euclidean distances between data points in X
    
    # intialize the knn matrix
    knn_mat <- matrix(0,
                      nrow = nrow(X),
                      ncol = nrow(X))
    
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(X)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    
    knn_mat[ knn_mat == 2 ] = 1
    
    return(knn_mat)
  }
  
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(X) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the n_eig smallest eigenvalues
  
}

#list of clustering solutions
clusters <- list()

#collects data for total within sum of squares 
withins <- data.frame('tots.withins' = NA, 'clusters' = 1:50)

#collects data for aglorithm run time
time <- data.frame('time.elapsed' = 0, 'clusters' = 1:50)
for(i in 1:nrow(time)) {
  
  start_time <- Sys.time()
  sc <- spec(data)
  kmeans <- kmeans(sc, time$clusters[i])
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  time$time.elapsed[i] <- time_taken
  clusters[[i]] <- kmeans
  withins$tots.withins[i] = kmeans$tot.withinss
}
time <- time[2:50,]


#saving all plots into jpeg format
#jpeg('time.jpg')
#ggplot(time) + geom_line(aes(x=clusters, y = time.elapsed))
#dev.off()

#jpeg('time1.jpg')
#ggplot(time) + geom_line(aes(x=clusters, y = time.elapsed))
#dev.off()


#jpeg('4.jpg')
#fviz_cluster(clusters[[4]], data= data, ggtheme = theme_bw(), main = "")
#dev.off()

#jpeg('8.jpg')
#fviz_cluster(clusters[[8]], data= data, ggtheme = theme_bw(), main = "")
#dev.off()

#jpeg('12.jpg')
#fviz_cluster(clusters[[12]], data= data, ggtheme = theme_bw(), main = "")
#dev.off()

#jpeg('withins.jpg')
#ggplot(withins) + geom_line(aes(x=clusters, y=tots.withins))
#dev.off()

#jpeg('withins1.jpg')
#ggplot(withins) + geom_line(aes(x=clusters, y=tots.withins))
#dev.off()
