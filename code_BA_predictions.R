
library(dplyr)
library(tidyverse)
library(mgcv)
library(extRemes)
library(gbm)
library(VGAM)
library(caret)
library(naivebayes)
library(POT)
library(doParallel)

# loading the dataset
load("~/Desktop/Project/data_train.RData")

# combining covariates clim1 and clim2 => wind speed
data_train_DF<- data_train_DF %>%
  mutate(wind = sqrt(clim1^2 + clim2^2)) %>%
  subset(select = -c(clim1, clim2) )

# scaling continuous climate and land cover covariates
data_scaled <- as.data.frame(scale(data_train_DF[,c(8:36)]))
data_scaled <- cbind(data_train_DF[,c(1:7)], data_scaled)


# splitting the dataset into training set & test set
# Note: training set is formed with rows with no NA values, test set with rows where BA = NA
train <- filter(data_scaled, !is.na(BA) & !is.na(CNT))
test <- filter(data_scaled, is.na(BA))

# adding mean monthly CNT as a variable for every voxel
train <- train %>%
  group_by(lon, lat, month)%>%
  mutate(mean_monthly_CNT= mean(CNT))

for (m in 3:9){
  for (L in seq(-124.75, -66.75, 0.5)){
    for (l in seq(25.25, 49.25, 0.5)){
      test$mean_monthly_CNT[test$lon==L & test$lat==l & test$month==m] <- 
        train$mean_monthly_CNT[train$lon==L & train$lat==l & train$month==m][1]
    }
  }
}

# splitting the test set depending on whether CNT is available or not
test1 <- filter(test,  is.na(CNT))
test2 <- filter(test,  !is.na(CNT))

rows1 <- which(is.na(test$CNT))
rows2 <- which(!is.na(test$CNT))

#setting the threshold for extremes as 2000
threshold <- 1500

rm(L, l, m)


# function to obtain probability matrix from gbm (log-normal distribution) model
predict_ba_prob_gaussian_gbm1 = function(model, test){
  prediction = predict(model, test, type="response")
  sd = sd(prediction)
  ba_probabilities = matrix(0, nrow(test), length(u_ba))
  for (i in 1:nrow(test)){
    mu = prediction[i]
    ba_probabilities[i,] = pnorm(log(u_ba+1), mean = mu, sd = sd)
  }
  ba_probabilities
}


# gbm models to obtain probabilities Prob(BA < u)
#without CNT
m_gbm1 <- gbm(log(BA+1) ~ .-CNT, data = train, distribution = "gaussian", interaction.depth = 6, n.trees=5000, n.minobsinnode = 10, shrinkage = 0.1)
p_gbm1 <- predict_ba_prob_gaussian_gbm1(m_gbm1, test1)

# with CNT
m_gbm2 <- gbm(log(BA+1) ~., data = train, distribution = "gaussian", interaction.depth = 6, n.trees=5000, n.minobsinnode = 10, shrinkage = 0.1)
p_gbm2 <- predict_ba_prob_gaussian_gbm1(m_gbm2, test2)



# With extremes
# below <- train %>% filter(BA <= threshold)
# m_gbm_nocnt <- gbm(log(BA+1) ~ .-CNT-overthreshold, data = below, distribution = "gamma", interaction.depth = 6, n.trees=1000, n.minobsinnode = 10, shrinkage = 0.1)
# p_gbm_nocnt <- predict_ba_prob_gaussian_gbm1(m_gbm_nocnt, test)
# 
# m_gbm_cnt <- gbm(log(BA+1) ~.-overthreshold, data = below, distribution = "gamma", interaction.depth = 6, n.trees=1000, n.minobsinnode = 10, shrinkage = 0.1)
# m_gbm_cnt <- predict_ba_prob_gaussian_gbm1(m_gbm_cnt, test)

#naive bayes

# naive bayes for binary classification of Prob(BA > threshold)
train$overthreshold <- as.factor(ifelse(train$BA > threshold, 1, 0))

naivebayes_threshold <- naive_bayes(overthreshold ~.-BA-CNT, data = train, usekernel = T) 
p_naivebayes1 <- predict(naivebayes_threshold, test1, 'prob')
p_naivebayes2 <- predict(naivebayes_threshold, test2, 'prob')

# test1$overthreshold <- p_naivebayes1[,2]
# test2$overthreshold <- p_naivebayes2[,2]
# test1$overthreshold <- NULL
# test2$overthreshold <- NULL

p_naivebayes <- predict(naivebayes_threshold, test, 'prob')
#test$overthreshold <- p_naivebayes[,2]
# test$overthreshold <- NULL
# train$overthreshold<- NULL


# vglm modeling of extremes with generalized pareto distribution
# without count
train1 <- train
train1$CNT<-NULL
modelvglm1 <-  vglm(BA~.-mean_monthly_CNT, family = gpd(threshold = threshold), link = "log", data = train1, subset = BA > threshold)

# with count
modelvglm2 <-  vglm(BA~.-mean_monthly_CNT, family = gpd(threshold = threshold), link = "log", data = train, subset = BA > threshold)


# subsetting rows for extremes
threshold_rows1 <- which(p_naivebayes1[,2]> 0.05)
threshold_rows2 <- which(p_naivebayes2[,2]> 0.05)
new1 <- test1[threshold_rows1,]
new2 <- test2[threshold_rows2,]

# predicting parameters of gpd models for extremes
predictedvglm1 <- predict(modelvglm1, untransform = TRUE, newdata = new1) #predict with gpd the selected rows
predictedvglm2 <- predict(modelvglm2, untransform = TRUE, newdata = new2)


# calculating conditional probabilities P(BA <  u_ba_threshold | BA > threshold )
x = matrix(0, nrow(new1), length(u_ba_threshold))
for (i in 1:nrow(predictedvglm1)){
  for (j in 1:length(u_ba_threshold))
    x[i,j] <- pgpd(u_ba_threshold[j], scale = predictedvglm1[i,1] , shape = predictedvglm1[i,2]) 
}

y = matrix(0, nrow(new2), length(u_ba_threshold))
for (i in 1:nrow(predictedvglm2)){
  for (j in 1:length(u_ba_threshold))
    y[i,j] <- pgpd(u_ba_threshold[j], scale = predictedvglm2[i,1] , shape = predictedvglm2[i,2]) 
}

# calculating probability P(BA <  u_ba_threshold | BA > threshold }
prob_threshold1 = matrix(0, nrow(x), length(u_ba_threshold))
for (i in 1:nrow(x)){
  prob_threshold1[i,] <- x[i,] * p_naivebayes1[i,2]
}

prob_threshold2 = matrix(0, nrow(y), length(u_ba_threshold))
for (i in 1:nrow(y)){
  prob_threshold2[i,] <- y[i,] * p_naivebayes2[i,2]
}

# replacing rows of extreme cases for u_ba > threshold
u_ba_threshold <- subset(u_ba, u_ba > threshold)
l<-length(u_ba_threshold)

# without CNT
a <- p_gbm1
a[threshold_rows1, (29-l):28] <- a[threshold_rows1, 28-l] +prob_threshold1

p1 <- a
q1 <- a[threshold_rows1,28] -1
a[threshold_rows1,] <- p1[threshold_rows1, ]- q1

for (i in 1:nrow(test1)){
  for(j in 1:28){
    if (a[i,j] < 0) {
      a[i,j] <- 0
    }
  }
}

# with CNT
b <- p_gbm2
b[threshold_rows2, (29-l):28] <- b[threshold_rows2, 28-l] +prob_threshold2

p2 <- b
q2 <- b[threshold_rows2,28] -1
b[threshold_rows2,] <- p1[threshold_rows2, ]- q2

for (i in 1:nrow(test2)){
  for(j in 1:28){
    if (b[i,j] < 0) {
      b[i,j] <- 0
    }
  }
}


# BA probability matrix
matrix = matrix(0, nrow = 80000, ncol = 28)

# rows1 : rows without CNT; rows2 : rows with CNT
matrix[rows1,] <- a
matrix[rows2,] <- b

# If a probability > 1, then make probability == 1
for (i in 1:nrow(matrix)){
  for (j in 1:ncol(matrix)){
    if (matrix[i,j]>1)
      matrix[i,j]=1
  }
}


# If CNT == 0, then BA == 0
for (i in 1:nrow(test)){
  if (test$CNT[i] == 0 & !is.na(test$CNT[i])){
    for (j in 1:ncol(matrix))
      matrix[i,j] = 1
  }
}


# If CNT > 0, then P(BA = 0) = 0
for (i in 1:nrow(test)){
  if (test$CNT[i] > 0 & !is.na(test$CNT[i]))
      matrix[i,1] = 0
}


prediction_ba<- matrix

save(prediction_ba, file = "/Users/jiwonmin/Desktop/Project/predictions.RData")


