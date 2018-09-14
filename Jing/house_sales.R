# Kaggle: House price
# Jing Sun, Aman, David

library(caret)
library(dplyr)
library(ISLR)
library(purrr)
library(ggplot2)
library(randomForest)
library(gridExtra)

train <- read.csv('train.csv')
test <- read.csv('test.csv')

test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
house <- rbind(train, test)


# fill in missing values for house (train+test) ---------------------------
house_missing <- data.frame(index = names(house), missing_count = colSums(sapply(house, is.na)))
house_missing$index[which(house_missing$missing_count>0)]
#  [1] MSZoning     LotFrontage  Alley        Utilities    Exterior1st  Exterior2nd  MasVnrType   MasVnrArea  
#  [9] BsmtQual     BsmtCond     BsmtExposure BsmtFinType1 BsmtFinSF1   BsmtFinType2 BsmtFinSF2   BsmtUnfSF   
# [17] TotalBsmtSF  Electrical   BsmtFullBath BsmtHalfBath KitchenQual  Functional   FireplaceQu  GarageType  
# [25] GarageYrBlt  GarageFinish GarageCars   GarageArea   GarageQual   GarageCond   PoolQC       Fence       
# [33] MiscFeature  SaleType     SalePrice   


# fill in missing LotFrontage and GarageArea
house$LotFrontage[which(is.na(house$LotFrontage))] <- mean(house$LotFrontage, na.rm=TRUE)
house$GarageArea[which(is.na(house$GarageArea))] <- mean(house$GarageArea, na.rm=TRUE)

house[,c(7,26,31,32,33,34,36,43,58,59,61,64,65,73:75)] <- 
  lapply(house[,c(7,26,31,32,33,34,36,43,58,59,61,64,65,73:75)], as.character)
house[,c(7,26,31,32,33,34,36,43,58,59,61,64,65,73:75)][is.na(house[,c(7,26,31,32,33,34,36,43,58,59,61,64,65,73:75)])] <- 'None'
house[,c(7,26,31,32,33,34,36,43,58,59,61,64,65,73:75)] <-
  lapply(house[,c(7,26,31,32,33,34,36,43,58,59,61,64,65,73:75)], as.factor)

# fill in missing MasVnrArea,GarageYrBlt,BsmtFinSF1,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,BsmtFullBath,BsmtHalfBath
house[,c(27,35,37:39,48,49,60)][is.na(house[,c(27,35,37:39,48,49,60)])] <- 0


# function for get mode https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# missing values for MSZoning,Utilities,Exterior1st,Exterior2nd,KitchenQual,Functional,GarageCars,SaleType
modeList <- c(3,10,24,25,54,56,62,79)
for (i in 1:length(modeList)) {
  house[is.na(house[,modeList[i]]),modeList[i]] <- getmode(house[,modeList[i]])
}



# separate back to train and test
train <- house[house$isTrain==1,]
train <- subset(train,select=-isTrain)
test <- house[house$isTrain==0,]
test <- subset(test,select=-c(isTrain,SalePrice))

cor_data <- train %>% select_if(negate(is.factor))
corr <- as.data.frame(round(cor(cor_data),2))
corr_df <- data.frame(var = colnames(cor_data), cor = corr$SalePrice)
corr_df[order(-corr_df$cor),][2:6,]
#            var  cor
# 5  OverallQual 0.79
# 17   GrLivArea 0.71
# 27  GarageCars 0.64
# 28  GarageArea 0.62
# 13 TotalBsmtSF 0.61

p1 <- ggplot(train,aes(OverallQual,SalePrice))+geom_point()
p2 <- ggplot(train,aes(GrLivArea,SalePrice))+geom_point()
p3 <- ggplot(train,aes(GarageCars,SalePrice))+geom_point()
p4 <- ggplot(train,aes(GarageArea,SalePrice))+geom_point()
p5 <- ggplot(train,aes(TotalBsmtSF,SalePrice))+geom_point()

grid.arrange(p1,p2,p3,p4,p5)

train <- train[train$GrLivArea<=4500,]
train <- train[train$TotalBsmtSF<4000,]


# Sampling for cross validation -------------------------------------------
set.seed(222)
ind <- sample(1:nrow(train), size=0.8*nrow(train))
training_cv <- subset(train[ind,],select=-Id)
validation <- subset(train[-ind,],select=-Id)


# Linear regression -------------------------------------------------------
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
lm1 = train(SalePrice ~ ., data = training_cv,
            preProcess = c("center", "scale"),
            method = "lm", trControl = trctrl)
summary(lm1)
# Residual standard error: 23060 on 925 degrees of freedom
# Multiple R-squared:  0.9355,	Adjusted R-squared:  0.9186 
# F-statistic:  55.4 on 242 and 925 DF,  p-value: < 2.2e-16

lm2 = train(SalePrice ~ .-MSSubClass-Alley-LotShape-Utilities-Heating-HeatingQC
                         -Electrical-LowQualFinSF-BsmtFullBath-BsmtHalfBath-MiscVal, 
            data=training_cv,
            preProcess = c("center","scale"),
            method = "lm", trControl = trctrl)
summary(lm2)
# Residual standard error: 22880 on 946 degrees of freedom
# Multiple R-squared:  0.935,	Adjusted R-squared:  0.9198 
# F-statistic:  61.6 on 221 and 946 DF,  p-value: < 2.2e-16

pred1 <- predict(lm2, newdata = validation)
rmse1 <- sqrt(mean((pred1-validation$SalePrice)^2))
rmse1
# [1] 60754.57

# submission 1
model1 <- train(SalePrice ~ .-Id-MSSubClass-Alley-LotShape-Utilities-Heating-HeatingQC
                -Electrical-LowQualFinSF-BsmtFullBath-BsmtHalfBath-MiscVal, 
                data=train,
                preProcess = c("center","scale"),
                method = "lm", trControl = trctrl)
summary(model1)

prediction1 <- predict(model1, newdata=test)
result1 <- data.frame(Id = test$Id, SalePrice = prediction1)
#write.csv(result1, "js6mj_saleprice_sub1.csv", row.names=FALSE)



# Random Forest -----------------------------------------------------------
rf_model1 <- randomForest(SalePrice~., data=training_cv)
importance(rf_model1)
varImpPlot(rf_model1)

rf_pred <- predict(rf_model1, newdata = validation)
rf_rmse <- sqrt(mean((rf_pred-validation$SalePrice)^2))
rf_rmse
# [1] 23681.36

#model2 <- randomForest(SalePrice~., data=train)
#prediction2 <- predict(model2, newdata=test)
#result2 <- data.frame(Id = test$Id, SalePrice = prediction2)
#write.csv(result2, "js6mj_saleprice_sub2.csv", row.names=FALSE)



imp1 <- data.frame(index = colnames(training_cv[,-80]), rf_model1$importance)
imp1[order(imp1$IncNodePurity),]


rf_model2 <- randomForest(SalePrice~.-BsmtFinType1-WoodDeckSF-OpenPorchSF-GarageFinish-BsmtUnfSF
                                     -Fireplaces-LotFrontage-GarageType-GarageYrBlt-MasVnrArea
                                     -FireplaceQu-Exterior1st-YearRemodAdd-Exterior2nd, data=training_cv)
rf_pred2 <- predict(rf_model2, newdata = validation)
rf_rmse2 <- sqrt(mean((rf_pred2-validation$SalePrice)^2))
rf_rmse2
# [1] 23887.42

model2 <- randomForest(SalePrice~OverallQual+Neighborhood+GrLivArea+GarageCars+ExterQual+
                                 TotalBsmtSF+X1stFlrSF+GarageArea+BsmtFinSF1+X2ndFlrSF+
                                 BsmtQual+KitchenQual+YearBuilt+FullBath+LotArea+TotRmsAbvGrd+
                                 Exterior2nd+YearRemodAdd+Exterior1st, data=train)
prediction2 <- predict(model2, newdata=test)
result2 <- data.frame(Id = test$Id, SalePrice = prediction2)
write.csv(result2, "zz_rf.csv", row.names=FALSE) # 0.14456





# knn ---------------------------------------------------------------------
## functions

euclidean_dist <- function(x,y) {
  d = 0
  for (i in 1:length(x)) {
    d = d + (x[[i]] - y[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

manhattan_dist <- function(x,y) {
  d = 0
  for (i in 1:length(x)) {
    d = d + abs(x[[i]] - y[[i]])^2
  }
  return(d)
}



knn_predict <- function(training, test, k) {
  pred <- c()
  for (i in 1:nrow(test)) {
    dist <- c()
    for (j in 1:nrow(training)) {
      dist <- c(dist, euclidean_dist(test[i,], training[j,]))
      #dist <- c(dist, sqrt(sum((test[i,] - training[j,1:ncol(training)-1])^2)))
    }
    dist_df <- data.frame(SalePrice = training$SalePrice, dist)
    dist_df <- dist_df[order(dist_df$dist),]
    dist_df <- dist_df[1:k,]
    
    pred <- c(pred, sum(dist_df$SalePrice)/k)
  }
  return(pred)
}



knn_predict_weighted <- function(training, test, k) {
  pred <- c()
  for (i in 1:nrow(test)) {
    dist <- c()
    for (j in 1:nrow(training)) {
      dist <- c(dist, euclidean_dist(test[i,], training[j,]))
    }
    dist_df <- data.frame(SalePrice = training$SalePrice, dist)
    dist_df <- dist_df[order(dist_df$dist),]
    dist_df <- dist_df[1:k,]
    
    num = 0
    denom = 0
    
    for (p in 1:nrow(dist_df)) {
      if (dist_df$dist[p] != 0) {
        num = num + dist_df$SalePrice[p] / dist_df$dist[p]
        denom = denom + 1/dist_df$dist[p]
      }
    }
    pred <- c(pred, num/denom)
  }
  return(pred)
}




normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}


knn_house <- house
knn_house <- select(knn_house, c(Id,OverallQual,Neighborhood,GrLivArea,GarageCars,ExterQual,TotalBsmtSF,
                                 X1stFlrSF,GarageArea,BsmtFinSF1,X2ndFlrSF,BsmtQual,KitchenQual,
                                 YearBuilt,FullBath,LotArea,isTrain,SalePrice))

knn_house_nonfactor <- knn_house %>%  select_if(negate(is.factor))
knn_house_factor <- data.frame(Id=knn_house$Id,knn_house %>% select_if(is.factor))

knn_house_nonfactor[,2:12] <- as.data.frame(lapply(knn_house_nonfactor[,2:12], normalize))


for (i in 1:length(colnames(knn_house_factor))) {
  if (is.factor(knn_house_factor[,i])) {
    levels <- unique(knn_house_factor[,i])
    for (j in 1:length(levels)) {
      knn_house_factor[paste(colnames(knn_house_factor)[i],levels[j],sep='')] <- 
        ifelse(knn_house_factor[,i]==levels[j],1,0)
    }
  }
}

knn_house_factor <- knn_house_factor[,c(1,6:43)]
knn_final <- merge(knn_house_factor,knn_house_nonfactor,by='Id')
#write.csv(knn_final,'knn_house.csv')


knn_train <- knn_final[knn_final$isTrain==1,]
knn_train <- subset(knn_train,select=-c(Id,isTrain))
knn_test <- knn_final[knn_final$isTrain==0,]
knn_test <- subset(knn_test,select=-c(Id,isTrain,SalePrice))

knn_ind <- sample(1:nrow(knn_train), size=0.8*nrow(knn_train))
knn_training_cv <- knn_train[knn_ind,]
knn_validation <- knn_train[-knn_ind,]





knn_pred_w2 <- knn_predict_weighted(knn_train, knn_test, 7)
knn_pred_w3 <- knn_predict_weighted(knn_train, knn_test, 8)
knn_pred_w4 <- knn_predict_weighted(knn_train, knn_test, 9)
knn_pred_w5 <- knn_predict_weighted(knn_train, knn_test, 10)



knn_result_w4 <- data.frame(Id = test$Id, SalePrice = knn_pred_w4)
write.csv(knn_result_w4, "knn_w9.csv", row.names=FALSE)

knn_result_w5 <- data.frame(Id = test$Id, SalePrice = knn_pred_w5)
write.csv(knn_result_w5, "knn_w10.csv", row.names=FALSE)
