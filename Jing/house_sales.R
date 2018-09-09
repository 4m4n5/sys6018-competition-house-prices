# Kaggle: House price
# Jing Sun, Aman, David

library(caret)
library(dplyr)
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
library(randomForest)
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


rf_model2 <- randomForest(SalePrice~.-Exterior2nd-MasVnrArea-Exterior1st-YearRemodAdd
                                     -FireplaceQu-GarageType-LotFrontage-GarageYrBlt
                                     -GarageFinish-Fireplaces-BsmtUnfSF-WoodDeckSF
                                     -OpenPorchSF-BsmtFinType1, data=training_cv)
rf_pred2 <- predict(rf_model2, newdata = validation)
rf_rmse2 <- sqrt(mean((rf_pred2-validation$SalePrice)^2))
rf_rmse2

#model2 <- randomForest(SalePrice~., data=train)
#prediction2 <- predict(model2, newdata=test)
#result2 <- data.frame(Id = test$Id, SalePrice = prediction2)
#write.csv(result2, "js6mj_saleprice_sub2.csv", row.names=FALSE)


