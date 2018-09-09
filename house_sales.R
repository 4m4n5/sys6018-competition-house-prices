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
house_missing <- data.frame(index = names(train), missing_count = colSums(sapply(train, is.na)))
house_missing[house_missing$missing_count>0,]
#                     index missing_count
# LotFrontage   LotFrontage           259
# Alley               Alley          1369
# MasVnrType     MasVnrType             8
# MasVnrArea     MasVnrArea             8
# BsmtQual         BsmtQual            37
# BsmtCond         BsmtCond            37
# BsmtExposure BsmtExposure            38
# BsmtFinType1 BsmtFinType1            37
# BsmtFinType2 BsmtFinType2            38
# Electrical     Electrical             1
# FireplaceQu   FireplaceQu           690
# GarageType     GarageType            81
# GarageYrBlt   GarageYrBlt            81
# GarageFinish GarageFinish            81
# GarageQual     GarageQual            81
# GarageCond     GarageCond            81
# PoolQC             PoolQC          1453
# Fence               Fence          1179
# MiscFeature   MiscFeature          1406


# fill in missing LotFrontage
table(house$LotFrontage) # shows possible outliers, will fill with median
house$LotFrontage[which(is.na(house$LotFrontage))] <- median(house$LotFrontage, na.rm=TRUE)

# fill in missing Alley
house$Alley <- as.character(house$Alley)
house$Alley[which(is.na(house$Alley))] <- 'None'
house$Alley <- as.factor(house$Alley)

# fill in missing MasVnrType
house$MasVnrType[which(is.na(house$MasVnrType))] <- 'None'

# fill in missing MasVnrArea
table(house$MasVnrArea) # no obvious outlier, will fill with mean
house$MasVnrArea[which(is.na(house$MasVnrArea))] <- mean(house$MasVnrArea, na.rm=TRUE)

# fill in missing BsmtQual
house$BsmtQual <- as.character(house$BsmtQual)
house$BsmtQual[which(is.na(house$BsmtQual))] <- 'None'
house$BsmtQual <- as.factor(house$BsmtQual)

# fill in missing BsmtCond
house$BsmtCond <- as.character(house$BsmtCond)
house$BsmtCond[which(is.na(house$BsmtCond))] <- 'None'
house$BsmtCond <- as.factor(house$BsmtCond)

# fill in missing BsmtExposure
house$BsmtExposure <- as.character(house$BsmtExposure)
house$BsmtExposure[which(is.na(house$BsmtExposure))] <- 'None'
house$BsmtExposure <- as.factor(house$BsmtExposure)

# fill in missing BsmtFinType1
house$BsmtFinType1 <- as.character(house$BsmtFinType1)
house$BsmtFinType1[which(is.na(house$BsmtFinType1))] <- 'None'
house$BsmtFinType1 <- as.factor(house$BsmtFinType1)

# fill in missing BsmtFinType2
house$BsmtFinType2 <- as.character(house$BsmtFinType2)
house$BsmtFinType2[which(is.na(house$BsmtFinType2))] <- 'None'
house$BsmtFinType2 <- as.factor(house$BsmtFinType2)

# fill in missing Electrical
house$Electrical <- as.character(house$Electrical)
house$Electrical[which(is.na(house$Electrical))] <- 'None'
house$Electrical <- as.factor(house$Electrical)

# fill in missing FireplaceQu
house$FireplaceQu <- as.character(house$FireplaceQu)
house$FireplaceQu[which(is.na(house$FireplaceQu))] <- 'None'
house$FireplaceQu <- as.factor(house$FireplaceQu)

# fill in missing GarageType
house$GarageType <- as.character(house$GarageType)
house$GarageType[which(is.na(house$GarageType))] <- 'None'
house$GarageType <- as.factor(house$GarageType)

# fill in missing GarageYrBlt
house$GarageYrBlt[which(is.na(house$GarageYrBlt))] <- 0

# fill in missing GarageFinish
house$GarageFinish <- as.character(house$GarageFinish)
house$GarageFinish[which(is.na(house$GarageFinish))] <- 'None'
house$GarageFinish <- as.factor(house$GarageFinish)

# fill in missing GarageQual
house$GarageQual <- as.character(house$GarageQual)
house$GarageQual[which(is.na(house$GarageQual))] <- 'None'
house$GarageQual <- as.factor(house$GarageQual)

# fill in missing GarageCond
house$GarageCond <- as.character(house$GarageCond)
house$GarageCond[which(is.na(house$GarageCond))] <- 'None'
house$GarageCond <- as.factor(house$GarageCond)

# fill in missing PoolQC
house$PoolQC <- as.character(house$PoolQC)
house$PoolQC[which(is.na(house$PoolQC))] <- 'None'
house$PoolQC <- as.factor(house$PoolQC)

# fill in missing Fence
house$Fence <- as.character(house$Fence)
house$Fence[which(is.na(house$Fence))] <- 'None'
house$Fence <- as.factor(house$Fence)

# fill in missing MiscFeature
house$MiscFeature <- as.character(house$MiscFeature)
house$MiscFeature[which(is.na(house$MiscFeature))] <- 'None'
house$MiscFeature <- as.factor(house$MiscFeature)




# separate back to train and test
train <- house[house$isTrain==1,]
train <- subset(train,select=-isTrain)
test <- house[house$isTrain==0,]
test <- subset(test,select=-c(isTrain,SalePrice))


# fill in missing values for test set -----------------------------------------
test_missing <- data.frame(index = names(test), missing_count = colSums(sapply(test, is.na)))
test_missing[test_missing$missing_count>0,]
#                     index missing_count
# MSZoning         MSZoning             4
# Utilities       Utilities             2
# Exterior1st   Exterior1st             1
# Exterior2nd   Exterior2nd             1
# BsmtFinSF1     BsmtFinSF1             1
# BsmtFinSF2     BsmtFinSF2             1
# BsmtUnfSF       BsmtUnfSF             1
# TotalBsmtSF   TotalBsmtSF             1
# BsmtFullBath BsmtFullBath             2
# BsmtHalfBath BsmtHalfBath             2
# KitchenQual   KitchenQual             1
# Functional     Functional             2
# GarageCars     GarageCars             1
# GarageArea     GarageArea             1
# SaleType         SaleType             1

# fill in missing values for MSZoning
table(train$MSZoning)
# C (all)      FV      RH      RL      RM 
#      10      65      16    1151     218
test$MSZoning[which(is.na(test$MSZoning))] <- 'RL'

# fill in missing values for Utilities
table(train$Utilities)
# AllPub NoSeWa 
# 1459      1 
test$Utilities[which(is.na(test$Utilities))] <- 'AllPub'

# fill in missing value for Exterior1st
table(train$Exterior1st)
# AsbShng AsphShn BrkComm BrkFace  CBlock CemntBd HdBoard ImStucc MetalSd Plywood   Stone  Stucco VinylSd Wd Sdng WdShing 
#      20       1       2      50       1      61     222       1     220     108       2      25     515     206      26
test$Exterior1st[which(is.na(test$Exterior1st))] <- 'VinylSd'

# fill in missing value for Exterior2nd
table(train$Exterior2nd)
# AsbShng AsphShn Brk Cmn BrkFace  CBlock CmentBd HdBoard ImStucc MetalSd   Other Plywood   Stone  Stucco VinylSd Wd Sdng Wd Shng 
#      20       3       7      25       1      60     207      10     214       1     142       5      26     504     197      38 
test$Exterior2nd[which(is.na(test$Exterior2nd))] <- 'VinylSd'

# fill in missing value for BsmtFinSF1
table(train$BsmtFinSF1)
test$BsmtFinSF1[which(is.na(test$BsmtFinSF1))] <- mean(train$BsmtFinSF1)

# fill in missing value for BsmtFinSF2
table(train$BsmtFinSF2)
test$BsmtFinSF2[which(is.na(test$BsmtFinSF2))] <- mean(train$BsmtFinSF2)

# fill in missing value for BsmtUnfSF
table(train$BsmtUnfSF)
test$BsmtUnfSF[which(is.na(test$BsmtUnfSF))] <- mean(train$BsmtUnfSF)

# fill in missing value for TotalBsmtSF
table(train$TotalBsmtSF)
test$TotalBsmtSF[which(is.na(test$TotalBsmtSF))] <- mean(train$TotalBsmtSF)

# fill in missing value for BsmtFullBath
table(train$BsmtFullBath)
#   0   1   2   3 
# 856 588  15   1
test$BsmtFullBath[which(is.na(test$BsmtFullBath))] <- 0

# fill in missing value for BsmtHalfBath
table(train$BsmtHalfBath)
#    0    1    2 
# 1378   80    2
test$BsmtHalfBath[which(is.na(test$BsmtHalfBath))] <- 0

# fill in missing value for KitchenQual
table(train$KitchenQual)
#  Ex  Fa  Gd  TA 
# 100  39 586 735
test$KitchenQual[which(is.na(test$KitchenQual))] <- 'TA'

# fill in missing value for Functional
table(train$Functional)
# Maj1 Maj2 Min1 Min2  Mod  Sev  Typ 
#   14    5   31   34   15    1 1360 
test$Functional[which(is.na(test$Functional))] <- 'Typ'

# fill in missing value for GarageCars
table(train$GarageCars)
#  0   1   2   3   4 
# 81 369 824 181   5
test$GarageCars[which(is.na(test$GarageCars))] <- 2

# fill in missing value for GarageArea
table(train$GarageArea)
test$GarageArea[which(is.na(test$GarageArea))] <- mean(train$GarageArea)

# fill in missing value for SaleType
table(train$SaleType)
# COD   Con ConLD ConLI ConLw   CWD   New   Oth    WD 
#  43     2     9     5     5     4   122     3  1267
test$SaleType[which(is.na(test$SaleType))] <- 'WD'




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
# [1] 60734.49

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
# [1] 23621.77

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


