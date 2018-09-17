library(tidyverse)

# read in data
test <- read.csv("test.csv")
train <- read.csv("train.csv")

# the documentation (http://ww2.amstat.org/publications/jse/v19n3/Decock/DataDocumentation.txt) suggests
# that there are 3-5 outliers, which will be indicated by a plot of SalePrice versus GrLivArea, that
# are recommended to be removed
plot(train$GrLivArea,train$SalePrice)
# remove observations with at least 4000 square feet from the data set
train <- train[train$GrLivArea<4000,]

plot(train$GrLivArea,train$SalePrice)
# 4 observations removed, no apparent outliers


### transform SalePrice variable?

# combine test and training sets for feature engineering 
test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
combdata <- rbind(train, test)

# Filling in missing values ----------------------------------------------------
# function for get mode https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# count na values in each column of the combined data frame
colSums(is.na(combdata))

# Fill in four missing values in MSZoning column with mode
combdata$MSZoning[is.na(combdata$MSZoning)] <- getmode(combdata$MSZoning)

# Fill in LotFrontage NAs with the median value for the different lot configurations
# Get a list of the median LotFrontages grouped by lot configuration  
lot_config_medians <- combdata %>%
  group_by(LotConfig) %>%
  summarise(median(LotFrontage,na.rm = T))

# LotConfig `median(LotFrontage, na.rm = T)`
# <fct>                                <dbl>
# 1 Corner                                  80
# 2 CulDSac                                 50
# 3 FR2                                     62
# 4 FR3                                     66
# 5 Inside                                  66

# Fill in missing LotFrontages with median for group of LotConfig
combdata$LotFrontage[is.na(combdata$LotFrontage) & combdata$LotConfig == "Corner"] <- 80
combdata$LotFrontage[is.na(combdata$LotFrontage) & combdata$LotConfig == "CulDSac"] <- 50
combdata$LotFrontage[is.na(combdata$LotFrontage) & combdata$LotConfig == "FR2"] <- 62
combdata$LotFrontage[is.na(combdata$LotFrontage) & combdata$LotConfig == "FR3"] <- 66
combdata$LotFrontage[is.na(combdata$LotFrontage) & combdata$LotConfig == "Inside"] <- 66
  
# Fill in NAs for Alley variable with 'None', as the documentation states NA means no alley access
combdata$Alley <- as.character(combdata$Alley)
combdata$Alley[is.na(combdata$Alley)] <- "None"
combdata$Alley <- as.factor(combdata$Alley)

# Fill in NAs for Utilities with mode
combdata$Utilities[is.na(combdata$Utilities)] <- getmode(combdata$Utilities)

# Fill in NA for Exterior1st with mode
combdata$Exterior1st[is.na(combdata$Exterior1st)] <- getmode(combdata$Exterior1st)

# Fill in NA for Exterior2nd with mode
combdata$Exterior2nd[is.na(combdata$Exterior2nd)] <- getmode(combdata$Exterior2nd)

# Fill in NAs for MasVnrType
# Could fill in with mode, but we assume NA means None 
combdata$MasVnrType[is.na(combdata$MasVnrType)] <- "None"
# Thus, NAs for MasVnrArea will be 0
combdata$MasVnrArea[is.na(combdata$MasVnrArea)] <- 0

# For BsmtExposure, BsmtFinType1, BsmtFinType2, BsmtQual, BsmtCond... the documentation states that 
# NA means no basement, so we replace NAs with None
combdata$BsmtExposure <- as.character(combdata$BsmtExposure)
combdata$BsmtFinType1 <- as.character(combdata$BsmtFinType1)
combdata$BsmtFinType2 <- as.character(combdata$BsmtFinType2)
combdata$BsmtQual <- as.character(combdata$BsmtQual)
combdata$BsmtCond <- as.character(combdata$BsmtCond)
combdata$BsmtExposure[is.na(combdata$BsmtExposure)] <- "None"
combdata$BsmtFinType1[is.na(combdata$BsmtFinType1)] <- "None"
combdata$BsmtFinType2[is.na(combdata$BsmtFinType2)] <- "None"
combdata$BsmtQual[is.na(combdata$BsmtQual)] <- "None"
combdata$BsmtCond[is.na(combdata$BsmtCond)] <- "None"
combdata$BsmtExposure <- as.factor(combdata$BsmtExposure)
combdata$BsmtFinType1 <- as.factor(combdata$BsmtFinType1)
combdata$BsmtFinType2 <- as.factor(combdata$BsmtFinType2)
combdata$BsmtQual <- as.factor(combdata$BsmtQual)
combdata$BsmtCond <- as.factor(combdata$BsmtCond)

# For BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, BsmtHalfBath, and BsmtFullBath... each missing one value, 
# we assume that the missing value suggests no basement, and thus can be made 0 
combdata$BsmtFinSF1[is.na(combdata$BsmtFinSF1)] <- 0
combdata$BsmtFinSF2[is.na(combdata$BsmtFinSF2)] <- 0
combdata$BsmtUnfSF[is.na(combdata$BsmtUnfSF)] <- 0
combdata$TotalBsmtSF[is.na(combdata$TotalBsmtSF)] <- 0
combdata$BsmtHalfBath[is.na(combdata$BsmtHalfBath)] <- 0
combdata$BsmtFullBath[is.na(combdata$BsmtFullBath)] <- 0

# Fill in NA for Electrical with mode
combdata$Electrical[is.na(combdata$Electrical)] <- getmode(combdata$Electrical)

# Fill in NA for KitchenQual with mode
combdata$KitchenQual[is.na(combdata$KitchenQual)] <- getmode(combdata$KitchenQual)

# Fill in NAs for Functional with Typ, as per assumption from the data description txt file 
combdata$Functional[is.na(combdata$Functional)] <- "Typ"

# Fill in NAs for FireplaceQu with None, as documentation suggests NA means no fireplace
combdata$FireplaceQu <- as.character(combdata$FireplaceQu)
combdata$FireplaceQu[is.na(combdata$FireplaceQu)] <- "None"
combdata$FireplaceQu <- as.factor(combdata$FireplaceQu)

# Fill in NAs for GarageType, GarageFinish, GarageQual and GarageCond with None, as the documentation states
# that NA means No Garage
combdata$GarageType <- as.character(combdata$GarageType)
combdata$GarageFinish <- as.character(combdata$GarageFinish)
combdata$GarageQual <- as.character(combdata$GarageQual)
combdata$GarageCond <- as.character(combdata$GarageCond)
combdata$GarageType[is.na(combdata$GarageType)] <- "None"
combdata$GarageFinish[is.na(combdata$GarageFinish)] <- "None"
combdata$GarageQual[is.na(combdata$GarageQual)] <- "None"
combdata$GarageCond[is.na(combdata$GarageCond)] <- "None"
combdata$GarageType <- as.factor(combdata$GarageType)
combdata$GarageFinish <- as.factor(combdata$GarageFinish)
combdata$GarageQual <- as.factor(combdata$GarageQual)
combdata$GarageCond <- as.factor(combdata$GarageCond)

# Fill in NAs for GarageYrBlt, GarageCars, GarageArea with 0, as NA signifies no garage
combdata$GarageYrBlt[is.na(combdata$GarageYrBlt)] <- 0
combdata$GarageCars[is.na(combdata$GarageCars)] <- 0
combdata$GarageArea[is.na(combdata$GarageArea)] <- 0

# Fill in NAs for PoolQC with None, as documentation suggests that NA means no pool
combdata$PoolQC <- as.character(combdata$PoolQC)
combdata$PoolQC[is.na(combdata$PoolQC)] <- "None"
combdata$PoolQC <- as.factor(combdata$PoolQC)

# Fill in NAs for Fence with None, as documentation suggests that NA means no fence
combdata$Fence <- as.character(combdata$Fence)
combdata$Fence[is.na(combdata$Fence)] <- "None"
combdata$Fence <- as.factor(combdata$Fence)

# Fill in NAs for MiscFeature with None, as documentation suggests that NA means no miscellaneous features
combdata$MiscFeature <- as.character(combdata$MiscFeature)
combdata$MiscFeature[is.na(combdata$MiscFeature)] <- "None"
combdata$MiscFeature <- as.factor(combdata$MiscFeature)

# Fill in NAs for SaleType with mode
combdata$SaleType[is.na(combdata$SaleType)] <- getmode(combdata$SaleType)

# check to make sure there are no more columns with NAs
colSums(is.na(combdata))

# Converting numerical variables to factors when necessary ----------------------------------------------------
combdata$MSSubClass <- as.factor(combdata$MSSubClass)
combdata$OverallQual <- as.factor(combdata$OverallQual)
combdata$OverallCond <- as.factor(combdata$OverallCond)
combdata$YearBuilt <- as.factor(combdata$YearBuilt)
combdata$YearRemodAdd <- as.factor(combdata$YearRemodAdd)
combdata$GarageYrBlt <- as.factor(combdata$GarageYrBlt)
combdata$MoSold <- as.factor(combdata$MoSold)
combdata$YrSold <- as.factor(combdata$YrSold)

# Seperating back to training and testing set ----------------------------------------------------
train <- combdata[combdata$isTrain==1,]
train <- subset(train,select=-isTrain)
test <- combdata[combdata$isTrain==0,]
test <- subset(test,select=-c(isTrain,SalePrice))

# Jing's KNN ----------------------------------------------------
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
    d = d + abs(x[[i]] - y[[i]])
  }
  return(d)
}


chisq_dist <- function(x,y) {
  d = 0
  for (i in 1:length(x)) {
    d = d + ((x[[i]] - y[[i]])^2 / (x[[i]] + y[[i]]))
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
      dist <- c(dist, manhattan_dist(test[i,], training[j,]))
      #dist <- c(dist, chisq_dist(test[i,], training[j,]))
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

knn_house <- subset(combdata, !(combdata$Id %in% c(524,1299)))

knn_house <- select(knn_house, c(Id,OverallQual,GrLivArea,GarageCars,GarageArea,TotalBsmtSF,
                                 X1stFlrSF,BsmtFinSF1,X2ndFlrSF,YearBuilt,isTrain,SalePrice)) # 0.16343


knn_house_nonfactor <- knn_house %>%  select_if(negate(is.factor))
knn_house_factor <- data.frame(Id=knn_house$Id,knn_house %>% select_if(is.factor))

knn_house_nonfactor[,2:(ncol(knn_house_nonfactor)-2)] <- 
  as.data.frame(lapply(knn_house_nonfactor[,2:(ncol(knn_house_nonfactor)-2)], normalize))

knn_final <- knn_house_nonfactor
#write.csv(knn_final,'knn_house.csv')

knn_train <- knn_final[knn_final$isTrain==1,]
knn_train <- subset(knn_train,select=-c(Id,isTrain))
knn_test <- knn_final[knn_final$isTrain==0,]
knn_test <- subset(knn_test,select=-c(Id,isTrain,SalePrice))

knn_pred <- knn_predict_weighted(knn_train, knn_test, 8)

knn_result <- data.frame(Id = test$Id, SalePrice = knn_pred)
write.csv(knn_result, "knn_k8_p9_w_manhattan.csv", row.names=FALSE)


