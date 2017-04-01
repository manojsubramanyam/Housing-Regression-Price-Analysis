rm(list = ls())
# require(data.table)

train <- read.csv('train.csv', stringsAsFactors = T)

# check data
dim(train)
str(train)

names(train)
name.type = function(x){
  print( names(x), class(x))
}
sapply(train, class)
# [1] "Id"            "MSSubClass"    "MSZoning"      "LotFrontage"   "LotArea"       "Street"        "Alley"        
# [8] "LotShape"      "LandContour"   "Utilities"     "LotConfig"     "LandSlope"     "Neighborhood"  "Condition1"   
# [15] "Condition2"    "BldgType"      "HouseStyle"    "OverallQual"   "OverallCond"   "YearBuilt"     "YearRemodAdd" 
# [22] "RoofStyle"     "RoofMatl"      "Exterior1st"   "Exterior2nd"   "MasVnrType"    "MasVnrArea"    "ExterQual"    
# [29] "ExterCond"     "Foundation"    "BsmtQual"      "BsmtCond"      "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"   
# [36] "BsmtFinType2"  "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "Heating"       "HeatingQC"     "CentralAir"   
# [43] "Electrical"    "1stFlrSF"      "2ndFlrSF"      "LowQualFinSF"  "GrLivArea"     "BsmtFullBath"  "BsmtHalfBath" 
# [50] "FullBath"      "HalfBath"      "BedroomAbvGr"  "KitchenAbvGr"  "KitchenQual"   "TotRmsAbvGrd"  "Functional"   
# [57] "Fireplaces"    "FireplaceQu"   "GarageType"    "GarageYrBlt"   "GarageFinish"  "GarageCars"    "GarageArea"   
# [64] "GarageQual"    "GarageCond"    "PavedDrive"    "WoodDeckSF"    "OpenPorchSF"   "EnclosedPorch" "3SsnPorch"    
# [71] "ScreenPorch"   "PoolArea"      "PoolQC"        "Fence"         "MiscFeature"   "MiscVal"       "MoSold"       
# [78] "YrSold"        "SaleType"      "SaleCondition" "SalePrice"

# We are going to analyze all above variables 
summary(train)

# We will take out all those columns(including numeric) which can be made categorical
num = sapply(train, is.numeric)
index = names(train[,num])
index
# Now we check with description file
a = sapply(train[,index], function(x){length(unique(x))})
a = a[a<16]

# check each variable from a 
lapply(train[,names(a)], table)
# converting all variables to factor
train[,names(a)] = lapply(train[,names(a)], as.factor)

# Number of numeric variables
sum(sapply(train, is.numeric))

# Treat missing values
sapply(train, function(x){sum(is.na(x))})

# instead of dropping we consider hight NA var and replace them to presence or not of NA
train$Fence = ifelse(is.na(train$Fence), 1, 0)
train$MiscFeature = ifelse(is.na(train$MiscFeature), 1, 0)
train$PoolQC = ifelse(is.na(train$PoolQC), 1, 0)
train$FireplaceQu = ifelse(is.na(train$FireplaceQu), 1, 0)
train$Alley  = ifelse(is.na(train$Alley), 1, 0)

train = DMwR::knnImputation(train)
sum(is.na(train))

spl = caTools::sample.split(train$SalePrice, 0.8)

x.train = subset(train, spl == T)
x.test = subset(train, spl == F)


rf.fit = randomForest::randomForest(SalePrice~ . , data = x.train, do.trace = 100, ntree = 1000, importance = T )
pred = predict(rf.fit, x.test[,-81])

DMwR::regr.eval(x.test[,81], pred, stats = 'mape')
# 0.0940 mape
