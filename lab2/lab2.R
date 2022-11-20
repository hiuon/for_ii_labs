require(ggplot2) 
require(stringr) 
require(plyr) 
require(glmnet) 
require(xgboost) 
require(e1071)
require(dplyr) 
require(caret) 
require(moments) 
require(MASS)
require(corrplot) 
require(ggthemes)
require(plotly)
require(Amelia)
require(gbm)
require(gridExtra)

train <- read.csv('./train.csv', stringsAsFactors = F) 
test <- read.csv('./test.csv', stringsAsFactors = F)

train <- train[-which(train$GrLivArea > 4000),]
outlier <- 3000 # By Eye
train <- train[-which(train$TotalBsmtSF> outlier),]

outlier <- 100000  
train <- train[-which(train$LotArea>outlier),]

outlier <- 200 
train <- train[-which(train$LotFrontage>outlier),]

outcome <- log(train$SalePrice+1)
train <- train %>% within(rm('SalePrice'))
df <- rbind(train,test) %>% within(rm('Id'))

na.cols <- which(colSums(is.na(df)) > 0)
na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')


na.cols <- which(colSums(is.na(df)) > 0)
na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')

df$PoolQC[is.na(df$PoolQC)] <- "None"

df$MiscFeature[is.na(df$MiscFeature)] <- 'None'
df$Fence[is.na(df$Fence)] <- "None"

df$Alley[is.na(df$Alley)] <- "None"

df$FireplaceQu[is.na(df$FireplaceQu)] <- 'None'


lot.f = aggregate(LotFrontage ~ Neighborhood, data = df, median)
lot.f2 = c()
for (str in df$Neighborhood[is.na(df$LotFrontage)]) {
  lot.f2 = c(lot.f2, which(lot.f$Neighborhood == str))
}
df$LotFrontage[is.na(df$LotFrontage)] = lot.f[lot.f2, 2]

gar.cols <- c('GarageCond','GarageQual','GarageType','GarageFinish')
for (x in gar.cols){
  df[x][is.na(df[x])] <- 'None'
}

for(col in c('GarageYrBlt','GarageArea','GarageCars')){
  df[col][is.na(df[col])] <- 0
}

for(i in c('BsmtExposure', 'BsmtCond','BsmtQual','BsmtFinType2', 'BsmtFinType1')){
  df[i][is.na(df[i])] <- 'None'
}

for(i in c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath',
           'BsmtHalfBath')){
  df[i][is.na(df[i])] <- 0
}

df$MasVnrType[is.na(df$MasVnrType)] <- 'None'

df$MasVnrArea[is.na(df$MasVnrArea)] <- 0

df$MSZoning[is.na(df$MSZoning)] <- 'RL'

df <- df[,!names(df) %in% c('Utilities')]

df$Functional[is.na(df$Functional)] <- 'Typ'

df$Electrical[is.na(df$Electrical)] <- 'SBrkr'

df$KitchenQual[is.na(df$KitchenQual)] <- 'TA'

for(i in c('Exterior1st', 'Exterior2nd')){
  df[i][is.na(df[i])] <- 'VinylSd'
}

df$SaleType[is.na(df$SaleType)] <- 'WD'

df$MSSubClass[is.na(df$MSSubClass)] <- 'None'

any(is.na(df))

df$YrSold <- as.character(df$YrSold)

df$MoSold <- as.character(df$MoSold)

df$OverallCond <- as.character(df$OverallCond)

map_categories <- function(columns, categories, dataset){
  for (col in columns){
    dataset[col] <- as.numeric(categories[dataset[,col]])
  }
  return(dataset)
}

qual.cols <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu',
               'KitchenQual', 'HeatingQC', 'BsmtQual')
qual.list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
df <- map_categories(qual.cols, qual.list, df)

bsmt.list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
df = map_categories(c('BsmtExposure'), bsmt.list, df)

bsmt.fin.list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5,
                   'GLQ' = 6)
df <- map_categories(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list, df)

functional.list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4,
                     'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)
df$Functional <- as.numeric(functional.list[df$Functional])

garage.fin.list <- c('None' = 0,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)
df$GarageFinish <- as.numeric(garage.fin.list[df$GarageFinish])

fence.list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)
df$Fence <- as.numeric(fence.list[df$Fence])

nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown'= 1,
              'Edwards' = 1, 'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2,
              'NPkVill' = 2, 'Mitchel' = 2,'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes'=2,
              'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3,'Crawfor' =3, 'Veenker'=3,
              'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge'= 4,'NridgHt' =4)
df$NeighborhoodBin <- as.numeric(nbrh.map[df$Neighborhood])
df$NeighorbodBin2 <- (nbrh.map[df$Neighborhood])

MSdwelling.list <- c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)

df$NewerDwelling <- as.numeric(MSdwelling.list[as.character(df$MSSubClass)])

df$RegularLotShape <- (df$LotShape == 'Reg') * 1
df$LandLeveled <- (df$LandContour == 'Lvl') * 1
df$LandSlopeGentle <- (df$LandSlope == 'Gtl') * 1
df$ElectricalSB <- (df$Electrical == 'SBrkr') * 1
df$GarageDetchd <- (df$GarageType == 'Detchd') * 1
df$HasPavedDrive <- (df$PavedDrive == 'Y') * 1
df$HasWoodDeck <- (df$WoodDeckSF > 0) * 1
df$Has2ndFlr <- (df$X2ndFlrSF > 0) * 1
df$HasMasVnr <- (df$MasVnrArea > 0) * 1
df$HasShed <- (df$MiscFeature == 'Shed') * 1
df$Remodeled <- (df$YearBuilt != df$YearRemodAdd) * 1
df$RecentRemodel <- (df$YearRemodAdd >= as.numeric(df$YrSold)) * 1
df$NewHouse <- (df$YearBuilt == as.numeric(df$YrSold)) * 1
df$HighSeason <- (df$MoSold %in% c(5,6,7)) * 1

df$AreaInside <- as.numeric(df$X1stFlrSF + df$X2ndFlrSF)
df$HouseAge <- as.numeric(2010 - (df$YearBuilt))
df$TimeSinceSold <- as.numeric(2010 - as.numeric(df$YrSold))
df$YrSinceRemodel <- as.numeric(as.numeric(df$YrSold) - df$YearRemodAdd)
df$TotalSF <- as.numeric(df$TotalBsmtSF + df$X1stFlrSF + df$X2ndFlrSF)
df$AllSF <- as.numeric(df$TotalBsmtSF + df$GrLivArea)
df$OverallGrd <- as.numeric(df$OverallQual * as.numeric(df$OverallCond))
df$ExterGrade <- as.numeric(df$ExterQual * df$ExterCond)
df$GarageScore <- as.numeric(df$GarageArea * as.numeric(df$GarageQual))
df$TotalBath <- as.numeric(df$BsmtFullBath+df$BsmtHalfBath +df$FullBath +df$HalfBath)

oc.list <- c('1' = 1, '2'= 1, '3' = 1, '4' = 2,'5' = 2, '6' = 2, '7' = 3, '8' = 3,
             '9' = 3)
df$SimpleOverallCond <- as.numeric(oc.list[df$OverallCond])

yr.list <- c('2006' = 0, '2007'= 1, '2008' = 2, '2009' = 3,'2010' = 4)
df$YrSold<- as.numeric(yr.list[df$YrSold])

s.list <- c('Abnorml'=1,'Alloca'=1,'AdjLand'= 1,'Family'= 1,'Normal'= 0,'Partial'=0)
sa.list <- c('Abnorml'=0,'Alloca'=0,'AdjLand'= 0,'Family'= 0,'Normal'= 0,'Partial'=1)
df$SaleCondition_PriceDown <- as.numeric(s.list[df$SaleCondition])

corr.df <- cbind(df[1:1448,], SalePrice = outcome)

num.cols <- sapply(corr.df,is.numeric)
corr <- corr.df[,num.cols] %>% cor()

corr.SalePrice <- as.matrix(sort(corr[,'SalePrice'], decreasing = TRUE))
corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))

feature_classes <- sapply(names(df), function(x) {
  class(df[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])
df_numeric <- df[,numeric_feats]

skewed <- apply(df_numeric, 2, skewness)
skewed <-skewed[abs(skewed) > 0.75] 

for (x in names(skewed)) {
  
  df_numeric[[x]] <- log(df_numeric[[x]] + 1)
}
# normalize the data
scaler <- preProcess(df_numeric)
df_numeric <- predict(scaler, df_numeric)


# ONE HOT ENCODING FOR CATEGORICAL VARIABLES
# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])
df.categories <- df[,categorical_feats] 

# one hot encoding for categorical data
library(caret)
dummy <- dummyVars(" ~ .",data=df.categories)
df.categoric <- data.frame(predict(dummy,newdata=df.categories))

###
df <- cbind(df_numeric, df.categoric)
paste('The dataframe has', dim(df)[1], 'rows and', dim(df)[2], 'columns')

nzv.data <- nearZeroVar(df, saveMetrics = TRUE)
# take any of the near-zero-variance perdictors
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]

df <- df[,!names(df) %in% drop.cols]

paste('The dataframe now has', dim(df)[1], 'rows and', dim(df)[2], 'columns')



################################################################################

df_train <- df[1:1448,]
df_test <- df[1449:nrow(df),]

trainSparse <- sparse.model.matrix(~. , data = df_train)[,-1]
testSparse <- sparse.model.matrix(~., data = df_test)[,-1]

paramxgb <- list(booster = "gbtree",
                 eval_metric = "rmse",
                 eta = 0.03125,
                 colsample_bytree = 0.2,
                 max_depth = 4,
                 min_child_weight = 4,
                 gamma = 0.01,
                 alpha = 0.9,
                 lambda = 0.8,
                 subsample = 0.5,
                 silent = TRUE)

paramsvr <- list(gamma = 1e-4, cost = 1000, epsilon = 0.001)

# GBM
paramgbm <- list(shrinkage = 0.1, interaction.depth = 3, n.minobsinnode = 10)

mod.svm <- svm(x = as.matrix(trainSparse), y = outcome, type = "eps-regression",
               kernel = "radial",cost = paramsvr[2], gamma = paramsvr[1], 
               epsilon = paramsvr[3])

# Predict on test set 
pred.svr <- predict(mod.svm, newdata = as.matrix(testSparse))

#############################
# XGBoost
dtrain <- xgb.DMatrix(trainSparse, label = outcome)
dtest <- xgb.DMatrix(testSparse)
set.seed(1235)
mod.xgb <- xgboost(data=dtrain,params = paramxgb, nrounds=1660,print_every_n = 200)
# Predict on test set
predTest.xgb <- predict(mod.xgb, newdata = dtest)

# Feature importances
importance <- xgb.importance(feature_names = trainSparse@Dimnames[[2]], model = mod.xgb)
xgb.ggplot.importance(importance[1:20])

mod.gbm <- gbm.fit(x = as.matrix(trainSparse), y = outcome, n.trees = 150,
                   shrinkage = 0.1 ,interaction.depth = 3, n.minobsinnode = 10,
                   distribution = "gaussian",bag.fraction = 0.5)

# Predict
pred.gbm <- predict(mod.gbm, newdata = as.matrix(testSparse), n.trees = 150)

nModels <- 3 # xgb, svr, gbm
nfolds <- 5

set.seed(1235)
folds <- createFolds(outcome, k = nfolds)
# Separate the folds for training and holdout
foldsTrain <- folds[1:(nfolds-1)]
foldsHoldOut <- folds[nfolds]

# Actual Folds
nActualFolds <- nfolds - 1 # training 0n 4 folds

# create table for out-of-fold predictions
nrowF <- length(unlist(foldsTrain))
x_fold <- matrix(0, nrow = nrowF, ncol = 4)  
y_fold <- data.frame(Id = 0, ActualSalePrice = 0) 
Id.train <- 1:1443


trainSVR <- function(inTrain, inPred, param){
  # Separate into training and prediction sets
  Xtr <- as.matrix(trainSparse)[inTrain, ]
  Xpred <- as.matrix(trainSparse)[inPred, ]
  
  mod.svr <- svm(x = Xtr, y = outcome[inTrain], type = "eps-regression", 
                 kernel = "radial",cost = param[2], gamma = param[1], 
                 epsilon = param[3], scale = FALSE)
  
  # Predict 
  pred <- predict(mod.svr, newdata = Xpred)
  return(pred)
}

trainXGB <- function(inTrain, inPred, param, nrounds){
  # Separate into training and prediction sets
  Xtr <- trainSparse[inTrain, ]
  Xpred <- trainSparse[inPred, ]
  # xgb style matrices
  dXtr <- xgb.DMatrix(Xtr, label = outcome[inTrain])
  dXpred <- xgb.DMatrix(Xpred, label = outcome[inPred])
  # Model fit
  mod.xgb <- xgboost(data=dXtr, params = param, nrounds = nrounds, verbose = 0)
  # Predict 
  pred <- predict(mod.xgb, newdata = dXpred)
  return(pred)
}

trainGBM <- function(inTrain, inPred, param, n.tree){
  # Separate into training and prediction sets
  Xtr <- as.matrix(trainSparse[inTrain, ])
  Xpred <- as.matrix(trainSparse[inPred, ])
  
  # Model fit
  mod.gbm <- gbm.fit(x = Xtr, y = outcome[inTrain], n.trees = n.tree,
                     shrinkage = param[1] ,bag.fraction = 0.5,
                     interaction.depth = param[2], 
                     n.minobsinnode =  10, distribution = "gaussian")
  
  # Predict 
  pred <- predict(mod.gbm, newdata = as.matrix(Xpred), n.trees = n.tree)
  return(pred)
}

start <- 1; end <- 1
for (i in 1:nActualFolds){
  
  inTrain <- unlist(foldsTrain[-i]) # training fold
  inPred <- unlist(foldsTrain[i]) # test fold
  
  # XGBoost 
  predMod1 <- trainXGB(inTrain, inPred, paramxgb, nrounds = 1660)
  
  # SVR 
  predMod2 <- trainSVR(inTrain, inPred, paramsvr)
  
  # GBM
  predMod3 <- trainGBM(inTrain, inPred, paramgbm, n.tree = 150)
  end <- (start - 1) + length(inPred)
  x_fold[start:end, 1] <- Id.train[inPred] # Save Ids
  x_fold[start:end, 2] <- predMod1 # Save predictions from XGB
  x_fold[start:end, 3] <- predMod2 # Save predictions from SVR
  x_fold[start:end, 4] <- predMod3 # Save predictions from GBM
  
  # Save corresponding actual outcomes: Y_fold
  y_fold[start:end, 1] <- Id.train[inPred] # Save Ids
  y_fold[start:end, 2] <- outcome[inPred] # Save outcomes
  
  # Increase start
  start <- start + length(inPred)
}
trainOOF <- as.data.frame(x_fold)
colnames(trainOOF) <- c("Id", "predXGB", "predSVR", "predGBM")
y_fold <- mutate(y_fold, Id = as.integer(Id))
trainOOF <- trainOOF %>% mutate(Id = as.integer(Id)) %>% left_join(y_fold, by = "Id")

head(trainOOF)


trainLevel2 <- function(trainOOF, testOOF, cvfolds, lFinalFit = FALSE){
  # By cross validation, train Ridge Regression model
  Xtr <- trainOOF[,-c(1,5)]
  
  # Values of alpha
  alpha_values = c(0, 1e-6, 1e-5, 1e-4, 1e-3, 0.01, 0.1, 1)
  cv.results <- data.frame(alpha = alpha_values, lambda = 0, rmse = 0)
  for (ial in 1:length(alpha_values)){
    set.seed(101)
    mod.cv <- cv.glmnet(x = as.matrix(Xtr), y = trainOOF$ActualSalePrice, 
                        nfolds = cvfolds, type.measure = "mse",alpha =alpha_values[ial])
    
    # Determine lambda
    lam <- mod.cv$lambda.min; ind.lam <- which(mod.cv$lambda == lam) 
    
    # Store CV results
    cv.results[ial, ]$lambda <- mod.cv$lambda.min
    cv.results[ial, ]$rmse <- sqrt( mod.cv$cvm[ind.lam])
  }
  ind.best <- which.min(cv.results$rmse)
  alBest <- cv.results[ind.best, 1]
  lamBest <- cv.results[ind.best, 2]
  
  # In Final Fit, the outcomes of testOOS are unknown 
  if (lFinalFit == TRUE){ 
    Xts <- testOOF[,-1]
  } else{
    Xts <- testOOF[,-c(1,5)]
  }
  # Train and predict
  mod.level2 <- glmnet(x = as.matrix(Xtr), y = trainOOF$ActualSalePrice, lambda = lamBest, alpha = alBest) 
  pred <- predict(mod.level2, newx = as.matrix(Xts))
  return(pred)
}

lStackVerify = TRUE
# Function for creating stack folds
createStacks <- function(nfolds, lStackVerify){
  set.seed(1235)
  folds <- createFolds(outcome, k = nfolds)
  
  if (lStackVerify == TRUE){
    # Separate the folds for training and holdout
    foldsTrain <- folds[1:(nfolds-1)]
    foldsHoldOut <- folds[nfolds]
    # Return the list of folds
    list(foldsTrain, foldsHoldOut)  
  } else{
    folds
  }
}  

if(lStackVerify == TRUE){
  listFolds <- createStacks(nfolds, lStackVerify)
  foldsTrain <- listFolds[[1]]
  foldsHoldOut <- listFolds[[2]]
  
  # Predictions
  inTrain <- unlist(foldsTrain)
  inHoldOut <- unlist(foldsHoldOut)
  
  predXGB <- trainXGB(inTrain, inHoldOut, paramxgb, nrounds = 1660)
  predSVR <- trainSVR(inTrain, inHoldOut, paramsvr)
  predGBM <- trainGBM(inTrain, inHoldOut, paramgbm, n.tree = 150)
  
  # The HoldOut data frame to be used in Level2
  yHoldOut <- outcome[inHoldOut]
  trainHoldOut <- data.frame(Id = Id.train[inHoldOut], predXGB = predXGB, 
                             predSVR = predSVR, predGBM = predGBM,
                             ActualSalePrice = yHoldOut)
  
  # Now using the level2 model, predict on HoldOut data
  predStack <- trainLevel2(trainOOF, trainHoldOut, cvfolds = 5)
  
  # Calculate RMSE for each model and stacked model
  rmse1 <- sqrt( mean( (yHoldOut - predXGB)^2 ) )
  rmse2 <- sqrt( mean( (yHoldOut - predSVR)^2 ) )
  rmse3 <- sqrt( mean( (yHoldOut - predGBM)^2 ) )
  rmseStack <- sqrt( mean( (yHoldOut - predStack)^2 ) )
}

cat("The rmse values are: Xgboost rmse:",rmse1, "Svr rmse:",rmse2,
    "GBM rmse:",rmse3,"Stack model:",rmseStack)

train.OOF <- rbind(trainOOF,trainHoldOut)

# Combine the test set prediction for base models
testOOF <- data.frame(Id = 1461:2919, predXGB = predTest.xgb, predSVR = pred.svr,
                      predGBM = pred.gbm)
# Final Predict using meta model             
pred.Stack <- trainLevel2(train.OOF, testOOF, cvfolds = 5, lFinalFit = TRUE)

# Final Submission
submission <- data.frame(Id = 1461:2919, SalePrice = exp(pred.Stack))
colnames(submission) <- c("Id", "SalePrice")
write.csv(submission,"stacked.csv",row.names = FALSE)
