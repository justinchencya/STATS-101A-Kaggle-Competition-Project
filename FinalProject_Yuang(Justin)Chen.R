library(dplyr)
library(corrplot)
library(glmnet)
library(forcats)
library(car)
library(leaps)
library(moments)
library(ggplot2)
library(Rmisc)
library(caTools)

# Read-in Data ------------------------------------------------------------

initial_data <- read.csv('HTrainW19Final.csv',header = TRUE, row.names = 1, sep = ',')
str(initial_data)
summary(initial_data)
head(initial_data)



# Data Cleaning ---------------------------------------------------------------------

## Remove columns with all missing values
not_all_NA <- rep(TRUE,ncol(initial_data))
for (i in 1:ncol(initial_data)){
  if (mean(is.na(initial_data[i])) == 1)
    not_all_NA[i] <- FALSE
}
initial_data <- initial_data[,not_all_NA]

## Remove columns with single values
not_single_value <- rep(TRUE, ncol(initial_data))
for (i in 1:ncol(initial_data)){
  if (nrow(unique(na.omit(initial_data[i]))) == 1)
    not_single_value[i] <- FALSE
}
colnames(initial_data)[!not_single_value]
summary(initial_data$Utilities)
initial_data <- initial_data[,not_single_value]
str(initial_data)

## Remove records with missing prices
initial_data <- initial_data %>% filter(!is.na(SalePrice))



# Investigate columns with over 10% missing values ------------------------

NA_10perc <- rep(TRUE,ncol(initial_data))
for (i in 1:ncol(initial_data)){
  if (mean(is.na(initial_data[i])) >= 0.1)
    NA_10perc[i] <- FALSE
}
colnames(initial_data)[!NA_10perc]

## Alley
summary(initial_data$Alley)
plot(SalePrice~Alley, data = initial_data)
levels(initial_data$Alley)[length(levels(initial_data$Alley)) + 1] <- 'NoAlley'
initial_data$Alley[is.na(initial_data$Alley)] <- 'NoAlley'
summary(initial_data$Alley)
plot(SalePrice~Alley, data = initial_data)

## FireplaceQu
summary(initial_data$FireplaceQu)
plot(Fireplaces~FireplaceQu, data = initial_data)
levels(initial_data$FireplaceQu)[length(levels(initial_data$FireplaceQu)) + 1] <- 'NoFirePlace'
initial_data$FireplaceQu[is.na(initial_data$FireplaceQu)] <- 'NoFirePlace'
summary(initial_data$FireplaceQu)
plot(Fireplaces~FireplaceQu, data = initial_data)
plot(SalePrice~FireplaceQu, data = initial_data)

## PoolQC
summary(initial_data$PoolQC)
plot(PoolArea~PoolQC, data = initial_data)
levels(initial_data$PoolQC)[length(levels(initial_data$PoolQC)) + 1] <- 'NoPool'
initial_data$PoolQC[is.na(initial_data$PoolQC)] <- 'NoPool'
summary(initial_data$PoolQC)
plot(PoolArea~PoolQC, data = initial_data)
plot(SalePrice~PoolQC, data = initial_data)

## Fence
summary(initial_data$Fence)
plot(SalePrice~Fence, data = initial_data)
levels(initial_data$Fence)[length(levels(initial_data$Fence)) + 1] <- 'NoFence'
initial_data$Fence[is.na(initial_data$Fence)] <- 'NoFence'
summary(initial_data$Fence)
plot(SalePrice~Fence, data = initial_data)

## MiscFeature
summary(initial_data$MiscFeature)
plot(SalePrice~MiscFeature, data = initial_data)
levels(initial_data$MiscFeature)[length(levels(initial_data$MiscFeature)) + 1] <- 'NoMiscFeature'
initial_data$MiscFeature[is.na(initial_data$MiscFeature)] <- 'NoMiscFeature'
summary(initial_data$MiscFeature)
plot(SalePrice~MiscFeature, data = initial_data)

## LotFrontage - This is tricky: fill-in median of houses in the same neighborhood
summary(initial_data$LotFrontage)
for (i in 1:nrow(initial_data)){
  if (is.na(initial_data$LotFrontage[i])){
    neighbor_temp <- initial_data$Neighborhood[i]
    median_lotFrontage <- median(na.omit(initial_data$LotFrontage[initial_data$Neighborhood == neighbor_temp]))
    initial_data$LotFrontage[i] <- median_lotFrontage
  }
}



# Fill in missing values --------------------------------------------------
initial_data1 <- initial_data

## Function for filling missing values in categorical predictors
fill_catg_NA <- function(my_col){
  my_col_na <- is.na(my_col)
  my_col[my_col_na] <- sample(my_col[!my_col_na], size = sum(my_col_na), replace = TRUE)
  return(my_col)
}

## For categorical variables, input missing values according to the distribution of valid values
## For numerical variables, fill in the medians

for (i in 1:ncol(initial_data1)){
  if(any(is.na(initial_data1[[i]]))){
    if (is.factor(initial_data1[[i]])){
      initial_data1[[i]] <- fill_catg_NA(initial_data1[[i]])
    }else{
      initial_data1[[i]][which(is.na(initial_data1[[i]]))] <- median(na.omit(initial_data1[[i]]))
    }
  }
}

summary(initial_data1)



# Univariate Analysis - Numeric Features ----------------------------------

## Correlation Matrix
numeric_vars <- initial_data1[sapply(initial_data1, is.numeric)]
categorical_vars <- initial_data1[sapply(initial_data1, is.factor)]

corrmat <- round(cor(numeric_vars),4)
corrplot(corrmat)

Y_indx <- which(names(numeric_vars) == 'SalePrice')
corrmat_ordered <- corrmat[,Y_indx][order(abs(corrmat[,Y_indx]), decreasing = TRUE)]
corrmat_ordered

## X-Y Plots
p1 <- ggplot(initial_data1, aes(OverallQual, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p2 <- ggplot(initial_data1, aes(GrLivArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p3 <- ggplot(initial_data1, aes(LotArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p4 <- ggplot(initial_data1, aes(LotFrontage, SalePrice)) + geom_point(color = 'blue') + theme_bw()
p5 <- ggplot(initial_data1, aes(GarageArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
multiplot(p1, p2, p3, p4, p5, cols=2)

## Convert selected non-count numeric features into categorical variables
convert_names <- c('MSSubClass','MoSold', 'YrSold')
for (i in 1:length(convert_names)){
  initial_data1[[convert_names[i]]] <- as.factor(initial_data1[[convert_names[i]]])
}



# Univariate Analysis - Categorical Features ------------------------------

## Convert Q&C into discrete quantitative features
QCNames <- c('ExterQual','ExterCond','BsmtQual','BsmtCond',
             'HeatingQC','KitchenQual','FireplaceQu','GarageQual','GarageCond','PoolQC')
summary(initial_data1[QCNames])

QCConvert <- function(my_col){
  new_col <- rep(NA, length(my_col))
  for (i in 1:length(my_col)){
    if (is.na(my_col[i])){
      next
    }else if(my_col[i] == 'Ex'){
      new_col[i] <- 5
    }else if(my_col[i] == 'Gd'){
      new_col[i] <- 4
    }else if(my_col[i] == 'TA'){
      new_col[i] <- 3
    }else if(my_col[i] == 'Fa'){
      new_col[i] <- 2
    }else if(my_col[i] == 'Po'){
      new_col[i] <- 1
    }else{
      new_col[i] <- 0
    }
  }
  return (new_col)
}

for (i in 1:length(QCNames)){
  initial_data1[[QCNames[i]]] <- QCConvert(initial_data1[[QCNames[i]]])
}

new_Fence <- rep(NA, length(initial_data1$Fence))
for (i in 1:length(initial_data1$Fence)){
  if (initial_data1$Fence[i] == 'GdPrv' || initial_data1$Fence[i] == 'GdWo'){
    new_Fence[i] <- 5
  }else if (initial_data1$Fence[i] == 'MnPrv' || initial_data1$Fence[i] == 'MnWw'){
    new_Fence[i] <- 1
  }else{
    new_Fence[i] <- 0
  }
}
initial_data1$Fence <- new_Fence



# Feature Engineering -----------------------------------------------------

## NeighborhoodLevel
plot(SalePrice~Neighborhood, initial_data1)
initial_data1$NeighborhoodLevel[initial_data1$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
initial_data1$NeighborhoodLevel[!initial_data1$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
initial_data1$NeighborhoodLevel[initial_data1$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

## TotBathRms
initial_data1$TotBathRms <- initial_data1$FullBath + (initial_data1$HalfBath*0.5) + initial_data1$BsmtFullBath + (initial_data1$BsmtHalfBath*0.5)

## Remodeled
initial_data1$Remodeled <- as.factor(initial_data1$YearBuilt == initial_data1$YearRemodAdd)
summary(initial_data1$Remodeled)
plot(SalePrice~Remodeled, initial_data1)

## SeasonSold
plot(SalePrice~MoSold, initial_data1)
plot(SalePrice~YrSold, initial_data1)
for (i in 1:nrow(initial_data1)){
  month_num <- as.numeric(initial_data1$MoSold[i])
  if (month_num >= 3 && month_num <= 5){
    season <- 'Spring'
  }else if (month_num >= 6 && month_num <= 8){
    season <- 'Summer'
  }else if (month_num >= 9 && month_num <= 11){
    season <- 'Fall'
  }else if (month_num >= 12 && month_num <= 2){
    season <- 'Winter'
  }
  initial_data1$SeasonSold[i] <- paste0(initial_data1$YrSold[i], season)
}
initial_data1$SeasonSold <- as.factor(initial_data1$SeasonSold)
summary(initial_data1$SeasonSold)
plot(SalePrice~SeasonSold, initial_data1)

## NewHouse
initial_data1$NewHouse <- as.factor(initial_data1$YearBuilt == initial_data1$YrSold)
summary(initial_data1$NewHouse)
plot(SalePrice~NewHouse, initial_data1)

## Total Square Footage
initial_data1$TotalSF <- initial_data1$TotalBsmtSF + initial_data1$GrLivArea
initial_data1$TotalFloorSF <- initial_data1$X1stFlrSF + initial_data1$X2ndFlrSF
initial_data1$TotalPorchSF <- initial_data1$OpenPorchSF + initial_data1$EnclosedPorch + initial_data1$X3SsnPorch + initial_data1$ScreenPorch

## Boolean Features
initial_data1$HasBsmt <- initial_data1$TotalBsmtSF > 0
initial_data1$HasGarage <- initial_data1$GarageArea > 0
initial_data1$HasPorch <- initial_data1$TotalPorchSF > 0
initial_data1$HasPool <- initial_data1$PoolArea > 0
initial_data1$Completed <- initial_data1$SaleCondition != 'Partial'

## Transform Skewed Numeric Features
for(i in 1:ncol(initial_data1)){
  if (is.numeric(initial_data1[,i]) && abs(skewness(initial_data1[,i]))>0.8){
    initial_data1[,i] <- log(initial_data1[,i] +1)
  }
}



# Import and Prepare Test Data --------------------------------------------

test_data <- read.csv("HTestW19Final No Y values.csv", header = TRUE, row.names = 1, sep = ",")

test_data$Utilities <- NULL

levels(test_data$Alley)[length(levels(test_data$Alley)) + 1] <- 'NoAlley'
test_data$Alley[is.na(test_data$Alley)] <- 'NoAlley'

levels(test_data$FireplaceQu)[length(levels(test_data$FireplaceQu)) + 1] <- 'NoFirePlace'
test_data$FireplaceQu[is.na(test_data$FireplaceQu)] <- 'NoFirePlace'

levels(test_data$PoolQC)[length(levels(test_data$PoolQC)) + 1] <- 'NoPool'
test_data$PoolQC[is.na(test_data$PoolQC)] <- 'NoPool'

levels(test_data$Fence)[length(levels(test_data$Fence)) + 1] <- 'NoFence'
test_data$Fence[is.na(test_data$Fence)] <- 'NoFence'

levels(test_data$MiscFeature)[length(levels(test_data$MiscFeature)) + 1] <- 'NoMiscFeature'
test_data$MiscFeature[is.na(test_data$MiscFeature)] <- 'NoMiscFeature'

summary(test_data$LotFrontage)
for (i in 1:nrow(test_data)){
  if (is.na(test_data$LotFrontage[i])){
    neighbor_temp <- test_data$Neighborhood[i]
    median_lotFrontage <- median(na.omit(test_data$LotFrontage[test_data$Neighborhood == neighbor_temp]))
    test_data$LotFrontage[i] <- median_lotFrontage
  }
}

for (i in 1:ncol(test_data)){
  if(any(is.na(test_data[[i]]))){
    if (is.factor(test_data[[i]])){
      test_data[[i]] <- fill_catg_NA(test_data[[i]])
    }else{
      test_data[[i]][which(is.na(test_data[[i]]))] <- median(na.omit(test_data[[i]]))
    }
  }
}

for (i in 1:length(convert_names)){
  test_data[[convert_names[i]]] <- as.factor(test_data[[convert_names[i]]])
}

for (i in 1:length(QCNames)){
  test_data[[QCNames[i]]] <- QCConvert(test_data[[QCNames[i]]])
}

new_Fence <- rep(NA, length(test_data$Fence))
for (i in 1:length(test_data$Fence)){
  if (test_data$Fence[i] == 'GdPrv' || test_data$Fence[i] == 'GdWo'){
    new_Fence[i] <- 5
  }else if (test_data$Fence[i] == 'MnPrv' || test_data$Fence[i] == 'MnWw'){
    new_Fence[i] <- 1
  }else{
    new_Fence[i] <- 0
  }
}
test_data$Fence <- new_Fence

## NeighborhoodLevel
test_data$NeighborhoodLevel[test_data$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
test_data$NeighborhoodLevel[!test_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
test_data$NeighborhoodLevel[test_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

## TotBathRms
test_data$TotBathRms <- test_data$FullBath + (test_data$HalfBath*0.5) + test_data$BsmtFullBath + (test_data$BsmtHalfBath*0.5)

## Remodeled
test_data$Remodeled <- as.factor(test_data$YearBuilt == test_data$YearRemodAdd)

## SeasonSold
for (i in 1:nrow(test_data)){
  month_num <- as.numeric(test_data$MoSold[i])
  if (month_num >= 3 && month_num <= 5){
    season <- 'Spring'
  }else if (month_num >= 6 && month_num <= 8){
    season <- 'Summer'
  }else if (month_num >= 9 && month_num <= 11){
    season <- 'Fall'
  }else if (month_num >= 12 && month_num <= 2){
    season <- 'Winter'
  }
  test_data$SeasonSold[i] <- paste0(test_data$YrSold[i], season)
}
test_data$SeasonSold <- as.factor(test_data$SeasonSold)

## NewHouse
test_data$NewHouse <- as.factor(test_data$YearBuilt == test_data$YrSold)

## Total Square Footage
test_data$TotalSF <- test_data$TotalBsmtSF + test_data$GrLivArea
test_data$TotalFloorSF <- test_data$X1stFlrSF + test_data$X2ndFlrSF
test_data$TotalPorchSF <- test_data$OpenPorchSF + test_data$EnclosedPorch + test_data$X3SsnPorch + test_data$ScreenPorch

## Boolean Features
test_data$HasBsmt <- test_data$TotalBsmtSF > 0
test_data$HasGarage <- test_data$GarageArea > 0
test_data$HasPorch <- test_data$TotalPorchSF > 0
test_data$HasPool <- test_data$PoolArea > 0
test_data$Completed <- test_data$SaleCondition != 'Partial'

## Transform Skewed Numeric Features
for(i in 1:ncol(test_data)){
  if (is.numeric(test_data[,i]) && abs(skewness(test_data[,i]))>0.8){
    test_data[,i] <- log(test_data[,i] +1)
  }
}



# Engineer Predictors with Different Levels -------------------------------

for (i in 1:ncol(initial_data1)){
  if(is.factor(initial_data1[[i]])){
    test_col_index <- which(names(test_data) == names(initial_data1)[i])
    new_levels <- setdiff(levels(test_data[[test_col_index]]), levels(initial_data1[[i]])) # Levels in testing but not in training
    if (length(new_levels) >= 1){                                                                    
      cat('Predictor Name:', names(initial_data1[i]))
      cat('\n')
      cat('New Levels:', new_levels)
      cat('\n')
    }
  }
}

test_data$Condition2[test_data$Condition2 == 'RRAn'] <- 'Norm'
test_data$Condition2 <- droplevels(test_data$Condition2)



# Train-Validate Split ----------------------------------------------------

sample <- sample.split(initial_data1$SalePrice, SplitRatio = .7)
train_data <- subset(initial_data1, sample == TRUE)
validate_data <- subset(initial_data1, sample == FALSE)

train_data <- droplevels(train_data)
validate_data <- droplevels(validate_data)

for (i in 1:ncol(train_data)){
  if(is.factor(train_data[[i]])){
    test_col_index <- which(names(validate_data) == names(train_data)[i])
    new_levels <- setdiff(levels(validate_data[[test_col_index]]), levels(train_data[[i]])) # Levels in testing but not in training
    if (length(new_levels) >= 1){                                                                    
      cat('Predictor Name:', names(train_data[i]))
      cat('\n')
      cat('New Levels:', new_levels)
      cat('\n')
    }
  }
}

validate_data$MSSubClass[validate_data$MSSubClass == 40] <- 20
validate_data$MSSubClass <- droplevels(validate_data$MSSubClass)

validate_data$Condition2[validate_data$Condition2 == 'RRNn'] <- 'Norm'
validate_data$Condition2 <- droplevels(validate_data$Condition2)

validate_data$Exterior2nd[validate_data$Exterior2nd == 'CBlock'] <- 'VinylSd'
validate_data$Exterior2nd <- droplevels(validate_data$Exterior2nd)



# Full Model with All Predictors ------------------------------------------

lm_full <- lm(SalePrice~., train_data)



# Backward Elimination ----------------------------------------------------

## AIC
backAIC <- step(lm_full, direction = 'backward')

## BIC
backBIC <- step(lm_full, direction = 'backward', k = log(nrow(train_data)))



# Forward Selection -------------------------------------------------------

## AIC
lm_min <- lm(SalePrice~1, train_data)
forwardAIC <- step(lm_min, direction = 'forward', scope = list(lower = lm_min, upper = lm_full))

## BIC
forwardBIC <- step(lm_min, direction = 'forward', scope = list(lower = lm_min, upper = lm_full), k = log(nrow(train_data)))



# Determining the Best Subset ---------------------------------------------

summary(backAIC)$adj.r.squared 
summary(backBIC)$adj.r.squared
summary(forwardAIC)$adj.r.squared 
summary(forwardBIC)$adj.r.squared

## Duplicate Model Selected
pred_backAIC <- predict(backAIC, validate_data)
pred_backBIC <- predict(backBIC, validate_data)
pred_forwardAIC <- predict(forwardAIC, validate_data)
pred_forwardBIC <- predict(forwardBIC, validate_data)

RSS_backAIC <- sum((pred_backAIC-validate_data$SalePrice)^2)
RSS_backBIC <- sum((pred_backBIC-validate_data$SalePrice)^2)
RSS_forwardAIC <- sum((pred_forwardAIC-validate_data$SalePrice)^2)
RSS_forwardBIC <- sum((pred_forwardBIC-validate_data$SalePrice)^2)

best_model_indx <- which.min(c(RSS_backAIC, RSS_backBIC, RSS_forwardAIC, RSS_forwardBIC))

if (best_model_indx == 1){
  lm_selected <- backAIC
}else if (best_model_indx == 2){
  lm_selected <- backBIC
}else if (best_model_indx == 3){
  lm_selected <- forwardAIC
}else if (best_model_indx == 4){
  lm_selected <- forwardBIC
}

lm1 <- lm_selected
summary(lm1)

## Linear Dependent Predictors
## Remove BldgType
lm2 <- lm(formula = SalePrice ~ OverallQual + TotalSF + Neighborhood + 
            BsmtUnfSF + GarageCars + OverallCond + RoofMatl + Foundation + 
            TotBathRms + LotArea + YearBuilt + Functional + TotalFloorSF + 
            BsmtFinSF1 + Condition2 + SaleCondition + BsmtExposure + 
            KitchenQual + Exterior1st + MSSubClass + BsmtFinType1 + Heating + 
            MSZoning + LandContour + GarageArea + Street + KitchenAbvGr + 
            ScreenPorch + SeasonSold + HouseStyle + HasBsmt + TotalBsmtSF + 
            HasPorch + OpenPorchSF + BsmtFinType2 + FullBath + GarageQual + 
            ExterQual + ExterCond + HasGarage + LotFrontage + 
            HeatingQC, data = train_data)
names(coef(lm2))[is.na(coef(lm2))]
summary(lm2)

## Multicolinearity
## Remove TotalBsmtSF
vif(lm2)

lm2 <- lm(formula = SalePrice ~ OverallQual + TotalSF + Neighborhood + 
            BsmtUnfSF + GarageCars + OverallCond + RoofMatl + Foundation + 
            TotBathRms + LotArea + YearBuilt + Functional + TotalFloorSF + 
            BsmtFinSF1 + Condition2 + SaleCondition + BsmtExposure + 
            KitchenQual + Exterior1st + MSSubClass + BsmtFinType1 + Heating + 
            MSZoning + LandContour + GarageArea + Street + KitchenAbvGr + 
            ScreenPorch + SeasonSold + HouseStyle + HasBsmt + 
            HasPorch + OpenPorchSF + BsmtFinType2 + FullBath + GarageQual + 
            ExterQual + ExterCond + HasGarage + LotFrontage + 
            HeatingQC, data = train_data)
vif(lm2)
summary(lm2)
par(mfrow = c(2,2))
plot(lm2)

## Remove Insigficant Predictors and Add Interactions
## Remove LotFrontage, HasGarage, ExterCond, GarageQual, HeatingQC, ExterQual, FullBath, HasPorch, HasBsmt, OpenPorchSF, ScreenPorch
##        KitchenAbvGr, BsmtFinType2
## Add OverallQual:Neighborhood+OverallQual:MSSubClass+OverallQual:GarageArea+OverallQual:SeasonSold+
##     TotalSF:Neighborhood+TotalSF:MSSubClass+TotalSF:SaleCondition+TotalSF:MSZoning+TotalSF:Neighborhood+
##     TotalSF:SeasonSold+TotalSF:HasGarage+OverallQual:YearBuilt+Neighborhood:YearBuilt+SeasonSold:YearBuilt+
##     Foundation:YearBuilt +MSSubClass:YearBuilt
anova(lm2)[order(anova(lm2)[,2]),]
colnames(train_data)

lm3 <- lm(formula = SalePrice ~ OverallQual + TotalSF + Neighborhood + 
            BsmtUnfSF + GarageCars + OverallCond + RoofMatl + Foundation + 
            TotBathRms + LotArea + YearBuilt + Functional + TotalFloorSF + 
            BsmtFinSF1 + Condition2 + SaleCondition + BsmtExposure + 
            KitchenQual + Exterior1st + MSSubClass + BsmtFinType1 + Heating + 
            MSZoning + LandContour + GarageArea + Street + 
            SeasonSold + HouseStyle + HasBsmt +
            OverallQual:Neighborhood+OverallQual:MSSubClass+OverallQual:GarageArea+OverallQual:SeasonSold+
            TotalSF:Neighborhood+TotalSF:MSSubClass+TotalSF:SaleCondition+TotalSF:MSZoning+TotalSF:Neighborhood+TotalSF:SeasonSold+
            TotalSF:HasGarage+OverallQual:YearBuilt+Neighborhood:YearBuilt+SeasonSold:YearBuilt+Foundation:YearBuilt +MSSubClass:YearBuilt,
          data = train_data)
summary(lm3)
par(mfrow = c(2,2))
plot(lm3)



# Backward Elimination ----------------------------------------------------

## AIC
backAIC <- step(lm3, direction = 'backward')

## BIC
backBIC <- step(lm3, direction = 'backward', k = log(nrow(train_data)))



# Forward Selection -------------------------------------------------------

## AIC
lm_min <- lm(SalePrice~1, train_data)
forwardAIC <- step(lm_min, direction = 'forward', scope = list(lower = lm_min, upper = lm3))

## BIC
forwardBIC <- step(lm_min, direction = 'forward', scope = list(lower = lm_min, upper = lm3), k = log(nrow(train_data)))



# Determining the Best Subset ---------------------------------------------

summary(backAIC)$adj.r.squared 
summary(backBIC)$adj.r.squared
summary(forwardAIC)$adj.r.squared 
summary(forwardBIC)$adj.r.squared

## Duplicate Model Selected
pred_backAIC <- predict(backAIC, validate_data)
pred_backBIC <- predict(backBIC, validate_data)
pred_forwardAIC <- predict(forwardAIC, validate_data)
pred_forwardBIC <- predict(forwardBIC, validate_data)

RSS_backAIC <- sum((pred_backAIC-validate_data$SalePrice)^2)
RSS_backBIC <- sum((pred_backBIC-validate_data$SalePrice)^2)
RSS_forwardAIC <- sum((pred_forwardAIC-validate_data$SalePrice)^2)
RSS_forwardBIC <- sum((pred_forwardBIC-validate_data$SalePrice)^2)

best_model_indx <- which.min(c(RSS_backAIC, RSS_backBIC, RSS_forwardAIC, RSS_forwardBIC))

if (best_model_indx == 1){
  lm_selected <- backAIC
}else if (best_model_indx == 2){
  lm_selected <- backBIC
}else if (best_model_indx == 3){
  lm_selected <- forwardAIC
}else if (best_model_indx == 4){
  lm_selected <- forwardBIC
}

summary(lm_selected)

## Refit the final model using full training data
## Manually add back Neighborhood:YearBuilt + SeasonSold:YearBuilt + Foundation:YearBuilt +
##                   MSSubClass:YearBuilt + TotalSF:Neighborhood + TotalSF:MSSubClass
lm4 <- lm(SalePrice~OverallQual + TotalSF + Neighborhood + 
            OverallCond + RoofMatl + TotBathRms + LotArea + YearBuilt + 
            Functional + TotalFloorSF + BsmtFinSF1 + Condition2 + SaleCondition + 
            BsmtExposure + KitchenQual + BsmtFinType1 + Heating + GarageArea + 
            Street + HasBsmt + OverallQual:Neighborhood + TotalSF:SaleCondition + 
            OverallQual:YearBuilt + Neighborhood:YearBuilt + SeasonSold:YearBuilt + Foundation:YearBuilt +
            MSSubClass:YearBuilt + TotalSF:Neighborhood + TotalSF:MSSubClass,
          initial_data1)
summary(lm4)
par(mfrow = c(2,2))
plot(lm4)



# Model Outliers ----------------------------------------------------------

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

outlier_tf <- abs(rstudent(lm4)) > 2
outlier_tf[is.na(outlier_tf)] <- FALSE

outlierPts <- initial_data1[outlier_tf,]
ordinaryPts <- initial_data1[!outlier_tf,]

numeric_influencer <- character()
categ_influencer_modeLevel <- character()
categ_influencer_uniqueLevel <- character()

for (i in 1:ncol(initial_data1)){
  var_name <- colnames(initial_data1)[i]
  if (is.numeric(initial_data1[[i]])){
    mean_outlr <- mean(outlierPts[[i]])
    mean_ordinary <- mean(ordinaryPts[[i]])
    if (abs(mean_ordinary - mean_outlr) >= 0.3*sd(initial_data1[[i]])){
      numeric_influencer <- c(numeric_influencer, var_name)
      cat(var_name)
      cat('\n')
      cat(mean_outlr)
      cat('\n')
      cat(mean_ordinary)
      cat('\n')
      cat('\n')
    }
  }else{
    mode_level_outlr <- getmode(droplevels(as.factor(outlierPts[[i]])))
    mode_level_ordinary <- getmode(droplevels(as.factor(ordinaryPts[[i]])))
    if (as.character(mode_level_ordinary) != as.character(mode_level_outlr)){
      categ_influencer_modeLevel <- c(categ_influencer_modeLevel, var_name)
      cat('Different Mode Level: ')
      cat(var_name)
      cat('\n')
      cat(mode_level_outlr)
      cat('\n')
      cat(mode_level_ordinary)
      cat('\n')
      cat('\n')
    }
    
    levels_outlr <- levels(droplevels(as.factor(outlierPts[[i]])))
    levels_ordinary <- levels(droplevels(as.factor(ordinaryPts[[i]])))
    outlr_unique_level <- setdiff(levels_outlr, levels_ordinary)
    
    if (length(outlr_unique_level) > 0){
      categ_influencer_uniqueLevel <- c(categ_influencer_uniqueLevel, var_name)
      cat('Different Number of Levels: ')
      cat(var_name)
      cat('\n')
      cat(outlr_unique_level)
      cat('\n')
      cat('\n')
    }
  }
}
numeric_influencer
categ_influencer_modeLevel
categ_influencer_uniqueLevel

par(mfrow = c(1,2))
plot(outlierPts$YearRemodAdd)
plot(ordinaryPts$YearRemodAdd)

par(mfrow = c(1,2))
plot(outlierPts$MasVnrArea)
plot(ordinaryPts$MasVnrArea)

par(mfrow = c(1,2))
plot(outlierPts$BsmtQual)
plot(ordinaryPts$BsmtQual)

par(mfrow = c(1,2))
plot(outlierPts$BsmtCond)
plot(ordinaryPts$BsmtCond)

par(mfrow = c(1,2))
plot(outlierPts$BsmtFinSF1)
plot(ordinaryPts$BsmtFinSF1)

par(mfrow = c(1,2))
plot(outlierPts$BsmtFullBath)
plot(ordinaryPts$BsmtFullBath)

par(mfrow = c(1,2))
plot(outlierPts$KitchenAbvGr)
plot(ordinaryPts$KitchenAbvGr) # Note: Outliers have KitchenAbvGr > 1

par(mfrow = c(1,2))
plot(outlierPts$KitchenQual)
plot(ordinaryPts$KitchenQual)

par(mfrow = c(1,2))
plot(outlierPts$TotRmsAbvGrd)
plot(ordinaryPts$TotRmsAbvGrd)

par(mfrow = c(1,2))
plot(outlierPts$GarageYrBlt)
plot(ordinaryPts$GarageYrBlt)

par(mfrow = c(1,2))
plot(outlierPts$GarageQual)
plot(ordinaryPts$GarageQual)

par(mfrow = c(1,2))
plot(outlierPts$GarageCond)
plot(ordinaryPts$GarageCond)

par(mfrow = c(1,2))
plot(outlierPts$Neighborhood)
plot(ordinaryPts$Neighborhood) # Note: A lot of Outliers in OldTown

par(mfrow = c(1,2))
plot(outlierPts$Foundation)
plot(ordinaryPts$Foundation) # Note: Outliers have way more BrkTil

par(mfrow = c(1,2))
plot(outlierPts$BsmtFinType1)
plot(ordinaryPts$BsmtFinType1) # Note: Outliers have a lot less GLQ

par(mfrow = c(1,2))
plot(outlierPts$GarageType)
plot(ordinaryPts$GarageType) # Note: Outliers have a lot more Detchd

par(mfrow = c(1,2))
plot(outlierPts$Remodeled)
plot(ordinaryPts$Remodeled) # Note: A lot of Outliers are Remodeled

par(mfrow = c(1,2))
summary(outlierPts$Exterior2nd)
summary(ordinaryPts$Exterior2nd)

## Construct a new variable marking potential outliers
for (i in 1:nrow(initial_data1)){
  initial_data1$SuspectOutlr[i] <- sum(c(initial_data1$KitchenAbvGr[i] > 1, initial_data1$Neighborhood[i] == 'OldTown', 
                                         initial_data1$Foundation[i] == 'BrkTil', initial_data1$GarageType[i] != 'GLQ',
                                         initial_data1$GarageType[i] == 'Detchd', initial_data1$Remodeled[i] == 'TRUE'))
}

for (i in 1:nrow(test_data)){
  test_data$SuspectOutlr[i] <- sum(c(test_data$KitchenAbvGr[i] > 1, test_data$Neighborhood[i] == 'OldTown', 
                                         test_data$Foundation[i] == 'BrkTil', test_data$GarageType[i] != 'GLQ',
                                         test_data$GarageType[i] == 'Detchd', test_data$Remodeled[i] == 'TRUE'))
}



# Model Bad Leverage Points -----------------------------------------------

BadLev_tf <- cooks.distance(lm4) >= 4/(nrow(initial_data1) - (length(coef(lm4))) - 1)
BadLev_tf[is.na(BadLev_tf)] <- FALSE

BadLeveragePts <- initial_data1[BadLev_tf,]
ordinaryPts <- initial_data1[!BadLev_tf,]

numeric_influencer <- character()
categ_influencer_modeLevel <- character()
categ_influencer_uniqueLevel <- character()

for (i in 1:ncol(initial_data1)){
  var_name <- colnames(initial_data1)[i]
  if (is.numeric(initial_data1[[i]])){
    mean_blp <- mean(BadLeveragePts[[i]])
    mean_ordinary <- mean(ordinaryPts[[i]])
    if (abs(mean_ordinary - mean_blp) >= 0.3*sd(initial_data1[[i]])){
      numeric_influencer <- c(numeric_influencer, var_name)
      cat(var_name)
      cat('\n')
      cat(mean_blp)
      cat('\n')
      cat(mean_ordinary)
      cat('\n')
      cat('\n')
    }
  }else{
    mode_level_blp <- getmode(droplevels(as.factor(BadLeveragePts[[i]])))
    mode_level_ordinary <- getmode(droplevels(as.factor(ordinaryPts[[i]])))
    if (as.character(mode_level_ordinary) != as.character(mode_level_blp)){
      categ_influencer_modeLevel <- c(categ_influencer_modeLevel, var_name)
      cat('Different Mode Level: ')
      cat(var_name)
      cat('\n')
      cat(mode_level_blp)
      cat('\n')
      cat(mode_level_ordinary)
      cat('\n')
      cat('\n')
    }
    
    levels_blp <- levels(droplevels(as.factor(BadLeveragePts[[i]])))
    levels_ordinary <- levels(droplevels(as.factor(ordinaryPts[[i]])))
    blp_unique_level <- setdiff(levels_blp, levels_ordinary)
    
    if (length(blp_unique_level) > 0){
      categ_influencer_uniqueLevel <- c(categ_influencer_uniqueLevel, var_name)
      cat('Different Number of Levels: ')
      cat(var_name)
      cat('\n')
      cat(blp_unique_level)
      cat('\n')
      cat('\n')
    }
  }
}
numeric_influencer
categ_influencer_modeLevel
categ_influencer_uniqueLevel

par(mfrow = c(1,2))
plot(BadLeveragePts$LotArea)
plot(ordinaryPts$LotArea)

par(mfrow = c(1,2))
plot(BadLeveragePts$OverallQual)
plot(ordinaryPts$OverallQual)

par(mfrow = c(1,2))
plot(BadLeveragePts$YearBuilt)
plot(ordinaryPts$YearBuilt)

par(mfrow = c(1,2))
plot(BadLeveragePts$MasVnrArea)
plot(ordinaryPts$MasVnrArea)

par(mfrow = c(1,2))
plot(BadLeveragePts$BsmtQual)
plot(ordinaryPts$BsmtQual)

par(mfrow = c(1,2))
plot(BadLeveragePts$KitchenAbvGr)
plot(ordinaryPts$KitchenAbvGr)    # Note: 2 blps have KitchenAbvGr at 1.4

par(mfrow = c(1,2))
plot(BadLeveragePts$TotRmsAbvGrd)
plot(ordinaryPts$TotRmsAbvGrd)

par(mfrow = c(1,2))
plot(BadLeveragePts$GarageYrBlt)
plot(ordinaryPts$GarageYrBlt)

par(mfrow = c(1,2))
plot(BadLeveragePts$GarageQual)
plot(ordinaryPts$GarageQual)

par(mfrow = c(1,2))
plot(BadLeveragePts$GarageCond)
plot(ordinaryPts$GarageCond)

par(mfrow = c(1,2))
plot(BadLeveragePts$SuspectOutlr)
plot(ordinaryPts$SuspectOutlr)

par(mfrow = c(1,2))
plot(BadLeveragePts$Neighborhood)
summary(BadLeveragePts$Neighborhood) # Note: A lot of blps have Neighborhood == OldTown, but very few in NAmes
plot(ordinaryPts$Neighborhood)
summary(ordinaryPts$Neighborhood)

par(mfrow = c(1,2))
plot(BadLeveragePts$Exterior1st)
plot(ordinaryPts$Exterior1st)        # Note: A lot of blps have Exterior1st == Wd Sdng, and blp has one unique AsphShn

par(mfrow = c(1,2))
plot(BadLeveragePts$Foundation)      # Note: A lot of blps have Foundation == BrkTil, and blp has three unique Wood
plot(ordinaryPts$Foundation)

par(mfrow = c(1,2))
plot(BadLeveragePts$BsmtFinType1)    # Note: Blps have a lot less BsmtFinType1 == GLQ
plot(ordinaryPts$BsmtFinType1)

par(mfrow = c(1,2))
plot(BadLeveragePts$YrSold)
plot(ordinaryPts$YrSold)

par(mfrow = c(1,2))
plot(BadLeveragePts$Remodeled)      # Note: Most blps have Remodeled == FALSE
plot(ordinaryPts$Remodeled)

par(mfrow = c(1,2))
plot(BadLeveragePts$SeasonSold)
plot(ordinaryPts$SeasonSold)

par(mfrow = c(1,2))
plot(BadLeveragePts$RoofMatl)       # Note: One blp has RoofMatl == ClyTile
plot(ordinaryPts$RoofMatl)

par(mfrow = c(1,2))
plot(BadLeveragePts$Exterior2nd)    # Note: Blps have a lot more Exterior2nd == Wd Sdng, and blp has one unique CBlock
plot(ordinaryPts$Exterior2nd)

par(mfrow = c(1,2))
plot(BadLeveragePts$Heating)        # Note: One blp has Heating == OthW
plot(ordinaryPts$Heating)

## Construct a new predictor SuspectBlp
for (i in 1:nrow(initial_data1)){
  initial_data1$SuspectBlp[i] <- sum(c(initial_data1$KitchenAbvGr[i] > 1.1, initial_data1$Neighborhood[i] == 'OldTown',
                                       initial_data1$Neighborhood[i] != 'NAmes', initial_data1$Exterior1st[i] == 'Wd Sdng',
                                       initial_data1$Foundation[i] == 'BrkTil', initial_data1$GarageType[i] != 'GLQ',
                                       initial_data1$Remodeled[i] == 'FALSE', initial_data1$RoofMatl[i] == 'ClyTile',
                                       initial_data1$Exterior2nd[i] == 'Wd Sdng', initial_data1$Heating[i] == 'OthW' ))
}

for (i in 1:nrow(test_data)){
  test_data$SuspectBlp[i] <- sum(c(test_data$KitchenAbvGr[i] > 1.1, test_data$Neighborhood[i] == 'OldTown',
                                       test_data$Neighborhood[i] != 'NAmes', test_data$Exterior1st[i] == 'Wd Sdng',
                                       test_data$Foundation[i] == 'BrkTil', test_data$GarageType[i] != 'GLQ',
                                       test_data$Remodeled[i] == 'FALSE', test_data$RoofMatl[i] == 'ClyTile',
                                       test_data$Exterior2nd[i] == 'Wd Sdng', test_data$Heating[i] == 'OthW' ))
}

summary(as.factor(initial_data1[BadLev_tf,]$SuspectBlp))
unique_Level <- (initial_data1$RoofMatl == 'ClyTile' | initial_data1$Exterior1st == 'AsphShn' |
                initial_data1$Exterior2nd == 'CBlock' | initial_data1$Foundation == 'Wood' |
                initial_data1$Heating == 'OthW')
#BlpRmv_tf <- BadLev_tf & initial_data1$SuspectBlp < 4 & !unique_Level
Rmv_tf <- (BadLev_tf | outlier_tf) & (initial_data1$SuspectBlp > 4 | initial_data1$SuspectOutlr > 2)
initial_data_Rmvd <- initial_data1[!Rmv_tf, ]



# # Refit the Model After Engineering Bad Leverage Points -------------------
# 
# lm5 <- lm(SalePrice~OverallQual + TotalSF + Neighborhood + 
#             OverallCond + RoofMatl + TotBathRms + LotArea + YearBuilt + 
#             Functional + TotalFloorSF + BsmtFinSF1 + Condition2 + SaleCondition + 
#             BsmtExposure + KitchenQual + BsmtFinType1 + Heating + GarageArea + 
#             Street + HasBsmt + OverallQual:Neighborhood + TotalSF:SaleCondition + 
#             OverallQual:YearBuilt + Neighborhood:YearBuilt + SeasonSold:YearBuilt + Foundation:YearBuilt +
#             MSSubClass:YearBuilt + TotalSF:Neighborhood + TotalSF:MSSubClass,
#           initial_data_Rmvd)
# summary(lm5)
# par(mfrow = c(2,2))
# plot(lm5)


# Make Prediction
pred_0314_t <- predict(lm4, test_data)
pred_0314 <- exp(pred_0314_t)
summary(pred_0314)
summary(initial_data$SalePrice)
write.csv(pred_0314, file = "pred_0314.csv")
