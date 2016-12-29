setwd("./simulations/xgb-iterations/")

## Load required packages
library('xgboost')
library('ggplot2')
library('viridis')
library('animation')

## Load utility functions
source("utils.R")

## Load and prepare data 
load("data.rda")
xtr <- data.matrix(subset(data.train, select = -class))
ytr <- as.numeric(data.train$class) - 1
xte <- data.matrix(subset(data.test, select = -class))
yte <- as.numeric(data.test$class) - 1
dtrain <- xgb.DMatrix(data = xtr, label = ytr)

## XGBoost parameters
xgb.params <- list(
   "booster"           = "gbtree",
   "eta"               = 5e-2,
   "max_depth"         = 3,
   "subsample"         = 1,
   "colsample_bytree"  = 1,
   "colsample_bylevel" = 1,
   "objective"         = "binary:logistic",
   "eval_metric"       = "logloss",
   "nthread"           = 4
)

## Simulate XGBoost iterations
setwd("../../")
setwd("./docs/simul/xgb-iterations/")
saveHTML({
   for(i in 1:50) {
      xgb.model <- xgb.train(data = dtrain, params = xgb.params, nrounds = i)
      g <- generateXGBDecisionBoundary(xgb.model, xte, yte, i)
      plot(g)
      print(i)
   }
}, single.opts = "'controls':['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin':0, 'utf8':false, 'theme':'light'",
interval=0.25, autoplay=FALSE, ani.width=700, ani.height=500, clean=TRUE, htmlfile="index.html", verbose = FALSE,
autobrowse=FALSE, title="", description="", img.name = "img", ani.dev = "jpeg", ani.type = "jpeg")

setwd("../../../")
