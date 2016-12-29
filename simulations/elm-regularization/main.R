setwd("./simulations/elm-regularization/")

## Load required packages
library('ggplot2')
library('viridis')
library('animation')

## Load utility functions
source("utils.R")

## Regularization path
lambda <- 2 ^ seq(from=-20, to=15, length=50)

## Load data 
load("data.rda")
x <- data.matrix(subset(data.train, select = -class))
y <- as.matrix(as.numeric(data.train$class) - 1)

## Simulate regularization effect on decision boundary
setwd("../../")
setwd("./docs/simul/elm-regularization/")
saveHTML({
   for(i in seq_along(lambda)) {
      elm.model <- trainELM(x, y, 150, lambda[i])
      xt <- data.matrix(subset(data.test, select = -class))
      yt <- as.matrix(as.numeric(data.test$class) - 1)
      g  <- generateELMDecisionBoundary(elm.model,xt,yt)
      plot(g)
      print(i)
   }
}, single.opts = "'controls':['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin':0, 'utf8':false, 'theme':'light'",
interval=0.25, autoplay=FALSE, ani.width=700, ani.height=500, clean=TRUE, htmlfile="index.html", verbose = FALSE,
autobrowse=FALSE, title="", description="", img.name = "img", ani.dev = "jpeg", ani.type = "jpeg")

setwd("../../../")