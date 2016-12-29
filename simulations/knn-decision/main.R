## Load required packages
library('fastknn')
library('ggplot2')
library('gridExtra')
library('animation')

## Load data
data("spirals", package = "fastknn")
x <- data.matrix(spirals$x)
y <- spirals$y

## Split data for training and test
set.seed(2020)
tr.idx <- which(fastknn:::createCVFolds(y, n = 2) == 1)
xtr <- x[tr.idx,]
ytr <- y[tr.idx]
xte <- x[-tr.idx,]
yte <- y[-tr.idx]

## Simulate KNN decision
setwd("./docs/simul/knn-decision/")
saveHTML({
   for(i in seq(1, 100, by = 2)) {
      g1 <- knnDecision(xtr, ytr, xte, yte, k = i, method = "vote", dpi = 125) +
         labs(title = paste("k =", i), subtitle = "Voting Estimator")
      g2 <- knnDecision(xtr, ytr, xte, yte, k = i, method = "dist", dpi = 125) +
         labs(title = "", subtitle = "Weighted Voting Estimator")
      g <- grid.arrange(g1, g2, ncol = 2)
      plot(g)
      print(i)
   }
}, single.opts = "'controls':['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin':0, 'utf8':false, 'theme':'light'",
interval=0.25, autoplay=FALSE, ani.width=700, ani.height=500, clean=TRUE, htmlfile="index.html", verbose = FALSE,
autobrowse=FALSE, title="", description="", img.name = "img", ani.dev = "jpeg", ani.type = "jpeg")

setwd("../../../")
