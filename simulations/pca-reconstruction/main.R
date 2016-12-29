setwd("./simulations/pca-reconstruction/")

## Load required packages
library('jpeg')
library('ggplot2')
library('scales')
library('reshape2')
library('gridExtra')
library('animation')

## Load utility functions
source("utils.R")

## Read Image
img <- readJPEG("image.jpg");

## Convert to gray scale
img <- 0.2126 * img[,,1] + 0.7152 * img[,,2] + 0.0722 * img[,,3]

# Compress Image
svd.model    <- svd(img);
img.values   <- diag(svd.model$d);
img.loadings <- svd.model$v[,is.finite(img.values)&&img.values>0];

# Principal component projection
setwd("../../")
setwd("./docs/simul/pca-reconstruction/")
saveHTML({
   for(i in seq(2, 128, by = 2))
   {
      n.pcs         <- i
      pcs           <- img.loadings[,1:n.pcs]
      img.projected <- img%*%tcrossprod(pcs)
      plotImage(img.projected, i, 1024)
      
      print(i)
   }
}, single.opts = "'controls':['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin':0, 'utf8':false, 'theme':'light'",
interval=0.25, autoplay=FALSE, ani.width=700, ani.height=500, clean=TRUE, htmlfile="index.html", verbose=FALSE,
autobrowse=FALSE, title="", description="", img.name = "img", ani.dev = "jpeg", ani.type = "jpeg")

setwd("../../../")
