setwd("./simulations/kmeans-segmentation/")

## Load required packages
library('jpeg')
library('ggplot2')
library('animation')

## Load utility functions
source("utils.R")

# Read image
img <- readJPEG("image.jpg")

# Assign RGB channels to data.frame
imgDm  <- dim(img)
imgRGB <- data.frame(
   x = rep(1:imgDm[2], each = imgDm[1]),
   y = rep(imgDm[1]:1, imgDm[2]),
   R = as.vector(img[,,1]),
   G = as.vector(img[,,2]),
   B = as.vector(img[,,3])
)

# K-means clustering
setwd("../../")
setwd("./docs/simul/kmeans-segmentation/")
saveHTML({
   for(i in 2:51)
   {
      kClusters <- i
      kMeans    <- kmeans(imgRGB[, c("R", "G", "B")], centers=kClusters)
      kColours  <- rgb(kMeans$centers[kMeans$cluster,])
      plotImage(imgRGB, kClusters, kColours)
      
      print(i)
   }
}, single.opts = "'controls':['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin':0, 'utf8':false, 'theme':'light'",
interval=0.35, autoplay=FALSE, ani.width=700, ani.height=500, clean=TRUE, htmlfile="index.html", verbose = FALSE,
autobrowse=FALSE, title="", description="", img.name = "img", ani.dev = "jpeg", ani.type = "jpeg")

setwd("../../../")
