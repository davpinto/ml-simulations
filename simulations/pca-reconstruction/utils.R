## Plot Image
plotImage <- function(img, pcs, orig.dim){
   g <- ggplot(melt(t(img[nrow(img):1,])), aes(Var1,Var2, fill=value)) + 
      geom_raster() +
      scale_fill_gradientn(colours=gray(1:100/100), guide='none') +
      labs(title = paste('PCs =', pcs, 'of', orig.dim)) +
      labs(x=expression(x[1]), y=expression(x[2]))
   plot(g)
}