## Plot image
plotImage <- function(imgRGB, k, kcol){
   g <- ggplot(data=imgRGB, aes(x=x, y=y)) + 
      geom_point(colour = kcol) +
      labs(title=paste('k = ', k, sep='')) +
      labs(x=expression(x[1]), y=expression(x[2]))
   
   plot(g)
}
