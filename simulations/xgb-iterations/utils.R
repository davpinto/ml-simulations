## Plot classification decision boundary
generateXGBDecisionBoundary <- function(model, x, y, iter)
{
   # Resample Data
   n  <- 125
   x1 <- seq(from=min(x[,1])-0.1*diff(range(x[,1])), to=max(x[,1])+0.1*diff(range(x[,1])), length=n)
   x2 <- seq(from=min(x[,2])-0.1*diff(range(x[,2])), to=max(x[,2])+0.1*diff(range(x[,2])), length=n)
   data.points <- data.frame(x1=x[,1], x2=x[,2], y=factor(y, levels = c(0,1)))
   
   # Predict probabilities
   xte <- cbind(rep(x1, times=length(x2)), rep(x2, each=length(x1)))
   dtest <- xgb.DMatrix(data = xte)
   y.hat <- predict(model, dtest)
   data.contour <- data.frame(x1=xte[,1], x2=xte[,2], y=y.hat)
   
   # Plot Contour
   g <- ggplot() + 
      geom_tile(data=data.contour, aes(x=x1,y=x2,fill=y), alpha = 0.8) +
      scale_fill_viridis(guide = "none", begin = 0.1, end = 0.9, 
                         option = "plasma") +
      stat_contour(data=data.contour, aes(x=x1, y=x2, z=as.numeric(y>0.5)), 
                   color='white', alpha=0.5, size = 1, bins=1) +
      geom_point(data=data.points, aes(x=x1,y=x2,color=factor(y)), size=3, 
                 alpha = 0.8) +  
      scale_color_manual(guide='none', values=c('black', 'white')) +
      labs(x=expression(x[1]), y=expression(x[2]),
           title=paste("Iteration:", iter))
   
   return( g )
}
