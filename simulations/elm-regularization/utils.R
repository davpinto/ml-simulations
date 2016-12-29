## Training Procedure
trainELM <- function(x,y,k,lambda)
{
   # Constants
   max.weight <- 5;
   
   # Insert Bias Column
   x <- cbind( 1, x );
   
   # Randon Input Weights
   set.seed(2)
   W <- replicate( k, runif(ncol(x), -max.weight, max.weight) );
   
   # Hidden Layer Output
   H <- tanh( x%*%W );
   H <- cbind( 1, H );
   
   # Estimate Coefficients
   G <- crossprod(H) + lambda*diag(c(0,rep(1,k)));
   Beta <- chol2inv(chol(G))%*%crossprod(H,y);
   
   # PRESS
   Hat   <- tcrossprod(H%*%chol2inv(chol(G)),H);
   PRESS <- sum( ( (y-Hat%*%y)/(1-diag(Hat)) )^2 );
   
   return(list(w=W,beta=Beta,l=lambda,press=PRESS))
}

## Testing Procedure
testELM <- function(model, x)
{
   # Hidden Layer Outputs for the test samples
   x <- cbind( 1, x );
   H <- tanh( x%*%model$w );
   H <- cbind(1,H);
   
   # Estimate Output
   y.hat <- H%*%model$beta;
   
   # Normalize Outputs
   y.hat.neg <- y.hat[y.hat<=0.5];
   y.hat.neg <- (y.hat.neg-min(y.hat.neg))/diff(range(y.hat.neg))-0.8;
   y.hat.pos <- y.hat[y.hat>0.5];
   y.hat.pos <- (y.hat.pos-min(y.hat.pos))/diff(range(y.hat.pos))+0.8;
   y.hat[y.hat<=0.5] <- y.hat.neg;
   y.hat[y.hat>0.5]  <- y.hat.pos;
   
   return( y.hat )
}

## Plot classification decision boundary
generateELMDecisionBoundary <- function(model, x, y)
{
   # Resample Data
   n  <- 125
   x1 <- seq(from=min(x[,1])-0.1*diff(range(x[,1])), to=max(x[,1])+0.1*diff(range(x[,1])), length=n)
   x2 <- seq(from=min(x[,2])-0.1*diff(range(x[,2])), to=max(x[,2])+0.1*diff(range(x[,2])), length=n)
   data.points <- data.frame(x1=x[,1], x2=x[,2], y=factor(y, levels = c(0,1)))
   
   # Estimate Output
   y.hat <- testELM( model, cbind(rep(x1, times=length(x2)), rep(x2, each=length(x1))) )
   data.contour <- data.frame(x1=rep(x1, times=length(x2)), x2=rep(x2, each=length(x1)), y=y.hat)
   
   # Plot Contour
   lb.lambda <- paste("lambda == ", format(model$l, nsmall=8, digits=3), sep='')
   sh.value <- bquote(lambda == .(model$l));
   g <- ggplot() + 
      geom_tile(data=data.contour, aes(x=x1,y=x2,fill=y), alpha = 0.8) +
      scale_fill_viridis(guide = "none", begin = 0.1, end = 0.9) +
      stat_contour(data=data.contour, aes(x=x1, y=x2, z=as.numeric(y>0.5)), 
                   color='white', alpha=0.5, size = 1, bins=1) +
      geom_point(data=data.points, aes(x=x1,y=x2,color=factor(y)), size=3, 
                 alpha = 0.8) +  
      scale_color_manual(guide='none', values=c('white', 'black')) +
      labs(x=expression(x[1]), y=expression(x[2]),
           title=sh.value)
   
   return( g )
}
