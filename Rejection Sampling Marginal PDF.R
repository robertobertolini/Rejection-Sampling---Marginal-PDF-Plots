# Rejection sampling

set.seed(6) # Set for reproducibility

library(cowplot)

################################################# Square Region

Square <- function(){
  
  df <- data.frame(X=numeric(),Y=numeric()) # Create an empty data frame for our 
  # random variables X and Y
  ContinueIteration <- FALSE # In case our points do not lie in the region, we want 
  # the program to reject this #sample and resample again
  for (i in 1:1000){
    while (!ContinueIteration){
      x <- runif(1,-6,6) # Sample one observation at a time from a uniform distribution
      y <- runif(1,-6,6) # Sample one observation at a time from a uniform distribution
      check <- (1/144)*((-6 <= x & (x <= 6)) & (-6 <= y & (y <= 6))) # This line checks 
      # that our point lies
      # within the domain. If it does not lie in the domain, this line is 0 
      # and this point is rejected.
      if (check!=0){
        break
      }
      else{
      }
    }
    df[i,] = c(x,y) # Add the results to the dataframe
  }
  
  # Histogram for X
  xhist <- hist(df$X,breaks=12,axes = FALSE,main='',xlab='',ylab='',xaxt='n',yaxt='n',
                plot=FALSE)
  # Histogram for Y
  yhist <- hist(df$Y,breaks=12,axes = FALSE,main='',xlab='',ylab='',xaxt='n',yaxt='n',
                plot = FALSE)
  
  
  layout(mat = matrix(c(2, 1, 0, 3),nrow = 2, ncol = 2),heights = c(1, 4), 
         widths = c(2, 1))
  # Height of the two rows and height of the two columns are specified. 
  
  # Plot the scatterplot  
  par(mar = c(5, 4, 0, 0))
  plot(df)
  
  # Plot the Marginal of X
  
  par(mar = c(0,4, 0, 0))
  barplot(xhist$density,axes=FALSE,space=0)
  
  # Plot the Marginal of Y
  
  par(mar=c(5, 0, 0, 5))
  barplot(yhist$density,axes=FALSE,space=0, horiz=TRUE)
  
}

################################################# Triangular Region

# Triangle

Triangle <- function(){
  
  df <- data.frame(X=numeric(),Y=numeric()) # Create an empty data frame for our 
  # random variables X and Y
  ContinueIteration <- FALSE # In case our points do not lie in the region, we want 
  # the program to reject this 
  #sample and resample again
  for (i in 1:1000){
    while (!ContinueIteration){
      x <- runif(1,-6,6) # Sample one observation at a time from a uniform distribution
      y <- runif(1,-6,6) # Sample one observation at a time from a uniform distribution
      check <- (1/72)*((y <= x)) # This line checks 
      # that our point lies
      # within the domain. If it does not lie in the domain, this line is 0 
      # and this point is rejected.
      if (check!=0){
        break
      }
      else{
      }
    }
    df[i,] = c(x,y) # Add the results to the dataframe
  }
  # Histogram for X
  xhist <- hist(df$X,breaks=12,axes = FALSE,main='',xlab='',ylab='',xaxt='n',yaxt='n',
                plot=FALSE)
  # Histogram for Y
  yhist <- hist(df$Y,breaks=12,axes = FALSE,main='',xlab='',ylab='',xaxt='n',yaxt='n',
                plot = FALSE)
  
  layout(mat = matrix(c(2, 1, 0, 3),nrow = 2, ncol = 2),heights = c(1, 4), 
         widths = c(2, 1))
  
  # Height of the two rows and height of the two columns are specified.
  
  # Plot the scatterplot  
  par(mar = c(5, 4, 0, 0))
  plot(df)
  
  # Plot the Marginal of X
  par(mar = c(0,4, 0, 0))
  barplot(xhist$density,axes=FALSE,space=0)
  
  # Plot the Marginal of Y
  par(mar=c(5, 0, 0, 5))
  barplot(yhist$density,axes=FALSE,space=0, horiz=TRUE)
  
}


################################################# Circular Region

Circle <- function(){
  
  df <- data.frame(X=numeric(),Y=numeric()) #  Create an empty data frame for our 
  # random variables X and Y
  ContinueIteration <- FALSE # In case our points do not lie in the region, we want 
  # the program to reject this 
  #sample and resample again
  for (i in 1:1000){
    while (!ContinueIteration){
      x <- runif(1,-6,6) # Sample one observation at a time from a uniform distribution
      y <- runif(1,-6,6) # Sample one observation at a time from a uniform distribution
      check <- (1/(36*pi))*((x^2 + y^2 <= 36)) # This line checks that our point lies
      # within the domain. If it does not lie in the domain, this line is 0 
      # and the point is rejected.
      if (check!=0){
        break
      }
      else{
      }
    }
    df[i,] = c(x,y)
  }
  # Histogram for X
  xhist <- hist(df$X,breaks=12,axes = FALSE,main='',xlab='',ylab='',xaxt='n',yaxt='n',
                plot=FALSE)
  # Histogram for Y
  yhist <- hist(df$Y,breaks=12,axes = FALSE,main='',xlab='',ylab='',xaxt='n',yaxt='n',
                plot = FALSE)
  
  # Height of the two rows and height of the two columns are specified.
  
  layout(mat = matrix(c(2, 1, 0, 3),nrow = 2, ncol = 2),heights = c(1, 4), 
         widths = c(2, 1))
  
  # Plot the scatterplot
  par(mar = c(5, 4, 0, 0))
  plot(df)
  
  # Plot the Marginal of X
  
  par(mar = c(0,4, 0, 0))
  barplot(xhist$density,axes=FALSE,space=0)
  
  # Plot the Marginal of Y
  
  par(mar=c(5, 0, 0, 5))
  barplot(yhist$density,axes=FALSE,space=0, horiz=TRUE)
  
}

## Plot Results
Square()
Triangle()
Circle()
