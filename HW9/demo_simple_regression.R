# interactive_regression() runs a regression simulation.
# Click on the plotting area to add points and see a corresponding regression line
#  (hitting ESC will stop the simulation). 
#
# You will also see three numbers: 
#   intercept – where the regression line intercepts the y-axis
#   regression coefficient – the slope of x on y
#   correlation - correlation of x and y.

interactive_regression <- function() {
  cat("Click on the plot to create data points; hit [esc] to stop")
  plot(NA, xlim=c(-5,50), ylim=c(-5,50))
  points = data.frame()
  repeat {
    click_loc <- locator(1)
    if (is.null(click_loc)) break
    if(nrow(points) == 0 ) {
      points <- data.frame(x=click_loc$x, y=click_loc$y)
    } else {
      points <- rbind(points, c(click_loc$x, click_loc$y))
    }
    plot(points, xlim=c(-5,50), ylim=c(-5,50), pch=19, cex=2, col="gray")
    if (nrow(points) < 2) next
    
    model <- lm(points$y ~ points$x)
    abline(model, lwd=2, col="cornflowerblue")
    text(1, 50, paste(c("Raw intercept: ", round(model$coefficients[1], 2)), collapse=" "))
    text(1, 45, paste(c("Raw slope    : ", round(model$coefficients[2], 2)), collapse=" "))
    text(1, 40, paste(c("Correlation  : ", round(cor(points$x, points$y), 2)), collapse=" "))
  }
  
  return(points)
}

