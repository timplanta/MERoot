#' Smooth noisy data and search for its peaks
#'
#' This code is modified originally from William S Huber's post on Cross
#' Validation. The idea is based on that filtering data using a maximum filter
#' could widen the peak in the data, so when overlaying a smoothened version of
#' noisy data, the peak in the smoothened data and the filtered data would
#' coincide. Please see following pages for details:
#' https://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset;
#' https://rpubs.com/mengxu/peak_detection.
#'
#' @param x Independent variable for the data
#' @param y Dependent variable for the data
#' @param w Integer. The half-width of the maximum filter applied to data
#' @param plot Logical. Whether or not to plot the resulting graph showing the found maximum points.
#' @param span Numeric. Span for loess smoothing of data. See details in description for `loess`
#' @param ... Allows addition of more constraint of loess smoothing.
#' @returns A list containing peak location information. It contains following elements:
#' * `x`. A numeric vector containing all local peaking point.
#' * `i`. An integer vector containing indices of all peak location.
#' * `y.hat`. A numeric vector containing the smoothening output of loess.
#' @export
searchPeaks <- function(x, y, w=10, plot = FALSE, span = 0.03, ...) {
  require(zoo)
  require(graphics)

  n <- length(y)
  y.smooth <- loess(y ~ x, span = span, ...)$fitted
  y.max <- zoo::rollapply(zoo::zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  peaks <- list(x=x[i.max], i=i.max, y.hat=y.smooth)


  if(plot){
    plot(x, y, cex=0.75, col="Gray", type = 'l', main=paste("w = ", w, ", span = ", span, sep=""))
    lines(x, peaks$y.hat, lwd=2)
    y.min <- min(y)
    sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]), col="Red", lty=2))
    points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)

  }
  return(peaks)

}
