#' Extract average-around-peak information from wavy data
#'
#' Given a wavy data with two columns, we extract the information of data
#' around their peaks. This is useful for root confocal microscopy when the wavy
#' data is the pixel intensity profile extracted along a line/piecewise line
#' segment, and the fluorescent sensor you use is a nuclear-based sensor.
#' In this case, attracting the average around a peak actually gives you an
#' approximation of average nuclear signal intensity. This could capture the
#' variation in data more accurately than directly using smoothening tools over
#' the intensity curve.
#' In this script, we first search for peaks and troughs in data by using the
#' assistant function `searchPeaks`. Have a look at its introduction for more
#' details. We then apply trapezoidal integration around the peak to get the
#' local mean information
#'
#' @param data Data frame contains two columns, one being your x-axis independent variable, one being your y-axis dependent variable
#' @param w Integer. The half-width of the maximum filter applied to data. See `searchPeaks` for details.
#' @param plot Logical. Whether or not to plot the resulting graph showing the found maximum points. See `searchPeaks` for details.
#' @param span Numeric. Span for loess smoothing of data. See details in description for `loess`.
#' @param ... Allows addition of more constraint of loess smoothing, which is directly applied in `searchPeaks`
#' @returns A data frame containing two columns:
#' * `peak`. The x-coordinates of peaks found in the wavy data.
#' * `mean`. The mean value of the peak bounded by the two nearest troughs. Trapezoidal integration was applied here to catch the integration of the wavy data around the peak.
#' @export

peakMean <- function(data, w = 10, plot = F, span = 0.03, ...){

require(zoo)
require(graphics)
require(pracma)

# Concatenate data format
colnames(data) <- c('x', 'y')
data$x <- data$x/max(data$x)

# Calculate peaks and troughs using searchPeaks
peaks <- searchPeaks(data$x,data$y, w = w, plot = plot, span = span, ...)
troughs <- searchPeaks(data$x, -data$y, w = w, plot = plot, span = span, ...)

# Find the nearest two troughs near a peak.
lb <- findInterval(peaks$x, troughs$x) # leftmost bound
rb <- lb + 1L

if(lb[1] == 0){
  left <- c(0, troughs$x[lb])
}else{
  left <- troughs$x[lb]
}

right <- c(troughs$x[rb])
if(rb[length(rb)] == 1 + length(troughs$x)){
  right[length(right)] <- 1
}

interval <- data.frame(left = left, right = right)

# A checkpoint
if(!(length(interval$left) == length(peaks$x))){
  print('Error: Number of peaks detected is not consistent with the number of intervals. Double check the settings.')
}

# Numerically integrate and find average at each point
peak.value<- data.frame(matrix(NA, nrow = length(peaks$x), ncol = 2))
peak.value[ , 1] <- peaks$x
for(i in 1:length(interval$left)){
  peak.data <- subset(data, data$x >= interval$left[i] & data$x <= interval$right[i])
  peak.value[i,2] <- pracma::trapz(peak.data$x, peak.data$y)/(interval$right[i] - interval$left[i])
}
colnames(peak.value) <- c('peak', 'mean')

return(peak.value)

}

#plot(peak.value$peak, peak.value$mean, type = 'l')
