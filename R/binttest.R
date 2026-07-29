#' Conduct t-test within bin for each bin
#' @param data1 A dataframe. Binned data (Data containing bin information) to be tested
#' @param data2 A dataframe. Binned data (Data containing bin information) to be tested
#' @param siglevel A numeric. Level of p-value to define significance. Default being 0.05
#' @returns Returns a dataframe containing whether significant difference is observed for each bin.
#' @export

binttest <- function(data1, data2, siglevel = 0.05){

  num.bin <- length(unique(data1$bin))
  pval <- as.matrix(rep(NA, num.bin))
  for(i in 1:num.bin){
    y1 <- data1$y[data1$bin == i]
    y2 <- data2$y[data2$bin == i]
    ttest <- t.test(y1,y2)
    pval[i] <- ttest$p.value
  }
  pval <- as.data.frame(pval)
  pval$label <- NA
  pval$label[pval$V1 <= siglevel] <- '*'

  return(pval)

}
