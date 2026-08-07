#' Bin fluorescence intensity spectrum data (.csv) from ImageJ
#'
#' @param dirlist A list containing the directory of all fluorescence intensity data in .csv format.
#' @param group An integer object assigning group index for all listed directory. This could be such that repeats of the same treatment being grouped together.
#' @param num.bin An integer. Number of bins you wish to have. Default is 50.
#' @param show.process Logical. Decide whether the file reading process would be shown in Console. Useful when there is bug in a file.
#' @returns Return a list containing all the data with additional column including binning information.
#' @export

databin <- function(dirlist, group, num.bin = 50, show.process = FALSE){

  ## Reading data
  data.list <- list()
  for(i in 1:length(dirlist)){
    dir <- dirlist[i]
    if(show.process == TRUE){
      print(paste('Processing file #', i, ', with directory:', dir))
    }
    data <- read.csv(dir)
    colnames(data) <- c('x', 'y')
    data$x <- data$x / max(data$x)
    data$gp <- i
    data.list[[i]] <- data
  }

  #group <- c(1,1,1,1,1,1,2,2,2,2,2,2,2)
  data.list.gp <- list()
  for(i in 1:max(group)){
    sublist <- data.list[group == i]
    subdata <- as.matrix(t(rep(NA, 3)))
    for(j in 1:length(sublist)){
      subdata <- rbind(subdata, as.matrix(sublist[[j]]))
    }
    subdata <- subdata[-1, ]
    subdata <- as.data.frame(subdata)
    subdata$expt <- i
    subdata <- sort(subdata)
    colnames(subdata)[3] <- 'Repeat'
    subdata$Repeat <- as.factor(subdata$Repeat)
    data.list.gp[[i]] <- subdata
  }

  # Assign binning
  #num.bin <- 50
  data.binned <- list()
  for(i in 1:length(data.list.gp)){
    subdata <- data.list.gp[[i]]
    subdata.tile <- subdata %>%
      ntile(n = num.bin)
    subdata$bin <- subdata.tile
    data.binned[[i]] <- subdata
  }

  return(data.binned)

}
