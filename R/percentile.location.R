#' percentile.location
#'
#' The function allows you to calculate the time location position of energy percentile based on the given quantile within a waveform.
#' With this information. In addition, you can calculate the energy percentile heights.
#' Here, the percentile will be calculated from the top (more likely the canopy part) by defalut.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param quan is the quantile of energy you are interested in. Defalut is seq(0.5,1,0.1).
#' @param rescale is to determine whether you want to rescale the waveform intensity or not. Here we used the minimum intensity of each waveform to conduct rescaling.
#'        Default is using rescaling.
#' @param top is to tell whether we calculate the percentile from the top (where waveform starts or canopy) or from the bottom (where the waveform ends). Default is from the top.
#' @return return the index or position of corressponding quantile energy of waveform starting from the top.
#' @export
#' @examples
#'
#'data(return)
#'x<-return[1,]
#'#default
#'percentile.location(x)
#'
#'#change the quantile
#'qr<-seq(0.45,0.99,0.05)
#'re1<-percentile.location(x,quan=qr)
#'###rescale affects your results
#'re2<-percentile.location(x,quan=qr,rescale = FALSE)
#'##after you get the index, you can convert them to the height
#'#based on temporal resolution or georeference information of this waveform.


percentile.location<-function(x,quan=seq(0.5,1,0.1),rescale = TRUE, top = TRUE){
  x<- as.numeric (x)
  x[x==0]<-NA
  if (rescale == TRUE){
    x<- x - min(x,na.rm=TRUE)+1
  }
  rsum<-sum(x,na.rm=TRUE);x[is.na(x)]<-0  ###we need to assign NA them as zero,otherwise cannot use the cumsum

  qsum<-rsum*quan
  if (top) {
    csum<-cumsum(x)
    sind<-sapply(csum, function(z) z>=qsum)
    ind<- apply(sind,1, function(z) which(z==TRUE)[1])
  } else {
    x<-rev(x)
    x<-x[x!=0]
    csum<-cumsum(x)
    sind<-sapply(csum, function(z) z>=qsum)
    ind<- apply(sind,1, function(z) which(z==TRUE)[1])
    #ind<- length(x) - ind0 +1
  }
  return(ind)
}


