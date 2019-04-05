#' which.half
#'
#' The function allows you to identify the half total energy position (time location) which can be used to calculate the height from waveform ending or waveform begining.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function.
#' @param rescale is to determine whether you want to rescale the waveform intensity or not. Here we used the minimum intensity of each waveform to conduct rescaling.
#'        Default is using rescaling.
#' @return return the index of half total energy position of waveform.
#' @export
#' @examples
#'
#'data(return)
#'x<-return[1,]
#'#default
#'which.half(x)
#'#NOT USE rescale, the result will be a bit different
#'half_pos <- which.half(x,rescale=FALSE)
#'##the distnace from half position to the waveform ending is
#'dis_end = (wavelen(x)-half_pos)*0.15
#'dis_begin = half_pos*0.15 ##here 0.15 is the 1ns distance.


which.half<-function(x, rescale = TRUE){
  x<-as.numeric(x);x[x==0]<-NA
  if (rescale == TRUE){
    x<- x - min(x,na.rm=TRUE)
  }
  rsum<-sum(x,na.rm=TRUE);x[is.na(x)]<-0  ###we need to assign NA them as zero,otherwise cannot use the cumsum
  ind<-which(cumsum(x)>rsum/2)[1]
  return(ind)
}
