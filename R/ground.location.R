#' ground.location
#'
#' The function allows you to identify the possible ground location (time index) in the waveform. Generally, we assume the last echo or peak corresponding to the ground.
#'
#' @param y is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param smooth is tell whether you want to smooth the waveform to reduce the effect of some obvious noise. Default is TRUE.
#' @param rescale is to determine whether you want to rescale the waveform intensity or not. Here we used the minimum intensity of each waveform to conduct rescaling.
#'        Default is using rescaling.
#' @param thres is to determine if the detected peak is the real peak. The real peak's intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width the width of moving window for smoothing.Default is 3, must be integer between 1 and n.This parameter ONLY work when the smooth is TRUE.
#' @param top is to tell whether we calculate the ground time location from the top (where waveform starts or canopy) or from the bottom (where the waveform ends). Default is from the top.
#' @return return the index of possible ground position of waveform.
#' @importFrom caTools runmean
#' @export
#' @examples
#'
#'data(return)
#'x<-return[125,]
#'#default
#'ground.location(x)
#'


ground.location<-function(y,smooth=TRUE,rescale=TRUE,thres=0.2,width=3,top = TRUE){
  y<-as.numeric(unlist(y))
  y[y==0]<-NA
  #y<-y[-c(1:11)]
  ###when for direct decomposition
  if (rescale) y<-y-min(y,na.rm = T)+1
  if (smooth==TRUE){
    y<-runmean(y,width,"C")
  }
  #####get the intial parameter for the waveform, it can be assumed as the prior parameters
  peakrecord<-lpeak(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-y[peaknumber]>thres*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  groind<-realind[length(realind)]
  if(!top) groind <- wavelen(y) - groind + 1
  return (groind)
}
