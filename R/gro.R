#' gro: find the ground location in a waveform (assume the last echo or peak corresponding to the ground).
#'
#' The function allows you to identify the possible ground location (index) in the waveform.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width width of moving window.Default is 3, must be integer between 1 and n.This parameter ONLY work when the smooth is TRUE.
#' @return return the index of possible ground position of waveform.
#' @importFrom caTools runmean
#' @export
#' @examples
#'
#'data(return)
#'x<-return[1,]
#'#default
#'gro(x)
#'


gro<-function(y,smooth="TRUE",thres=0.2,width=3){
  y<-as.numeric(unlist(y))
  y[y==0]<-NA
  #y<-y[-c(1:11)]
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if (smooth=="TRUE"){
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

  return (groind)
}
