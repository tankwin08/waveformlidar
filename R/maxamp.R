#' maxamp
#'
#' The function allows you to find the maximum amplitude of waveform above the ground (not the whole waveform) if this waveform come from vegetation with more than 1 peaks. Otherwise it will
#' give the last peak's corresponding intensity. The identified maximum intensity can be used to calculate the height from waveform ending or waveform begining.
#'
#' @param y is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param smooth is tell whether you want to smooth the waveform to reduce the effect of some obvious noise. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.2.
#' @param width width of moving window.Default is 3, must be integer between 1 and n.This parameter ONLY work when the smooth is TRUE.

#' @return return the largest amplitude above the ground for the waveform with more than two peaks and ground amplitude for the waveform with one peak.
#' @importFrom caTools runmean
#' @export
#' @examples
#'
#'data(return)
#'x<- return[1,]
#'
#'rx<- maxamp(x)
#'###for more complicated waveforms
#'x1<- return[182,]
#'rx1<- maxamp(x1)



##########find the maximum ampilitude above 3m above the the last echo
maxamp<-function(y,smooth = TRUE,thres=0.2,width=3){
  y<-as.numeric(unlist(y))
  y[y==0]<-NA
  #y<-y[-c(1:11)]
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if (smooth == TRUE){
    y<-runmean(y,width,"C")
  }
  #####get the intial parameter for the waveform, it can be assumed as the prior parameters
  peakrecord<-lpeak(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-y[peaknumber]>thres*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  #groindex<-realind[length(realind)]
  newpeak<-y[realind]  #collect intensity
  if (length(realind)>1){
    maxam<-max(newpeak[1:length(newpeak)-1],na.rm=T);
  } else {
    maxam=imax
  }
  return (maxam)
}
