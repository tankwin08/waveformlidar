#' fslope
#'
#' The function allows you calculate the (1) front slope angle (FS, the angle from waveform beginning to the first peak
#' which is assumed to be canopy returns) and (2)roughness of outermost canopy (ROUGH, the distnace from the waveform beginning to the first peak).
#'
#' @param y is a waveform with only intensities.
#' @param smooth is tell whether you want to smooth the waveform to reduce the effect of some obvious noise. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width width of moving window.Default is 3, must be integer between 1 and n.This parameter ONLY works when the smooth is TRUE.
#' @param tr the temporal resolution of waveform.Default is 1 ns. Must be integer from 1 to n.
#' @return return the front slope angle (FS, The angle from waveform beginning to the first peak which is assumed to be canopy returns)
#'   and roughness of outermost canopy (ROUGH, the distnace from the waveform beginning to the first peak).
#' @importFrom caTools runmean
#' @export
#' @examples
#'
#' data(return)
#' ###for the one peak, this function returns not useful results.
#' #This function more related to vegetation and canopy.
#' y<-return[1,]
#' #default
#' yr<-fslope(y)
#'
#' yy<-return[182,]
#' yyr<-fslope(yy)


fslope<-function(y,smooth=TRUE,thres=0.22,width=5,tr=1){
  y0<-as.numeric(unlist(y))
  y0[y0==0]<-NA
  #y<-y[-c(1:11)]
  ###when for direct decomposition
  y0<-y0-min(y0,na.rm = T)+1
  y<-y0
  if (smooth==TRUE){
    y<-runmean(y,width,"C")  ###when we identify the peaks, maybe we should try both smooth and non-smooth functions to determine which one is better
  }

  #####get the intial parameter for the waveform, it can be assumed as the prior parameters
  peakrecord<-lpeak(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-y[peaknumber]>thres*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  newpeak<-y0[realind]  #collect intensity
  #fs<-newpeak[1]/realind[1]
  ###to identify the first na value
  beginid<-which(y0>=1)[1]
  fs1<-(newpeak[1]-y0[beginid])/(realind[1]-beginid)
  #if (type=="r"){
  #slope<-atan(fs)*180/pi  ###here we need to convert to degree
  #rough<-realind[1]
  #}else{
  slope<-atan(fs1)*180/pi
  rough<-realind[1]-beginid
  #}
  return(c(slope,rough*0.15*tr))
}
