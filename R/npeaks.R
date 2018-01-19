#' npeaks: count the number of waveform compoments
#'
#' The function allows you to briefly know how many peaks you have in a waveform
#'
#' @param y is the waveform intensities.
#' @param drop is the index of waveform index we should ingore or non-intensity of waveform information.Default is c(0,0) that means use the all input data.
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param threshold is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.2.
#' @return return the number of waveform componments.
#' @importFrom caTools runmean
#' @export
#' @examples
#'
#' ##import return waveform data
#' data(return)
#' ###individual waveform
#'   x<-return[1,]
#'   npeaks(x)
#'   npeaks(x,smooth="FALSE") ##it will use the raw data to detect peaks
#'   npeaks(x,smooth="FALSE",threshold=0.25) ###you can set up threshold to determine if peaks are correctly identified.
#' ###if there are some columns are not intensity, you can delete before you process
#'    y<-c(c(1,2,3),as.numeric(x))
#'    npeaks(y,drop=c(1,3))
#'




npeaks<-function(y,drop=c(0,0),smooth="TRUE",threshold=0.2){
  y<-as.numeric(unlist(y))
  y[y==0]<-NA
  if (drop[1]>0){
    y<-y[-c(drop[1]:drop[2])]}
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if(smooth=="TRUE"){
    y<-runmean(y,3,"C")  ###when we identify the peaks, maybe we should not use the smooth function
  }
  #####get the intial parameter for the waveform, it can be assumed as the prior parameters
  peakrecord<-lpeak(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-y[peaknumber]>threshold*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  newpeak<-y[realind]  #collect intensity
  z<-length(realind)
  #return(y)
  return (z)
}
