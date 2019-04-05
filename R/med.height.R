#' med.height
#'
#' The function allows you to calculate the height of half total energy above ground (not total waveform) and height proportion at the half energy position.
#' If you are interested in variables from the total waveform, you can use which.half or percentile.location functions.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param smooth is tell whether you want to smooth the waveform to reduce the effect of some obvious noise. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width width of moving window.Default is 3, must be integer between 1 and n.This parameter ONLY work when the smooth is TRUE.
#' @param tr the temporal resolution of waveform.Default is 1 ns. Must be integer from 1 to n.
#' @return return the height of half total energy above the ground and
#'         the height proportion of the first half (from waveform begining to the half total energy position).
#' @importFrom caTools runmean
#' @export
#' @examples
#'
#'data(return)
#'x<-return[1,]
#'#default
#'med.height(x)  ###for one peak waveform, it generally not make too much sense
#'xx<-return[182,]
#'med.height(xx)  ##this can help to charcterize the waveform




med.height<-function(x, smooth=TRUE,thres=0.2,width=3, tr=1){  ####return the half energy height for waveform beginning to ground
  x<-as.numeric(unlist(x))
  x[x==0]<-NA
  x<-x-min(x,na.rm = T)+1
  if (smooth== TRUE){
    x<-runmean(x,width,"C")
  }

  #####get the intial parameter for the waveform, it can be assumed as the prior parameters
  peakrecord<-lpeak(x,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(x,na.rm=T)
  ind<-x[peaknumber]>thres*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  newpeak<-x[realind]  #collect intensity
  nx<-x[1:realind[length(realind)]]
  find<-which.half(nx)
  medh<-(realind[length(realind)]-find)*0.15*tr
  hr<-find/realind[length(realind)]
  #if (sum(!is.na(x))%%2>0){
  #	ind<-which(x<=median(x,na.rm=TRUE)+0.5 & x>=median(x,na.rm=TRUE)-0.5)
  #	} else {
  #	a<-sort(x);ind<-which(x==a[sum(!is.na(x))/2])
  #		}
  #find<-ifelse(length(ind)%%2>0,ind[ceiling(length(ind)/2)],ind[length(ind)/2])
  return(c(medh,hr))

}
