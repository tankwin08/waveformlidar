#' peakfind: find peak locations, corresponding amplitude and echo width
#'
#' The function allows you to roughly know A,u,sig parameters will fit in the Gaussian decomposition.
#'
#' @param x is a waveform with a index at the begining mainly to .
#' @param drop is the index of waveform index we should ingore or non-intensity of waveform information.Default is c(0,0) that means use the all input data.
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.2.
#' @param width width of moving window.Default is 3, must be integer between 1 and n. This parameter ONLY work when the smooth is TRUE.
#' @return return a list contains waveform index, rough estimates of A, u, sig.
#' @export
#' @examples
#'
#' ##import return waveform data
#' data(return)
#' ind<-c(1:nrow(return))
#' return<-data.frame(ind,return)
#' x<-return[1,] ###must be must be a dataset incluing intensity with index at the beginning.
#' peakfind(x) ## index, estimated A, u, and sig
#'
#'##to get accurate estimates of A, u,g, you need to explore your dataset to optimized parameters.
#'##generally thres affects a lot, assigning smooth to TRUE is preferable in most of cases.
#' for the whole dataset
#' dr<-apply(return,1,peakfind)
#' ####to manage data and store in a data frame.
#' rpf<-do.call("rbind",lapply(dr,"[[",1))
#'


peakfind<-function(x, smooth="TRUE",thres=0.2,width=3){
  y0<-as.numeric(x);index<-y0[1]
  y<-y0[-1];
  y[y==0]<-NA
  ###when for direct decomposition, you need the following two steps
  y<-y-min(y,na.rm = T)+1
  if (smooth=="TRUE"){
    y<-runmean(y,width,"C")##"fast" here cannot handle the NA in the middle
  }

  peakrecord<-lpeak(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-which(y[peaknumber]>thres*imax)    #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  newpeak<-y[realind]  #collect intensity
  z<-length(realind)
  ########make a matrix to store data,parameters
  wm<-matrix(0,z,4)
  wm[,1]<-rep(index,z)
  wm[,2]<-newpeak
  wm[,3]<-realind
  wm[1,4]<-realind[1]/4   ####sd
  if (z>1){
    wm[2:z,4]<-diff(realind)/3
  }

  return(list(wm))
}
