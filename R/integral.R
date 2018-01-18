#' integral: calculate the integral or cumulative enery from vegetation and ground parts of the waveform.
#'
#' The function allows you to calculate the integral of vegetation part, ground part, sum of them and ration between vegation integral and total integral.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.2.
#' @param width width of moving window.Default is 3, must be integer between 1 and n.This parameter ONLY work when the smooth is TRUE.
#' @param tr the temporal resolution of waveform.Default is 1 ns. Must be integer from 1 to n.
#' @param dis the distance from last echo (ground) which determine the boundary between ground and vegetation. Default is 20 ns which equals to 3 m.
#'   This means the ground part signals are from identified ground location to 20 ns above and the signals of vegetation are from waveform begining to 3 m above ground.

#' @return return the integral of waveform intensity for the ground, vegetation parts, the whole waveform above ground and the integral ration betwwen vegetation and the whole waveform .
#' @import caTools
#' @import flux
#' @export
#' @examples
#'
#'data(return)
#'x<-return[1,]
#'#if we kept everything as default, before you use this function, you need to know the temporal resolution of waveform, default is 1 ns.
#'##for one peak, it generaly not make too much sense since generally only ground was present in this case.
#'r1<-integral(x)
#'
#'#if you didn't want to smooth,
#'r2<-integral(x,smooth="FALSE")
#'
#'##you also can define the boundary between vegetation and ground by assign adjusting dis
#'#if we assign 15, it means vegetation starts at 15*1*0.15 m above the last echo.
#'
#'r3<-integral(x,dis=15)
#'# when it comes to the waveform with several peaks
#'xx<-return[182,]
#'
#'rr1<-integral(xx)
#'


##########find the maximum ampilitude above 3m above the the last echo
integral<-function(y,smooth="TRUE",thres=0.2,width=3,tr=1,dis=20){
  y<-as.numeric(unlist(y))
  y[y==0]<-NA
  #y<-y[-c(1:11)]
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if (smooth=="TRUE"){
    y<-runmean(y,width,"C")
  }
  #####get the intial parameter for the waveform, it can be assumed as the prior parameters
  peakrecord<-peaks(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-y[peaknumber]>thres*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  newpeak<-y[realind]  #collect intensity
  if (length(realind)>1){
    pind1<-realind[length(realind)]-dis
    inddiff<-diff(realind)
    vegind<-ifelse (rev(inddiff)[1]>dis,pind1,realind[length(realind)-1])
    groind<-realind[length(realind)]  ###here 20*0.15=3m
    maxa<-max(newpeak[1:length(newpeak)-1],na.rm=T);
  } else {
    maxa=imax
    vegind<-realind-dis
    groind<-realind
  }
  ty<-y[1:groind]
  tintegral<-auc(seq_along(ty)*tr*0.15,ty)
  vegy<-y[1:vegind]
  vegintegral<-auc(seq_along(vegy)*tr*0.15,vegy)
  grointegral<-tintegral - vegintegral
  ratiointegral<-vegintegral/tintegral
  return (c(grointegral,vegintegral,tintegral,ratiointegral))
}
