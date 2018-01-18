#' which.half: calculate the position of half total energy position
#'
#' The function allows you to identify the half total energy position which can be used to calculate the height from waveform ending or waveform begining.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param normalization is determine whether you want to normalized the waveform intensity or not. Here we used the minimum intensity to conduct normalization. Default is use the normalized data.
#' @return return the index of half total energy position of waveform.
#' @export
#' @examples
#' 
#'data(return)
#'x<-return[1,]
#'#default
#'which.half(x)
#'#NOT USE normalization
#'which.half(x,normalization="TRUE")

which.half<-function(x, normalization = "TRUE"){
  x<-as.numeric(x);x[x==0]<-NA
  if (normalization =="TRUE"){
    x<- x - min(x,na.rm=TRUE)
  }
  rsum<-sum(x,na.rm=TRUE);x[is.na(x)]<-0  ###we need to assign NA them as zero,otherwise cannot use the cumsum
  ind<-which(cumsum(x)>rsum/2)[1]
  return(ind)
}
