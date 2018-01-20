#' percentile: calculate the position of energy percentile in the waveform (the total waveform).
#'
#' The function allows you to calculate the position of energy percentile based on the given quantile within a waveform.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .
#' @param quan is the quantile of energy you are interested in. Defalut is seq(0.5,1,0.1).
#' @param normalization is determine whether you want to normalized the waveform intensity or not. Here we used the minimum intensity for each waveform to conduct normalization.
#'                      Default is use normalization.
#' @return return the index or position of corressponding quantile energy of waveform.
#' @export
#' @examples
#'
#'data(return)
#'x<-return[1,]
#'#default
#'percentile(x)
#'
#'#change the quantile
#'qr<-seq(0.45,0.99,0.03)
#'re1<-percentile(x,quan=qr)
#'###normalization affects your results
#'re2<-percentile(x,quan=qr,normalization = "FALSE")
#'##after you get the index, you can convert them to the height based on temporal resolution or georeference information of this waveform.


percentile<-function(x,quan=seq(0.5,1,0.1),normalization = "TRUE"){
  x<- as.numeric (x)
  x[x==0]<-NA
  if (normalization =="TRUE"){
    x<- x - min(x,na.rm=TRUE)+1
  }
  rsum<-sum(x,na.rm=TRUE);x[is.na(x)]<-0  ###we need to assign NA them as zero,otherwise cannot use the cumsum

  qsum<-rsum*quan
  csum<-cumsum(x)
  sind<-sapply(csum, function(z) z>=qsum)
  ind<- apply(sind,1, function(z) which(z=="TRUE")[1])
  return(ind)
}


