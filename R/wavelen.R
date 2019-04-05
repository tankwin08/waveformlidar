#' wavelen
#'
#' The function allows you to measure the length of the waveform with intensity after excluding non-intensity parts.
#' Generally, we assumed the intensity of a waveform should be larger than 0.
#'
#' @param x is the waveform intensities. If you have other information, you should delete these intensites before you run this function .

#' @return return the the number of waveform intensities or waveform length.
#' @export
#' @examples
#'
#' data(return)
#' y<-cbind(1:nrow(return),return)
#' x<-return[1,]
#' wavelen(x)
#' #for x has index and intensities, we should delete non-intensities first
#' yy<-y[,-1]
#' xx<-yy[1,]
#' wavelen(xx)


wavelen<-function(x){
  x<-as.numeric(x)
  ind<-which(x>0)
  num<-ind[length(ind)]-ind[1]+1
  return(num)
}
