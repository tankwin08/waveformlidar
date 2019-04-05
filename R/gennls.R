#' gennls
#'
#' The function allows you to prepare the formula and start values for Gaussian decomposition
#'
#' @param A is the amplitude of waveform.
#' @param u is the peak location of waveform.
#' @param sig is the echo width of waveform.
#' @return return a formula suitable for different number of waveform componments with Gaussian distribution.The number of componments
#'         needs to be determined by the users. Generally you need to use peakfind function to roughly estimate the number of waveform componments
#' @export
#' @examples
#'
#' A<-c(76,56,80);u<-c(29,40,67);sig<-c(4,3,2.5) ##these three should have the same length
#' fg<-gennls(A,u,sig)
#' ##input formula for Gaussian decomposition
#' fgf<-fg$formula
#' ###start values
#' fgs<-fg$start

gennls <- function(A, u, sig) {
  n <- seq_along(A)
  fs <- paste("y ~",
              paste(
                paste0("A",n, " * exp(-(x-u",n,")**2/(2 * sigma",n,"**2))"),
                collapse=" + ")
  )
  names(A)<-paste0("A",seq_along(A))
  names(u)<-paste0("u",seq_along(u))
  names(sig)<-paste0("sigma",seq_along(sig))
  list(
    formula=as.formula(fs),
    start=c(as.list(A), as.list(u), as.list(sig))
  )
}
