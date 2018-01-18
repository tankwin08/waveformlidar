#' automatic generate a fromula and start values for unknown parameters for a mixture of adaptive or generalized Gaussian distribution
#'
#' The function allows you to prepare the formula and start values for adaptive or generalized Gaussian decomposition.
#'
#' @param A is the amplitude of waveform.
#' @param u is the peak location of waveform.
#' @param sig is the echo width of waveform.
#' @param r is the rate parameter of adpative or generalized Gaussian distribution. For Gaussian distributionm, r is fixed to 2.
#' @return return a formula suitable for different number of waveform componments with adaptive Gaussian distribution.
#' @export
#' @examples
#' 
#' A<-c(76,56,80);u<-c(29,40,67);sig<-c(4,3,2.5); r<-c(2,2,2) ##these four should have the same length

#' fg<-agennls(A,u,sig,r)
#' ##input formula for Gaussian decomposition
#' fgf<-fg$formula
#' ###start values
#' fgs<-fg$start


agennls <- function(A, u, sig, r) {
  n <- seq_along(A)
  fs <- paste("y~",
              paste(paste0("A",n," *exp(-(x-u",n,")**r",n,"/(2*sigma",n,"**2))"),collapse="+",sep="")
  )
  names(A)<-paste0("A",seq_along(A))
  names(u)<-paste0("u",seq_along(u))
  names(sig)<-paste0("sigma",seq_along(sig))
  names(r)<-paste0("r",seq_along(r))
  list(
    formula=as.formula(fs),
    start=c(as.list(A), as.list(u), as.list(sig),as.list(r))
  )
}
