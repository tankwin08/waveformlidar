#' wgennls
#'
#' The function allows you to prepare the formula and start values for the decomposition using Weibull functions.
#'
#' @param A is the amplitude of waveform.
#' @param u is a location parameter in the Weibull model, but this one is different from Gaussian or adaptive Gaussian distribution.
#' @param sig the scale parameter that controls the spread of the distribution,sig>0.
#' @param k is the shape parameter that controls the behaviour or the shape of distribution, k>0.
#' @return A formula suitable for different number of waveform componments with Weibull distribution.
#' @export
#' @examples
#'
#' A<-c(1000,900,1500);u<-c(-3,-5,0);sig<-c(30,40,75); k<-c(3,3,3)
#' ##these four should have the same length

#' fg<-wgennls(A,u,sig,k)
#' ##input formula for Gaussian decomposition
#' fgf<-fg$formula
#' ###start values
#' fgs<-fg$start


wgennls <- function(A, u, sig, k) {
  n <- seq_along(A)
  fs <- paste("y~",
              paste(paste0("A",n,"*k/sigma",n,"*(abs(x-u",n,")/sigma",n,")**(k-1)*exp(-(abs(x-u",n,")/sigma",n,")**k)"),collapse="+",sep="")
  )
  names(A)<-paste0("A",seq_along(A))
  names(u)<-paste0("u",seq_along(u))
  names(sig)<-paste0("sigma",seq_along(sig))
  names(k)<-paste0("k",seq_along(k))
  names(k)<-"k"
  list(
    formula=as.formula(fs),
    start=c(as.list(A), as.list(u), as.list(sig),as.list(k))
  )
}
