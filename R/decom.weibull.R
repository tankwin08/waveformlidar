#' decom.weibull
#'
#' The function allows you to fit waveforms with the Weibull function and get parameters estimated.
#'
#' @param x is a waveform with a index at the begining and followed with intensities.
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width width of moving window.Default is 3, must be odd integer between 1 and n.This parameter ONLY work when the smooth is TRUE.

#' @return A list contains estimates of A (amplitude parameter), u (location parameter), sigma (scale parameter) and k (shape parameter) after decomposition.
#' these parameters are different from the adaptive Gaussian and Gaussian decomposition.
#' @import caTools
#' @import minpack.lm
#' @importFrom stats na.omit
#' @export
#' @references
#'   Tan Zhou, and Sorin C. Popescu, 2017. Bayesian decomposition of full waveform LiDAR data with
#'   uncertainty analysis. Remote Sensing of Environment 200 (2017): 43-62.
#' @examples
#'
#' ##import return waveform data
#' data(return)
#' lr<-nrow(return)
#' ind<-c(1:lr)
#' return<-data.frame(ind,return)
#' x<-return[1,] ###must be a dataset including intensity with index at the beginning.
#' r1<-decom.weibull(x)
#'
#' r2<-decom.weibull(return[2,])
#'
#' \donttest{
#' # for the whole dataset
#' dr3<-apply(return,1,decom.weibull)
#' dd<-return[10:20,]
#' dr4<-apply(dd,1,decom.weibull)
#' }


decom.weibull<-function(x,smooth=TRUE,thres=0.22,width=3){
  y0<-as.numeric(x)
  index<-y0[1]
  y<-y0[-1]
  y[y==0]<-NA
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if (smooth==TRUE) y<-runmean(y,width,"C")##"fast" here cannot handle the NA in the middle
  peakrecord<-lpeak(y,3)#show TRUE and FALSE
  peaknumber<-which(peakrecord == T)#show true's position, namely time in this case
  #peaknumber,it show the peaks' corresponding time
  imax<-max(y,na.rm=T)
  ind<-y[peaknumber]>thres*imax      #####################you need to change threshold##########################################
  realind<-peaknumber[ind]#collect time
  newpeak<-y[realind]  #collect intensity
  z<-length(realind)

  #then we fliter peak we have in the waveform
  #you must define newpeak as a list or a vector(one demision),otherwise it's just a value
  #I just assume that intensity is larger than 45 can be seen as a peak, this can be changed

  #####if the peak location is too close, remove it just keep one???????
  #not sure we really need this step

  ##################################initilize parameters
  ##use adptive Gaussian
  ##first use gaussian to estimate the values

  gu<-realind  ###this more like location, but it represent sigma in weibull function we defined
  gi<-newpeak*20  ##here we make it 10 time larger

  gsd<-sample(seq(0,10,1),z)

  r<- 3.5

  init0<-wgennls(gi,gsd,gu,r)

  #init$formula
  #init$start
  df<-data.frame(x=seq_along(y),y)
  #log<-tryCatch(fit<-nlsLM(init0$formula,data=df,start=init0$start,algorithm='LM',lower=low,upper=up,control=nls.lm.control(factor=100,maxiter=1024,
  log<-tryCatch(fit<-nlsLM(init0$formula,data=df,start=init0$start,algorithm='LM',
                           control=nls.lm.control(factor=100,maxiter=1024,
                           ftol = .Machine$double.eps, ptol = .Machine$double.eps),na.action=na.omit),error=function(e) NULL)#this maybe better

  ###then you need to determine if this nls is sucessful or not?
  if (!is.null(log)){
    result=summary(fit)$parameters
    #pn<-sum(result[,1]>0)
    rownum<-nrow(result);#npeak<-rownum/4
    #record the shot number of not good fit
    rightfit<-NA;ga<-matrix(NA,rownum,5);#pmi<-matrix(NA,npeak,9)
    ga<-cbind(index,result)
    pmi<-NULL
    #if (pn==rownum){
      rightfit<-index

      ####directly get the parameters
      ###make a matrix
      pm<-matrix(NA,z,8)
      pm[,1]<-result[1:z,1];pm[,5]<-result[1:z,2]
      s2<-z+1;e2<-2*z
      pm[,2]<-result[s2:e2,1];pm[,6]<-result[s2:e2,2]
      s3<-2*z+1;e3<-3*z
      pm[,3]<-result[s3:e3,1];pm[,7]<-result[s3:e3,2]
      s4<-3*z+1;
      pm[,4]<-result[s4,1];pm[,8]<-result[s4,2]
      pmi<-cbind(index,pm)
      colnames(pmi) = c("index","A","u","sigma","k","A_se","u_se","sigma_se","k_se")

    #}
    return (list(rightfit,ga,pmi))
  }
}
