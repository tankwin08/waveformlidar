#' decom: decompose waveform with different functions including Gaussian, adpative Gaussian and Weibull functions.
#'
#' The function allows you to eatimate parameters charcterizing waveforms and to pave the way for generating waveform-based point cloud.
#'
#' @param x is a waveform with a index at the begining and followed with intensities.
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width width of moving window.Default is 3, must be odd integer between 1 and n.This parameter ONLY work when the smooth is TRUE.
#' @param fun determine which method will be used for waveform decompsotion.Default is to use Gaussian (G) function. Other two functions are adpative Gaussian (AG) and Weibull (W) functions.

#' @return A list contains estimates of A, u, sig after decomposition.
#' @importFrom caTools runmean
#' @importFrom minpack.lm nlsLM
#' @export
#' @examples
#'
#' ##import return waveform data
#' data(return)
#' lr<-nrow(return)
#' ind<-c(1:lr)
#' return<-data.frame(ind,return)
#' x<-return[182,] ###must be a dataset including intensity with index at the beginning.
#' r1<-decom(x)
#' r2<-decom(x,smooth="TRUE",width=3) ###you can assign different smooth width for the data
#'
#' for the whole dataset
#' dr<-apply(return,1,decom)
#'
#' ####to collect all data
#' rfit<-do.call("rbind",lapply(dr,"[[",1)) ## waveform is correctly decomposed with index,some are not correct index by NA
#' ga<-do.call("rbind",lapply(dr,"[[",2))   ###the original results, which can help to get more detailed results.
#' pa<-do.call("rbind",lapply(dr,"[[",3))   ###useful decompostion results for next step or geolocation transformation.
#'
#' colnames(pa)<-c("index","pi","t","sd","pise","tse","sdse")
#'
#' ####delete some wrong ones
#' rid<-rfit[!is.na(rfit),]
#' wid<-setdiff(c(1:lr),rid)  ###index of waveforms needs to be reprocessed
#' rpars<-pa[!is.na(pa[,1]),]    ###useful decomposition parameters



decom<-function(x,smooth="TRUE",thres=0.22,width=3,fun="G"){
  y0<-as.numeric(x);index<-y0[1]
  y<-y0[-1]
  y[y==0]<-NA
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if (smooth=="TRUE"){
    y<-runmean(y,width,"C")##"fast" here cannot handle the NA in the middle
  }
  peakrecord<-peaks(y,3)#show TRUE and FALSE
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
  ###for normal Gaussian
  gu<-realind
  gi<-newpeak*2/3
  gsd<-realind[1]/5
  if (z>1){
    gsd[2:z]<-diff(realind)/4
  }


  ri<- rnorm (1,2,0.1)  ###for adaptive Gaussian function

  ##for weibull function
  wi<- rnorm (1,3,1)  ##this one can be fixed,but not sure if this will work or not
  #we should define some start value of the nls, then can reduce the error
  wgsd<-rnorm(z,realind,5)
  wgu<-rnorm(z,0,3)
  wgi<-rnorm(z,2000,500)


  # start to fit use the auto generate formula
  if (fun=="G"){
    init0 <- gennls(gi, gu, gsd)
  } else if (fun=="AG") {
    init0 <- agennls(gi, gu, gsd, ri)
  } else {
    init0 <- wgennls(wgi, wgu, wgsd, wi) ####we should use different prior information for the weibull function
  }


  #init$formula
  #init$start
  df<-data.frame(x=seq_along(y),y)
  log<-tryCatch(fit<-nlsLM(init0$formula,data=df,start=init0$start,algorithm='LM',control=nls.lm.control(factor=100,maxiter=1024,
                          ftol = .Machine$double.eps, ptol = .Machine$double.eps),na.action=na.omit),error=function(e) NULL)#this maybe better
  ###then you need to determine if this nls is sucessful or not?
  if (!is.null(log)){
    result=summary(fit)$parameters
    pn<-sum(result[,1]>0)
    rownum<-nrow(result);npeak<-rownum/3
    #record the shot number of not good fit
    rightfit<-NA;ga<-matrix(NA,rownum,5);pmi<-matrix(NA,npeak,7)
    if (pn==rownum){
      rightfit<-index
      ga<-cbind(index,result)
      ####directly get the parameters
      ###make a matrix
      pm<-matrix(NA,npeak,6)
      pm[,1]<-result[1:npeak,1];pm[,4]<-result[1:npeak,2]
      s2<-npeak+1;e2<-2*npeak
      pm[,2]<-result[s2:e2,1];pm[,5]<-result[s2:e2,2]
      s3<-2*npeak+1;e3<-3*npeak
      pm[,3]<-result[s3:e3,1];pm[,6]<-result[s3:e3,2]
      pmi<-cbind(index,pm)
    }
    return (list(rightfit,ga,pmi))
  }
}




