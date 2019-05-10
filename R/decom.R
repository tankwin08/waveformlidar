#' decom
#'
#' The function allows you to eatimate parameters charcterizing waveforms and to pave the way for generating waveform-based point cloud.
#'
#' @param x is a waveform with a index at the begining and followed with intensities.
#' @param smooth is tell whether you want to smooth the waveform to remove some obvious outliers. Default is TRUE.
#' @param thres is to determine if the detected peak is the real peak whose intensity should be higher than threshold*maximum intensity. Default is 0.22.
#' @param width width of moving window.Default is 3, must be odd integer between 1 and n.This parameter ONLY work when the smooth is TRUE.

#' @return A list contains estimates of A, u, sig after decomposition.
#' @import caTools
#' @import minpack.lm
#' @importFrom stats na.omit
#' @export
#' @references
#'   Zhou, Tan*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017. Gold-A novel deconvolution algorithm with
#'   optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017):
#'   131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021
#'
#' @examples
#'
#' ##import return waveform data
#' library(data.table)
#' data(return)
#' return<-data.table(index=c(1:nrow(return)),return)
#' x<-return[1,] ###must be a dataset including intensity with index at the beginning.
#' r1<-decom(x)
#' r2<-decom(x,smooth=TRUE,width=5) ###you can assign different smooth width for the data
#' ###when it comes very noisy waveforms, it may give you some problems
#' xx<-return[182,]
#' r3<-decom(xx)  ##this one returns NULL which means the function didn't work for the
#'                ##complex waveform or too noisy waveform,we should try to reprocess
#'                ##these unsucessful waveforms using larger width to smooth the waveforms.
#' r4<-decom(xx,smooth=TRUE,width=5) ##when you change to a larger width, it can work,
#'                                   #but give you some unreasonable estimates, return NA
#'
#' ###original result from this decom is (you will not see it, the function filter this result
#' ###and put NA for the estimation since they maybe not right results)
#' #Nonlinear regression model
#' #model:y~A1*exp(-(x-u1)^2/(2*sigma1^2))+A2*exp(-(x-u2)^2/(2*sigma2^2)) n\
#' #+A3*exp(-(x-u3)^2/(2*sigma3^2))
#' #data: df
#' #A1      A2      A3      u1      u2      u3  sigma1  sigma2  sigma3
#' #228.709 -30.883  81.869  41.640  42.131  71.680  14.613   3.522   8.073
#' ##A (ampilitude should not be negative)
#'
#' r5<-decom(xx,width=10) ##this will work by smoothing the waveform
#' r6<-decom(xx,thres=0.1,width=5)  ##by adjusting width and thres of real peak, you may
#'                                  ##get a reasonable results
#' \donttest{
#' # for the whole dataset
#' dr<-apply(return,1,decom)
#' }

#'


decom<-function(x,smooth=TRUE,width=3,thres=0.22){
  y0<-as.numeric(x)
  index<-y0[1]
  y<-y0[-1]
  y[y==0]<-NA
  ###when for direct decomposition
  y<-y-min(y,na.rm = T)+1
  if (smooth ==TRUE)  y<-runmean(y,width,"C")##"fast" here cannot handle the NA in the middle
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

  #####if the peak location is too close, remove it just keep one
  #not sure we really need this step

  ##################################initilize parameters
  ###for normal Gaussian
  gu<-realind
  gi<-newpeak*2/3
  gsd<-realind[1]/5
  if (z>1){
    gsd[2:z]<-diff(realind)/4
  }

  # start to fit use the auto generate formula

    init0 <- gennls(gi, gu, gsd)

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
    rightfit<-NA;ga<-matrix(NA,rownum,5);#pmi<-matrix(NA,npeak,7)
    ga<-cbind(index,result)
    pmi<-NULL
    if (pn==rownum){
      rightfit<-index
      #ga<-cbind(index,result)
      ####directly get the parameters
      ###make a matrix
      pm<-matrix(NA,npeak,6)
      pm[,1]<-result[1:npeak,1];pm[,4]<-result[1:npeak,2]
      s2<-npeak+1;e2<-2*npeak
      pm[,2]<-result[s2:e2,1];pm[,5]<-result[s2:e2,2]
      s3<-2*npeak+1;e3<-3*npeak
      pm[,3]<-result[s3:e3,1];pm[,6]<-result[s3:e3,2]
      pmi<-cbind(index,pm)
      colnames(pmi) = c("index","A","u","sigma","A_std","u_std","sig_std")
    }
    return (list(rightfit,ga,pmi))
  }
}




