#' deconvolution: deconvolove waveform to obtain effective target positions.
#'
#' The function allows you to deconvolve retrun waveform lidar data by using the corresponding outgoing waveform and impulse response to
#'   to obtain effective target positions. Two algorithms were used including Gold or Richardson-Lucy algorithms. The detailed deconvolution
#'   infromation can be founded in package Peaks.
#'
#' @param re is the return waveform.
#' @param out is the outgoing waveform.
#' @param imp is the impulse reponse.
#' @param method two methods including Gold or Richardson-Lucy (RL) algorithms. method=c("Gold","RL"). Default is method=c("Gold").
#' @param normalization is determine whether you want to normalized the waveform intensity or not. Here we used the minimum intensity to conduct normalization. Default is use normalization.
#' @param para1 is the parameters including iterations, repetitions and boost for deconvolution when the number of waveform componments is less than np.
#' @param para2 is the parameters including iterations, repetitions and boost for deconvolution when the number of waveform componments is more than np.
#'   Generally when the waveform become more complicated, these parameters should be larger than corresponding para1.
#'   iterations: number of iterations (parameter L in the Gold deconvolution algorithm) between boosting operations;
#'   repetitions number of repetitions of boosting operations. It must be greater or equal to one.So the total number of iterations is repetitions*iterations
#'   boosting coefficient/exponent. Applies only if repetitions is greater than one. Recommended range [1..2].
#' @param np a threshold value which determines use para1 (smaller iterations and repetitions in the deconvolution) or
#'          or para2 (larger iterations and repetitionsin the deconvolution). Default is 2.
#' @return The deconvovled waveform.
#' @import data.table
#' @import Peaks
#' @export
#' @examples
#'
#' data(return)
#' data(outg)
#' data(imp)  ##The impulse function is generally one for the whole study area or
#'
#' re<-return[1,]
#' out<-outg[1,]
#' imp<-imp
#'
#' dr<-deconvolution(re,out,imp)
#' dr1<-deconvolution(re,out,imp,method="RL")
#' dr2<-deconvolution(re,out,imp,method="RL",para1=c(20,2,1.8,20,2,2))
#'
#' plot(dr,type="l")
#' lines(dr1,col="red")
#' lines(dr2,col="blue")
#' ###some differences can be observed when you used different parameters.
#' ##In the real application, you need to find optimized parameters suitable for your case.
#' ## In addition, the accuracy of impulse function significantly affects results based on experiments.
#'





deconvolution<-function(re,out,imp,method =c("Gold"),np=2,normalization="TRUE",para1=c(30,2,1.8,30,2,2),para2=c(40,3,1.8,40,3,2)){

  library.dynam('Peaks', 'Peaks', lib.loc=NULL)
  #(the above line tells R to load the dynamic library for the Peaks package, which allows Rcpp to call its functions.)
  y<-as.numeric(re)
  x<-as.numeric(out)
  im<- as.numeric(imp)
  le<- length(which(im>0))
  im<-im[1:le]

  if (normalization=="TRUE"){
    y[y==0]<-NA;
    y<-y-min(y,na.rm=T)+1
    y[is.na(y)]<-0


    x[x==0]<-NA
    x<-x-min(x,na.rm=T)+1
    x[is.na(x)]<-0

    im<-im-min(im)+1
  }
  zn<- npeaks(y)
  if (zn<=np){
    irb1<-para1[1:3]
    irb2<-para1[4:6]
  } else {
    irb1<-para2[1:3]
    irb2<-para2[4:6]
  }

    ###parameter are import for geting the information
    y1<-SpectrumDeconvolution(y,im,iterations=irb1[1],repetitions=irb1[2],boost=irb1[3],method=method)
    de<-SpectrumDeconvolution(y1,x,iterations=irb2[1],repetitions=irb2[2],boost=irb2[3],method=method)

  return(round(de,2))
}







