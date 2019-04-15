#' deconvolution
#'
#' The function allows you to deconvolve retrun waveform lidar data by using the system impulse response (it can be given by the data provider or measured with the return impulse response
#'  and its corresponding outgoing pluse) and corresponding outgoing waveform to obtain effective target positions. Two algorithms including Gold or Richardson-Lucy algorithms were available.
#'
#' @param re is the return waveform.
#' @param out is the outgoing waveform, which should have the same number of rows.
#' @param imp is the return impulse reponse (this should come with the corresponding outgoing pluse) or the sytem impulse reponse (when the corresponding outgoing pulse
#' for the impluse response is not available or the data vendors directly provide system impluse response).
#' @param imp_out is the corresponding outgoing pulse of the return impulse response. Through devonvolution of return impulse response, we can obtain
#'  true system impluse repsonse if it wasn't provided by the data vendor.
#' @param method two methods including Gold or Richardson-Lucy (RL) algorithms. method=c("Gold","RL"). Default is method=c("Gold").
#' @param np is a threshold value which determines use small_paras (smaller iterations and repetitions in the deconvolution) or
#'          or large_paras (larger iterations and repetitionsin the deconvolution). Default is 2.
#' @param rescale is determine whether you want to rescale the waveform intensity. Generally, we used the minimum intensity to conduct rescaling. Default is to implement rescale.
#' @param small_paras is the deconvolution parameters including iterations, repetitions and boost when condcuting the system impulse and outgoing pulse deconvolution algorithms with waveform componments is less than np
#' or the waveform is less complicated with few noise.
#' @param large_paras is the deconvolution parameters including iterations, repetitions and boost for the system impulse and outgoing pulse deconvolution when the number of waveform componments is more than np.
#'   Generally when the waveform become more complicated, we should use larger iteration, repetitions.
#'   iterations: number of iterations (parameter in the Gold deconvolution algorithm) between boosting operations;
#'   repetitions number of repetitions of boosting operations. It must be greater or equal to one.So the total number of iterations is repetitions*iterations
#'   boosting coefficient/exponent. Applies only if repetitions is greater than one. Recommended range [1,2].
#' @param imp_out_pars is the deconvolution parameters for obtaining system impluse response using the impulse response and corresponding outgoing pulse.
#' As same as the small_paras and large_parameters. This parameter is effective only when the imp_out is not NULL.

#' @return The deconvovled waveform.
#' @import data.table
#' @export
#' @references
#'   Zhou, Tan*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017. Gold-A novel deconvolution algorithm with
#'   optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017):
#'   131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021
#'
#' @examples
#' #not run
#' #library(waveformlidar)
#' #if (!require("rPeaks")) {
#' #    devtools::install_github("jrminter/rPeaks")
#' #  }
#' #data(return)
#' #data(outg)
#' #data(imp)  ##The impulse function is generally one for the whole study area or
#' #data(imp_out)
#'
#' #re<-return[1,]
#' #out<-outg[1,]
#' #imp<-imp
#'
#' #dr<-deconvolution(re,out,imp)
#' #dr1<-deconvolution(re,out,imp,method="RL")
#' #dr2<-deconvolution(re,out,imp,method="RL",small_paras=c(20,2,1.8,20,2,2))
#'
#' #plot(dr,type="l")
#' #lines(dr1,col="red")
#' #lines(dr2,col="blue")
#' ###some differences can be observed when you used different parameters.
#' ##In the real application, you need to find optimized parameters suitable for your case.
#' ## In addition, the accuracy of impulse function significantly affects results based on experiments.
#'

deconvolution<-function(re,out,imp,imp_out=NULL,method =c("Gold"),np=2,rescale=TRUE,
                        small_paras=c(30,2,1.8,30,2,1.8),large_paras=c(30,3,1.8,40,3,1.8),
                        imp_out_pars=c(20,5,1.8)){

  #library.dynam('Peaks', 'Peaks', lib.loc=NULL)
  #(the above line tells R to load the dynamic library for the Peaks package, which allows Rcpp to call its functions.)

  # if (!requireNamespace("rPeaks", quietly = TRUE)) {
  #   devtools::install_github("jrminter/rPeaks")
  # }
  y<-as.numeric(re)
  x<-as.numeric(out)


  if (rescale == TRUE){
    y[y==0]<-NA;
    y<-y-min(y,na.rm=T)+1
    y[is.na(y)]<-0


    x[x==0]<-NA
    x<-x-min(x,na.rm=T)+1
    x[is.na(x)]<-0

    imp[imp==0]<-NA
    minim<-min(imp,na.rm=TRUE)
    imp<-imp-minim+1
    imp[is.na(imp)]<-0
  }


  zn<- npeaks(y)

  if (zn<=np){
    irb1<-small_paras[1:3]
    irb2<-small_paras[4:6]
  } else {
    irb1<-large_paras[1:3]
    irb2<-large_paras[4:6]
  }

  if (is.null(imp_out) == FALSE) {
    imp_out[imp_out==0]<-NA
    imp_out<-imp_out - min(imp_out,na.rm=TRUE)
    imp_out[is.na(imp_out)]<-0
    imre<-rPeaks::SpectrumDeconvolution(imp,imp_out,iterations=imp_out_pars[1],repetitions=imp_out_pars[2],boost=imp_out_pars[3],method=method)
    imre<-round(imre,2)  #########################################very important step

    y1<-rPeaks::SpectrumDeconvolution(y,imre,iterations=irb1[1],repetitions=irb1[1],boost=irb1[3],method=method)
  } else {
    ###parameter are import for geting the information
    y1<-rPeaks::SpectrumDeconvolution(y,imp,iterations=irb1[1],repetitions=irb1[2],boost=irb1[3],method=method)
  }
    ###parameter are import for geting the information
    #y1<-SpectrumDeconvolution(y,im,iterations=irb1[1],repetitions=irb1[2],boost=irb1[3],method=method)
    de<-rPeaks::SpectrumDeconvolution(y1,x,iterations=irb2[1],repetitions=irb2[2],boost=irb2[3],method=method)

  return(round(de,2))
}







