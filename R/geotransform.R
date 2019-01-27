#' geotransform: convert parameter from decomposition to point cloud.
#'
#' The function allows you to convert parameters from decomposition to point cloud by using georeference data (generally it should come with waveform data).
#'   Detailed description of how to calculate can refer to Zhou, T., Popescu, S.C., Krause, K., Sheridan, R.D., Putman, E., 2017.
#'   Gold - a noveldeconvolution algorithm with optimization for waveform LiDAR processing.ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017): 131-150.
#'   For the direct decomposition method, three kinds of position are calculated: leading edge, peak and trail edge. For the deconvolution and decomposition method,
#'   only the peak position is used to calculate the position. In additional, the uncertianty of the point cloud was provided based on detected peaks'
#'   95% confidence interval using deterministic method for waveform decomposition. This fucntion is suitable for NEON waveform lidar. For other kinds of dataset,
#'   you need to preprocess data to the same format or have the same required information or datsets.
#'
#' @param decomp the parameter estimation result after decomposition.
#' @param geo the reference geolocation that is generally coming with waveform data and provided by the data provider.
#' @param decomindex this tells which colomns of decomposition results are used for desired positions' geolocation calculation.
#'   7 important colomns are needed to be assigned: "index" (the index of waveform), "pi" (estimated amplitude,A), "t"(estimated peak location this is the most important one, u),
#'   "sd" (estimated echo width of the waveform, sigma), "pise" (standard error of estimated amplitude), "tse" (standard error of estimated peak location), and "sdse" (standard error
#'   of estimated echo width). Default colomn number for decompostion results is c(1:7).
#' @param geoindex this tells which colomns of geo-reference are used for desired positions' geolocation calculation. 10 important colomns are needed to be
#'   assigned: "i" (index of waveform which help to find the corresponding reference data for waveform data),"orix" (reference x (Easting) position), "oriy" (reference y (Northing) position),
#'   "oriz" (reference z (height) position), "dx","dy", "dz" (pulse direction vector that can measure position change per nanosecond), "outref" (the outgoing pulse reference bin location
#'   (leading edge 50% point of the outgoing pulse)), "refbin" (first return reference bin location (leading edge 50% point of the first return)) and "outpeak" (the time of
#'   peak location for the each outgoing pulse). Default colomn number for geo-reference data is c(1:9,16).
#' @param source is determined by input data. If estimated parameters are from dcomposition, source = "decomposition". Otherwise will be deconvolution and decomposition.
#'   Default is decomposition.

#' @return A dataframe with columns. For the direct decompostion method, we will have 29 columns and for the deconvolution and decomposition method,
#'   17 columns were generated ("index","pi","t","sd","pise","tse","sdse","px","py","pz","uncerUXpeak","uncerUYpeak","uncerUZpeak","uncerLXpeak","uncerLYpeak","uncerLZpeak","rn").
#'   \item{index}{The index of waveform}
#'   \item{pi}{The estimated amplitude of an waveform componment}
#'   \item{t}{The estimated peak location of an waveform componment}
#'   \item{sd}{The estimated echo width of an waveform componment}
#'   \item{pise}{The standard error of the estimated amplitude}
#'   \item{tse}{The standard error of the estimated peak location}
#'   \item{sdse}{The standard error of the estimated echo width}
#'   \item{px}{Desired x position using peak locations}
#'   \item{py}{Desired y position using peak locations}
#'   \item{pz}{Desired y position using peak locations}
#'   \item{lowx}{Desired x position using leading edge locations}
#'   \item{lowy}{Desired y position using leading edge locations}
#'   \item{lowz}{Desired z position using leading edge locations}
#'   \item{upx}{Desired x position using trailing edge locations}
#'   \item{upy}{Desired y position using trailing edge locations}
#'   \item{upz}{Desired z position using trailing edge locations}
#'   \item{uncerUXpeak}{upper bound of 95% confidence interval of px}
#'   \item{uncerUYpeak}{upper bound of 95% confidence interval of py}
#'   \item{uncerUZpeak}{upper bound of 95% confidence interval of pz}
#'   \item{uncerLXpeak}{lower bound of 95% confidence interval of px}
#'   \item{uncerLYpeak}{lower bound of 95% confidence interval of py}
#'   \item{uncerLZpeak}{lower bound of 95% confidence interval of pz}
#'   \item{uncerUXleading}{upper bound of 95% confidence interval of lowx}
#'   \item{uncerUYleading}{upper bound of 95% confidence interval of lowy}
#'   \item{uncerUZleading}{upper bound of 95% confidence interval of lowz}
#'   \item{uncerLXleading}{lower bound of 95% confidence interval of lowx}
#'   \item{uncerLYleading}{lower bound of 95% confidence interval of lowy}
#'   \item{uncerLZleading}{lower bound of 95% confidence interval of lowz}
#'   \item{rn}{the number of return for each waveform}
#'   By combining xyz, the users can get waveform-based point cloud using leading edge, peak and trail edge positions, and parameter uncertainty of the point cloud.
#' @import data.table
#' @import splitstackshape
#' @import sqldf
#' @export
#' @examples
#'
#' data(geo)
#' data(decom_result)
#' data(decon_result)
#'##used part of data to show the results
#' decomp<-decom_result[1:80,]
#' geo<-geo[1:80] ###the index of geolocation files should include all index of decomposition results.
#' deconp<-decon_result[1:80]
#'
#' ####for decomposition results
#' geor<-geotransform(decomp,geo)  ###generally you need to specify the columns for corresponding paramters
#'                                 #####here we used the example from the data, so we kept it default.
#'                                 ###most of us maybe just interested in xyz (px,py,pz or lowx,lowy,lowz or upx,upy,upz) and intensity(pi).
#' geor1<-geotransform(decomp,decomindex=c(1:7),geoindex=c(1:9,16))
#'
#' ########for deconvolution and decomposition
#' decongeo<-geotransform(decomp=deconp,geo,source="deconvolution")
#'
#' ##the columns for deconvolution and decomposition should be different as we have stated in the description.




#####new function for geolocation transformation
#geoindex=c(1:9,16)
#colnames(geo)[geoindex]<-c("index","orix","oriy","oriz","dx","dy","dz","outref","refbin","outpeak")
#colnames(decomp)[decomindex]<-c("index","pi","t","sd","pise","tse","sdse")

geotransform<-function (decomp,geo,source="decomposition"){
  ###1 get the right geo reference according to the data you got
  decomp<- data.table(decomp)
  setnames(decomp,c("A","u","sigma","A_se","u_se","sigma_se"),
            c("pi","t","sd","pise","tse","sdse"))

  id<-unique(decomp$index)

  geosub0<-geo[geo$index %in% id,]

  ###2 create a geolocation dataframe which consistent with our decomposition data
  rindex<-table(decomp$index)
  geo0<-data.frame(geosub0,rindex)
  ngeo<-expandRows(geo0, "Freq")

  orix<-ngeo$orix
  oriy<-ngeo$oriy
  oriz<-ngeo$oriz
  dx<-ngeo$dx
  dy<-ngeo$dy
  dz<-ngeo$dz
  refbin<-ngeo$refbin

  ##these two will be used for the deconvolution and decomposition result
  outref<-ngeo$outref
  outpeak<-ngeo$outpeak

  ####3 find corresponding decomposition parameters file
  nr<-nrow(decomp)
  peak<-decomp$t;sd<-decomp$sd;tse<-decomp$tse
  low<-peak-sd*sqrt(2*log(2))
  up<-peak+sd*sqrt(2*log(2))



  ###4 begin to calculate

  #################to calculate the peak, leading and trail edge position
  if (source=="decomposition"){
    sgeo<-matrix(NA,nr,29)
    sgeo[,1:7]<-cbind(decomp$index,decomp$pi,decomp$t,decomp$sd,decomp$pise,decomp$tse,decomp$sdse)
    sgeo[,8]<-(peak-refbin)*dx+orix
    sgeo[,9]<-(peak-refbin)*dy+oriy
    sgeo[,10]<-(peak-refbin)*dz+oriz
    sgeo[,11]<-(low-refbin)*dx+orix
    sgeo[,12]<-(low-refbin)*dy+oriy
    sgeo[,13]<-(low-refbin)*dz+oriz
    sgeo[,14]<-(up-refbin)*dx+orix
    sgeo[,15]<-(up-refbin)*dy+oriy
    sgeo[,16]<-(up-refbin)*dz+oriz

    #### to calculate the uncertianty of using peak,
    ####upper peak
    sgeo[,17]<-sgeo[,8] + 1.96*tse*dx;
    sgeo[,18]<-sgeo[,9] + 1.96*tse*dy;
    sgeo[,19]<-sgeo[,10] + 1.96*tse*dz;
    ####lower peak
    sgeo[,20]<-sgeo[,8] - 1.96*tse*dx;
    sgeo[,21]<-sgeo[,9] - 1.96*tse*dy;
    sgeo[,22]<-sgeo[,10] - 1.96*tse*dz;

    ####upper leading edge
    sgeo[,23]<-sgeo[,11] + 1.96*tse*dx;
    sgeo[,24]<-sgeo[,12] + 1.96*tse*dy;
    sgeo[,25]<-sgeo[,13] + 1.96*tse*dz;
    ####lower leading edge
    sgeo[,26]<-sgeo[,11] - 1.96*tse*dx;
    sgeo[,27]<-sgeo[,12] - 1.96*tse*dy;
    sgeo[,28]<-sgeo[,13] - 1.96*tse*dz;

    ########we need to give the return number of points i think
    ###http://stackoverflow.com/questions/20869374/create-the-frequency-count-from-a-vector-in-r
    ind<-decomp$index;rn<-ave(ind, ind, FUN = seq_along)
    sgeo[,29]<-rn
    colnames(sgeo)<- c("index","pi","t","sd","pise","tse","sdse","px","py","pz","lowx","lowy","lowz","upx","upy","upz",
                       "uncerUXpeak","uncerUYpeak","uncerUZpeak","uncerLXpeak","uncerLYpeak","uncerLZpeak",
                       "uncerUXleading","uncerUYleading","uncerUZleadig","uncerLXleading","uncerLYleading","uncerLZleading","rn")
  } else {
    sgeo<-matrix(NA,nr,17)
    sgeo[,1:7]<-cbind(decomp$index,decomp$pi,decomp$t,decomp$sd,decomp$pise,decomp$tse,decomp$sdse)
    sgeo[,8]<-(peak-refbin-outpeak+outref)*dx+orix
    sgeo[,9]<-(peak-refbin-outpeak+outref)*dy+oriy
    sgeo[,10]<-(peak-refbin-outpeak+outref)*dz+oriz

    #### to calculate the uncertianty of using peak,
    ####upper peak
    sgeo[,11]<-sgeo[,8] + 1.96*tse*dx;
    sgeo[,12]<-sgeo[,9] + 1.96*tse*dy;
    sgeo[,13]<-sgeo[,10] + 1.96*tse*dz;
    ####lower peak
    sgeo[,14]<-sgeo[,8] - 1.96*tse*dx;
    sgeo[,15]<-sgeo[,9] - 1.96*tse*dy;
    sgeo[,16]<-sgeo[,10] - 1.96*tse*dz;
    ########we need to give the return number of points i think
    ###http://stackoverflow.com/questions/20869374/create-the-frequency-count-from-a-vector-in-r
    ind<-decomp$index;rn<-ave(ind, ind, FUN = seq_along)
    sgeo[,17]<-rn
    colnames(sgeo)<-c("index","pi","t","sd","pise","tse","sdse","px","py","pz","uncerUXpeak","uncerUYpeak","uncerUZpeak","uncerLXpeak","uncerLYpeak","uncerLZpeak","rn")

  }

  return (sgeo)

}

