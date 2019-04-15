#' waveformvoxel
#'
#' The function allows you to project raw waveforms into 3d voxels from hyperpointcloud with self defined resolution.
#'   For the intensity of each grid, four kinds of values were available to be used: the total number of intensity in each voxel,
#'   maximum intensity of the voxel, mean intensity of the voxel, and total intensity of the voxel.
#'
#' @param hpc the objetcs from hyperpointcloud function.
#' @param res the voxel size with x, y, and z spatial resolution. Default is 0.8*0.8*0.15.
#' @param quan The quantile of intenisty in the given grid or voxel. Defalut is NULL.

#' @return A dataframe with 7 columns including geolocation and intensities. Specifically,"index","cx","cy","length_maxi_meani_totali.1","length_maxi_meani_totali.2",
#'   "length_maxi_meani_totali.3","length_maxi_meani_totali.4").
#'   \item{index}{The index of the voxel}
#'   \item{cx}{The average x of the voxel center}
#'   \item{cy}{The average y of the voxel center}
#'   \item{cz}{The average z of the voxel center}
#'   \item{intensity.length}{The number of intensity in the voxel}
#'   \item{intensity.maxi}{The maximum intensity of waveforms in the voxel}
#'   \item{intensity.meani}{The mean intensity of waveforms in the voxel}
#'   \item{intensity.totali}{The total intensity of waveforms in the voxel}
#'   \item{...}{Percentile intensity based on the quan}
#' @import data.table
#' @import stats
#' @export
#' @references
#'   Tan Zhou, Sorin Popescu, Lonesome Malambo, Kaiguang Zhao, Keith Krause. From LiDAR waveforms
#'   to Hyper Point Clouds: a novel data product to characterize vegetation structure.
#'   Remote Sensing 2018, 10(12), 1949; https://doi.org/10.3390/rs10121949
#' @examples
#'
#' data(return)  ###import raw return waveforms
#' data(geo)  ###import corresponding reference geolocation
#'
#' #' ### you should know which columns corresponding to above column names
#' ## before run the hyperpointcloud when you used your own new datasets, this is very important step
#' colnames(geo)[2:9]<-c("x","y","z","dx","dy","dz","or","fr")
#'
#' hpr<- hyperpointcloud(waveform = return,geo = geo)
#'
#' ##beofre run waveformvoxel, we need to create hyperpointcloud first
#' ##this exampel we just used 100000 points to reduce processing time
#'
#' voxr<-waveformvoxel(hpc = hpr,res=c(1,1,0.3))
#'
waveformvoxel<-function(hpc,res=c(0.8,0.8,0.15),quan=NULL){

  #x<-hpr[,1];y<-hpr[,2];
  x<-unlist(hpc[,1]);y<-unlist(hpc[,2]);  ##when you read in fread, it will give you error, sicne the input will be list and this require the double
  z<-unlist(hpc[,3]);intensity<-unlist(hpc[,4])
  dx<-res[1];dy<-res[2];dz<-res[3]

  x.res<-seq(from = min(x,na.rm=T),to = max(x,na.rm=T)-0.3*dx, by=dx)
  y.res<-seq(from = min(y,na.rm=T),to = max(y,na.rm=T)-0.3*dy, by=dy)
  z.res<-seq(from = min(z,na.rm=T),to = max(z,na.rm=T)-0.3*dz, by=dz)

  indx<-findInterval(x, x.res)
  #newx<-x.res[data$indx]+dx/2
  indy<-findInterval(y, y.res)
  #newy<-y.res[data$indy]+dy/2

  indz<-findInterval(z, z.res)
  #newz<-z.res[data$indz]+dz/2

  index<-paste0(indx,"_",indy,"_",indz)

  dat0<-data.frame(hpc[,c(1,4)]) ##use an old matrix as data.frame will be faster?

  dat0[,1]<-index
  colnames(dat0)[1]<-"index"

  dat1<-data.frame(hpc)
  dat1[,4]<-index
  colnames(dat1)[4]<-"index"

  if (is.null(quan)){
    summary_stat<-aggregate(. ~ index, data=dat0[,c(1,2)], function(x) c(length(x),max(x),mean(x),sum(x)))

  } else {
    summary_stat<-aggregate(. ~ index, data=dat0[,c(1,2)], function(x) c(length(x),max(x),mean(x),sum(x),quantile(x,quan)))

  }
  colnames(summary_stat$intensity)[1:4]<-c("length","maxi","meani","totali")
  ###to calculate the xyz in each grid
  xyzc<-aggregate(. ~ index, data=dat1, FUN=mean)

  ##to combine data
  voxelr<-merge(xyzc,summary_stat,by="index")

  colnames(voxelr)[2:4]<-c("cx","cy","cz")
  return(voxelr)

}









