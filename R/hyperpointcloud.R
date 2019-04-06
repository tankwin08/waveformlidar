#' hyperpointcloud
#'
#' The function allows you to convert every waveform intensities into points which formed hyper point cloud. This can help
#'   you visualize the waveform data in an efficient way. It will generate a big dataset which may reach the memory of computer's RAM.
#'   Thus, it requires to split the study regions into smaller tiles when you have large study sites.
#'
#' @param waveform the raw waveform data.
#' @param geo the reference geolocation that corresponds to the raw waveform data which requres to have the same number of rows as the waveform data.

#' @return A dataframe with 4 columns including xyz geolocation and intensity.
#'   \item{x}{The x position of one waveform intensity}
#'   \item{y}{The y position of one waveform intensity}
#'   \item{z}{The z position of one waveform intensity}
#'   \item{intensity}{The position's intensity}
#' @export
#' @references
#' 	 Zhou, Tan, Sorin Popescu, Lonesome Malambo, Kaiguang Zhao, and Keith Krause. From LiDAR 
#'   waveforms to Hyper Point Clouds: a novel data product to characterize vegetation structure. 
#'   Remote Sensing 10, no. 12 (2018): 1949.
#' @examples
#'
#' data(return)  ###import raw return waveforms
#' data(geo)  ###import corresponding reference geolocation
#' geo$index<- NULL
#' colnames(geo)[1:8]<- c("x","y","z","dx","dy","dz","or","fr")
#' ### you should know which columns corresponding to above column names
#' ### before run the hyperpointcloud when you used your own new datasets
#' hpr<- hyperpointcloud (waveform = return,geo = geo)

hyperpointcloud<-function(waveform,geo){
    waveform[waveform==0]<-NA
    nr<-nrow(waveform)
    coll<-ncol(waveform)
    orix<-geo$x
    oriy<-geo$y
    oriz<-geo$z
    dx<-geo$dx
    dy<-geo$dy
    dz<-geo$dz
    refbin<-geo$fr

    ndat<-matrix(NA,coll*nr,4)

    for (i in 1:coll){
      s<-1+(i-1)*nr;e<-nr*i
      nbx<-orix + dx*(i-refbin)
      ndat[s:e,1]<-nbx
      nby<-oriy + dy*(i-refbin)
      ndat[s:e,2]<-nby
      nbz<-oriz + dz*(i-refbin)
      ndat[s:e,3]<-nbz
      intensity<-waveform[[i]]
      ndat[s:e,4]<-intensity
    }
    ndat<-ndat[!is.na(ndat[,4]),]
    colnames(ndat)<-c("x","y","z","intensity")
    return (ndat)

  }









