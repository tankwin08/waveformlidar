#' rawtocomposite
#'
#' The function allows you to convert point cloud after waveformvoxel or raw waveforms into composite waveforms (with vertical distribution of intensity)
#' by reducing the effect of off-naid angle of emitted laser.
#' The conversion is based on the waveform voxellization product. Four kinds of values you can chose to represent the intensity of composite waveform:
#' the number of intensity (generally is not useful), mean intensity, maximum intensity and total intensity (the last one is also not prefered in most of cases).
#'
#' @param voxr the object from the waveformvoxel.
#' @param inten_index the value (1,2,3,4,...) to represnt the intensity of composite waveforms.It is a integer from 1 to 4 and default is 2.
#'   1: the number of intensity of the voxel (generally is not useful); 2: the maximum intensity of the waveform voxel; 3: the mean intensity of the waveform voxel;
#'   4: the total intensity of voxel(the last one is also not prefered in most cases)
#'
#' @return A dataframe with first three columns including geolocation xyz of the first Non-NA intensity (Highest position) and intensities along the height bins, other non-NA values are intensities for the rest columns.
#'   \item{x}{The x position of the first Non-NA intensity or highest intensity position in one waveform}
#'   \item{y}{The y position of the first Non-NA intensity or highest intensity position in one waveform}
#'   \item{z}{The z position of the first Non-NA intensity or highest intensity position in one waveform}
#'   \item{intensity 1}{The intnesity of first height bin}
#'   \item{intensity 2}{The intensity of second height bin}
#'   \item{...}{Intensities along the height bin}
#' @import data.table
#' @export
#' @examples
#'
#' data(return)  ###import raw return waveforms
#' data(geo)  ###import corresponding reference geolocation
#' colnames(geo)[2:9]<-c("x","y","z","dx","dy","dz","or","fr")
#' ### you should know which columns corresponding to above column names before
#' ### run the hyperpointcloud when you used your own new datasets.
#'
#' hpr<-hyperpointcloud(waveform=return,geo=geo)
#'
#' ##beofre run waveformvoxel, we need to create hyperpointcloud first
#' ##this exampel we just used 100000 points to reduce processing time
#'
#' voxr<-waveformvoxel(hpc=hpr,res=c(1,1,0.3))
#' rtc<-rawtocomposite(voxr)

rawtocomposite<-function(voxr,inten_index=2){

  ###to get the sperate index for xyz
  index0<-voxr$index
  spindex<-strsplit(index0,"_")


  u<-as.numeric(unlist(spindex))
  nr<-nrow(voxr)
  index<-matrix(u,nr,3,byrow=TRUE)
  fdat<-data.frame(voxr,index)

  #colnames(fdat)[5:11]<-c("num","maxi","meani","totali","indx","indy","indz")

  colnames(fdat)[6:8]<-c("indx","indy","indz")
  ####determine the sscale
  roundup <- function(x) 10^ceiling(log10(x))
  mr<-max(fdat$indx,na.rm=T)
  tx<-roundup(mr)

  rowindex<-fdat$indx*tx+fdat$indy

  fdat<-data.frame(fdat,rowindex)

  ###detemine the number of row you will use
  fdat<-fdat[order(fdat$rowindex),]
  #####http://stackoverflow.com/questions/10150579/adding-a-column-to-a-data-frame

  dn<-diff(fdat$rowindex)
  ###detect the one differnt from others
  po<- dn>0
  pocum<-cumsum(po)+1
  cumind<-c(1,pocum)
  fdat<-data.frame(fdat,cumind)

  ###how many columns in the dataset
  zin<-unique(fdat$indz);zl<-length(zin)

  ###how many rows we have in the new dataset
  rown<-length(unique(rowindex))
  #grind<-sort(unique(rowindex))
  ###then we hadrown rows and coln columns
  ##build a new matrix including the starting points' xyz, this why we need to add 3
  wfdat<-matrix(NA,rown,(zl+3))
  ###the higher zin[ii], the higher z
  ###to assign highest intensity's xyz, we need to
  zin<-zin[order(zin)]
  for (ii in 1:zl){  ###here this oder is important to assign the highest point in the top
    tind<-which(fdat$indz==zin[ii])
    ###we also need the xyz of the highest in this voxel
    sdat<-fdat[tind,]
    intensity<-sdat$intensity[,inten_index]
    rowi<-sdat$cumind
    coli<-(zl-zin[ii])+4  ###here we put higher zin value into lower column to make sure it start fromthe highest point
    wfdat[rowi,coli]<-intensity

    pos<-unlist(sdat[,2:4])
    posm<-matrix(pos,nrow(sdat),3)

    wfdat[rowi,1:3]<-posm
    ###we will use the zin to determine the position of column and xy to determine

  }

  return(wfdat)

}









