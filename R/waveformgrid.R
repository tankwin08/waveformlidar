#' waveformgrid
#'
#' The function allows you to project raw waveforms into 2d grids (can be xy, yz, and xz) with self defined resolution.
#'   For the values of each grid, four kinds of values were available to be used: the total number of intensity in each grid,
#'   maximum intensity of the grid, mean intensity of the grid, and total intensity of the grid.
#'
#' @param hpc The hyper point cloud generated from the raw waveform data with hyperpointcloud function or
#' the data with the same structure as the hyperpointcloud object.
#' @param waveform The raw waveform data with only intensities.
#' @param geo The reference geolocation that corresponds to the raw waveform data which requres has the same row as the waveform data.
#'            At least three colomns should be assigned: original x, y and first return reference bin location (fr,leading edge 50% point of the first return)).
#'            You can see detailed description of the columns in geotransform.
#' @param quan The quantile of intenisty in the given grid or voxel. Defalut is NULL.
#' @param res The grid size with x and y spatial resolution. Default is 0.8*0.8, res=c(0.8,0.8).
#' @param method There are two methods c("HPC","Other") to project the waveforms into the 2d surface. Default ("HPC") is to use the HPC as the input data. The waveform and geo will be NULL.
#' "Other" is to use the raw waveform and corresponding reference geolcation data to generate the intensities in each grid, and hpc should be NULL.

#' @return A dataframe with 7 columns including geolocation (center of the grid) and intensities. Specifically, these 7 columns are "index","cx","cy","length_maxi_meani_totali.1","length_maxi_meani_totali.2",
#'   "length_maxi_meani_totali.3","length_maxi_meani_totali.4").
#'   \item{index}{The index of the grid}
#'   \item{cx}{The mean x of the grid center }
#'   \item{cy}{The mean y of the grid center}
#'   \item{intensity.length or value.length}{The number of intensity in the grid. intensity.length is for the HPC method and value.intensity for the others}
#'   \item{intensity.maxi or value.maxi}{The maximum intensity of waveforms in the grid}
#'   \item{intensity.meani or value.meani}{The mean intensity of waveforms in the grid}
#'   \item{intensity.totali or value.totali}{The total intensity of waveforms in the grid}
#'   \item{...}{Percentile intensity based on the quan}
#'   By assigning differnt columns as x and y, the users can project waveforms into the xy, xz, yz surfaces.
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
#' ##if your geo data didn't the same column names as the follwoing one, you need to change it to
#' #it to the same column names. One thing you need to figure out is the corresponding column numbers
#' colnames(geo)[1:8]<-c("x","y","z","dx","dy","dz","or","fr")
#' ###at least you should know which columns corresponding to x,y and fr before run the waveformgrid
#' ##using the raw data
#' grid_re<-waveformgrid(waveform=return,geo=geo,res=c(0.8,0.8),method = "Other")
#'
#' ##using the hpc object
#' hpc<-hyperpointcloud(waveform=return,geo=geo)
#'
#' hpcgrid<-waveformgrid(hpc=hpc,res=c(1,1))



waveformgrid<-function(hpc,waveform=NULL,geo=NULL, quan=NULL,res=c(0.8,0.8),method = "HPC"){
  if (method == "HPC"){
    x<-unlist(hpc[,1]);y<-unlist(hpc[,2]);  ##when you read in fread, it will give you error, sicne the input will be list and this require the double
    #z<-hpr[,3];
    intensity<-hpc[,4]
    dx<-res[1];dy<-res[2];#dz<-res[3]

    x.res<-seq(from = min(x,na.rm=T),to = max(x,na.rm=T)-0.3*dx, by=dx)
    y.res<-seq(from = min(y,na.rm=T),to = max(y,na.rm=T)-0.3*dy, by=dy)
    #z.res<-seq(from = min(z,na.rm=T),to = max(z,na.rm=T)-0.3*dz, by=dz)

    indx<-findInterval(x, x.res)
    #newx<-x.res[data$indx]+dx/2
    indy<-findInterval(y, y.res)
    #newy<-y.res[data$indy]+dy/2

    #indz<-findInterval(z, z.res)
    #newz<-z.res[data$indz]+dz/2

    index<-paste0(indx,"_",indy)

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
    xyc<-aggregate(. ~ index, data=dat1, FUN=mean)

    ##to combine data
    gridr<-merge(xyc,summary_stat,by="index")
    gridr$z<-NULL
    #gridr<-data.table(gridr)


    colnames(gridr)[2:3]<-c("cx","cy")

  } else{
    waveform<-data.table(waveform)
    geo<-data.table(geo)
    ll<-apply(waveform,1, wavelen)
    waveform[waveform==0]<-NA
    x<-geo$x + geo$dx*(round(ll/2)-geo$fr)  ##use the middle point to represent the waveform position and further the griding position
    y<-geo$y + geo$dy*(round(ll/2)-geo$fr)

    data<-cbind(x,y,waveform)
    nc<-ncol(data)
    dx<-res[1];dy<-res[2]

    x.res<-seq(from = min(x,na.rm=T),to = max(x,na.rm=T)-0.3*dx, by=dx)
    y.res<-seq(from = min(y,na.rm=T),to = max(y,na.rm=T)-0.3*dy, by=dy)

    data$indx<-findInterval(data$x, x.res)
    data$newx<-x.res[data$indx]+dx/2
    data$indy<-findInterval(data$y, y.res)
    data$newy<-y.res[data$indy]+dy/2

    data$index<-paste0(data$indx,"_",data$indy)

    tc<-nc+5
    ###select data to perform gridding
    acns<-colnames(data);
    sns<-acns[c(3:nc,tc)] ###here we just need intensity and index
    #ind1<-c(3:nc,tc)

    #datm<-data[,..sns]  ###for list or data.table
    datm<- data[,sns,with = FALSE]

    ###to get the xy geolocation
    sns1<-acns[c(1,2,tc)]
    #dat1<-data[,..sns1]
    dat1<- data[,sns1, with = FALSE]
    dat0<-melt(datm,id="index",na.rm=TRUE)
    ###calculate in each grid, how many intenisty has, the mean intensity, max intensity, and total intensity
    if (is.null(quan)){
      summary_stat<-aggregate(. ~ index, data=dat0[,c(1,3)], function(x) c(length(x),max(x),mean(x),sum(x)))

    } else {
      summary_stat<-aggregate(. ~ index, data=dat0[,c(1,3)], function(x) c(length(x),max(x),mean(x),sum(x),quantile(x,quan)))

    }


    colnames(summary_stat$value)[1:4]<-c("length","maxi","meani","totali")
    ###to calculate the xy in each grid
    xyc<-aggregate(. ~ index, data=dat1, FUN=mean)

    ##to combine data
    gridr<-merge(xyc,summary_stat,by="index")
    #colnames(gridr$value)<-c("length","maxi","meani","totali")
    colnames(gridr)[2:3]<-c("cx","cy")
  }

  return(gridr)

}


