#' waveformclip: clip waveforms based on shpfile or geoextent.
#'
#' The function allows you to select waveforms of your interest from the whole waveform dataset based on the shpfile(s) or
#'   geoextent including xmin,xmax,ymin,ymax.
#'
#' @param waveform the raw waveform data with intensities only.
#' @param geo the reference geolocation that corresponds to the raw waveform data which requires has the same row as the waveform data.
#' @param shp the region of interst which tells the regions to clip, it require has the same projetct coordinate system (UTM is prefered) as the geo data.
#' @param geoextent another way to clip waveform using xmin, xmax, ymin, ymax. Please note you should specify the parameter in this order. Defalut is NULL and
#'   we prefer to use shpfile to clip waveform.

#' @return For using shpfile to clip, it will return a dataframe with shpfile index and corresponding select waveforms in each sub shpfiles.
#'   For the geoextent, it will return the selected waveforms in the extent.
#' @import raster
#' @import data.table
#' @import rgdal
#' @import sp
#' @import rgeos
#' @export
#' @examples
#'
#' data(sj_wave)  ###import raw return waveforms
#' data(sj_geo)  ###import corresponding reference geolocation
#' data(shp)  ###import shpefile
#'##the next step is required, since everybody's georeference data maybe a bit difference, you need to adjust by yourself when you implement the function.
#' colnames(sj_geo)[1:8]<-c("x","y","z","dx","dy","dz","or","fr") ###here at least you need to assign x and y columns at least to make the function run properly
#'
#' ##use shp file
#' waveform<-cbind(waveformindex=1:nrow(sj_wave),sj_wave)  ##this step is required and can hep you to identify index of selected waveforms from original datasets
#' geo<-sj_geo
#' shp<-shp
#' swre<-waveformclip(waveform,geo,shp)
#'
#' ###use geoextent
#' swre1<-waveformclip(waveform,geo,geoextent=c(256830,256840,4110810,4110830))



waveformclip<-function(waveform,geo,shp,geoextent=c()){
  waveform1<-waveform[,-1]
  ll<-apply(waveform1,1, wavelen)
  x<-geo$x + geo$dx*(round(ll/2)-geo$fr)  ##use the middle point to represent the waveform position
  y<-geo$y + geo$dy*(round(ll/2)-geo$fr)

  if (is.null(geoextent)){
    xy<-data.frame(x,y)
    pxy<-SpatialPoints(xy)
    crs(pxy)<-crs(shp) # transform CRS
    re<-over(shp,pxy,returnList = TRUE)
    ind<- unlist(re)
    shpindex<-rep(seq_along(re), sapply(re, length))
    swaveform<- cbind(shpindex,waveform[ind,])
  } else {

    ind<-which (x >= geoextent[1] & x<= geoextent[2] & y >= geoextent[3] & y<= geoextent[4])
    swaveform<- waveform[ind,]
  }

  return(swaveform)

}
