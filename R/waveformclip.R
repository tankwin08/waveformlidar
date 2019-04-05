#' waveformclip
#'
#' The function allows you to select waveforms of your interest from the whole waveform dataset based on the shpfile(s) or
#'   geoextent including xmin,xmax,ymin,ymax.
#'
#' @param waveform The raw waveform data containing intensities and index of waveform.
#' @param geo The reference geolocation that corresponds to the raw waveform data which has the same number of rows as the waveform data.
#' @param shp The region of interst which tells the regions to clip, it require has the same projetct coordinate system (UTM is prefered) as the geo data.
#' @param geoextent Another way to clip waveform using xmin, xmax, ymin, ymax. Please note you should specify the parameter in this order. Defalut is NULL.

#' @return For using shapefile, it will return a dataframe with shapefile index (this will be useful for multipolygons) and corresponding select waveforms in each sub shapefiles.
#'   For the geoextent, it will return the selected waveforms in the extent and corresponind selected index of geo data.
#' @import rgdal
#' @import sp
#' @import rgeos
#' @export
#' @examples
#'
#' data(return)  ###import raw return waveforms
#' data(geo)  ###import corresponding reference geolocation
#' data(shp_hf)  ###import shpefile
#'##the next step is required, since everybody's georeference data maybe
#'##a bit difference, you need to adjust by yourself when you implement the function.
#'
#'### you need to assign x and y columns at least to make the function run properly
#' colnames(geo)[2:9]<-c("x","y","z","dx","dy","dz","or","fr")
#'
#' ##use shp file
#' ##this step is required. Mainly to identify index of selected waveforms from original datasets
#' waveform<-cbind(waveformindex=1:nrow(return),return)
#' geo<-geo
#' shp<-shp_hf
#' swre<-waveformclip(waveform,geo,shp)
#'
#' ###use geoextent
#' swre1<-waveformclip(waveform,geo,geoextent=c(731126,731128,4712678,4712698))



waveformclip<-function(waveform,geo,shp,geoextent=c()){
  waveform1<-waveform[,-1]
  ll<-apply(waveform1,1, wavelen)
  x<-geo$x + geo$dx*(round(ll/2)-geo$fr)  ##use the middle point to represent the waveform position
  y<-geo$y + geo$dy*(round(ll/2)-geo$fr)

  if (is.null(geoextent)){
    xy<-data.frame(x,y)
    pxy<-SpatialPoints(xy)
    proj4string(pxy)<-proj4string(shp) # transform CRS
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
