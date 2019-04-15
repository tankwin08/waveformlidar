#' 500 sample LiDAR waveforms from Harvard Forest provided by National Ecological Observatory Networks (NEON).
#' More details can be found in	Tan Zhou*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017.
#' Gold-A novel deconvolution algorithm with optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017): 131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021
#'
#' A dataset containing the return waveforms with intensity along the path. The zero padding was used
#' to make sure every waveforms has the same length. In this case, 0 means no recorded intensity.
#'
#' @format A data frame with 500 rows and 208 variables:
"return"


#' 500 corresponding outgoing pulses.
#'
#' This dataset should have the same number of rows as the return pulses, which is the outgoing
#' pulse for each return waveform pulse.The zero padding was used to make sure every waveforms has the same length. In this case, 0 means no recorded intensity.
#'
#'
#' @format A data frame with 500 rows and 100 variables:
"outg"


#' prototype system impulse
#'
#' This data was generated from a hard ground target with a mirror angle close to nadir
#' and its corresponding outgoing waveform. Generally one study site only need one system impulse which is
#' mainly for calibration purpose.
#'
#' @format A data frame with 100 rows and 1 column.
#' \describe{
#'  \item{V1}{The intensity of the system impulse}
#' }
#'
"imp"


#' Outgoing pulse corresponding to the system impulse
#'
#' This dataset is the corresponding outgoing pulse of the system impulse
#'
#' @format A vector with the length of 100. The zero padding was used to make sure every waveforms has the same length. In this case, 0 means no recorded intensity.
#'
#' \describe{
#'   \item{outgoing pulse}{}
#' }

"imp_out"

#' The reference geolocation of the return waveforms.
#'
#' This dataset should have the same number of rows of the return waveforms (return).
#' It is generally coming with waveform data and provided by the data provider. The number of rows should be
#' the same as the return waveform or pulse (return).
#'
#'
#' @format A data frame with 500 rows and 17 variables.
#' \describe{
#'  \item{index}{index to connect return waveform and geolocation}
#'  \item{\code{x}}{Easting of first return}
#'  \item{\code{y}}{Northing  of first return}
#'  \item{\code{z}}{Height of first return}
#'  \item{\code{dx}}{the position change per nanasecond at x direction}
#'  \item{\code{dy}}{the position change per nanasecond at y direction}
#'  \item{\code{dz}}{the position change per nanasecond at z direction}
#'  \item{\code{or}}{outgoing pulse reference bin location (leading edge 50th point of the outgoing pulse)}
#'  \item{\code{fr}}{first return reference bin location (leading edge 50th point of the first return)}
#'  \item{\code{V9}}{Easting of Return Bin0}
#'  \item{\code{V10}}{Northing of Return Bin0}
#'  \item{\code{V11}}{Height of Return Bin0}
#'  \item{\code{V12}}{the position change per nanasecond at x direction}
#'  \item{\code{V13}}{the position change per nanasecond at y direction}
#'  \item{\code{V14}}{the position change per nanasecond at z direction}
#'  \item{\code{V15}}{outgoing pulse peak bin location}
#'  \item{\code{V16}}{return bin0 location}
#' }
"geo"

#' Results of decomposition using Gaussian model
#'
#' This dataset contained the result from the Gaussian decompositon.
#'
#' @format A data frame with 1000 rows and 7 variables
#' \describe{
#'   \item{index}{index of waveform, which can tell which rows or results belongs to specific waveform}
#'   \item{A}{the amplitude of one waveform componment (A)}
#'   \item{u}{the time location corresponds to the amplitude for one waveform componment}
#'   \item{sigma}{the echo width of one waveform componment}
#'   \item{A_std}{the standard error of A, which can be used for uncertainty analysis}
#'   \item{u_std}{the standard error of u, which can be used for uncertainty analysis}
#'   \item{sig_std}{the standard error of sigma, which can be used for uncertainty analysis}
#' }
"decom_result"

#' Results of using the deconvolution and decomposition method.
#'
#' This dataset contained the results using deconvolution and then decompostion method as described in the
#' Tan Zhou*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017.
#' Gold-A novel deconvolution algorithm with optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017): 131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021
#'
#' @format A data frame with 1000 rows and 7 variables
#' \describe{
#'   \item{index}{index of waveform, which can tell which rows or results belongs to specific waveform}
#'   \item{A}{the amplitude of one waveform componment (A)}
#'   \item{u}{the time location corresponds to the amplitude for one waveform componment}
#'   \item{sigma}{the echo width of one waveform componment}
#'   \item{A_std}{the standard error of A, which can be used for uncertainty analysis}
#'   \item{u_std}{the standard error of u, which can be used for uncertainty analysis}
#'   \item{sig_std}{the standard error of sigma, which can be used for uncertainty analysis}
#' }
"decon_result"


#' A boundary shapefile, polygon
#'
#' This data is manily to test the fucntion waveformclip.
#'
#' @format A shapefile containing a small boudary of the Harvard Forest.
"shp_hf"
