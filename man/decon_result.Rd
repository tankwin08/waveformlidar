% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{decon_result}
\alias{decon_result}
\title{Results of using the deconvolution and decomposition method.}
\format{
A data frame with 1000 rows and 7 variables
\describe{
  \item{index}{index of waveform, which can tell which rows or results belongs to specific waveform}
  \item{A}{the amplitude of one waveform componment (A)}
  \item{u}{the time location corresponds to the amplitude for one waveform componment}
  \item{sigma}{the echo width of one waveform componment}
  \item{A_std}{the standard error of A, which can be used for uncertainty analysis}
  \item{u_std}{the standard error of u, which can be used for uncertainty analysis}
  \item{sig_std}{the standard error of sigma, which can be used for uncertainty analysis}
}
}
\usage{
decon_result
}
\description{
This dataset contained the results using deconvolution and then decompostion method as described in the
Tan Zhou*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017.
Gold-A novel deconvolution algorithm with optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017): 131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021
}
\keyword{datasets}
