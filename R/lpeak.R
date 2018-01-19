#' peak finding function
#'
#' This function allows you to locate where are peaks of a waveform and return a list of TRUE and FALSE
#'    TRUE represents this location is the peak.
#'
#' @param series is the input a numeric vector.
#' @param span is the length or interval of peak finding cell, default is 3.
#' @return return a boolean type data corresponding to the numeric vector.
#' @export
#' @examples
#' data(return)
#' w1<-return[1,]
#' #w1 is numeric values represnt an waveform.
#' lpeak(w1,3)
#' w2<-return[3,]
#' lpeak(w2,5)

lpeak <- function(series,span=3)
{
  series<-as.numeric(series)
  z <- embed(series, span)
  s <- span%/%2
  v <- max.col(z, "first") == 1 + s   # take first if a tie
  result <- c(rep(FALSE,s),v)
  result <- result[1:(length(result)-s)]
  result
}

