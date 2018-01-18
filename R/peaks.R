#' peak finding function
#'
#' This function allows you to find where is the peak and return a list of TRUE and FALSE
#'    TRUE represents this location is the peak.
#'
#' @param series is the input a numeric vector.
#' @param span is the length or interval of peak finding cell, default is 3.
#' @return return a boolean type data corresponding to the numeric vector.
#' @export
#' @examples
#' w1 is numeric values represnt an waveform.
#' peaks(w1,3)
#' peaks(w2,5)

peaks <- function(series,span=3)
{
  z <- embed(series, span)
  s <- span%/%2
  v <- max.col(z, "first") == 1 + s   # take first if a tie
  result <- c(rep(FALSE,s),v)
  result <- result[1:(length(result)-s)]
  result
}

