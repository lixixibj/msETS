#' get the accuracy of certain section
#'
#' @param error A list of the error.
#' @param start start of the section you want to choose (start with 1).
#' @param end end of the section you want to choose (start with 1).
#'
#'
#' @return average of the certain section
#' }
error_horizons <- function(error, start, end) {
  error_list<-error[start:end]
  value<-mean(error_list)
  return(value)
}