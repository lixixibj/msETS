#' get the generated dataset
#'
#' @return A list with the elements having the following structure
#' @export
ge_data <- function() {
  load(system.file("extdata", "train_data.rData", package = "msETS"))
  return(train_data)
}
