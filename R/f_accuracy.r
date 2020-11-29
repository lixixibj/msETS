#' Calculate the accuracy of time series
#'
#' @param insample series that input the model.
#' @param outsample hope to predict the phase of the series.
#' @param forecasts the result of the model.
#' @param method which metric to calculate the accuracy.
#'
#'
#' @return A list with the elements having the following structure
#'
#' @importFrom stats is.ts
#' @importFrom stats frequency
#' @export
f_accuracy<-function(insample,outsample,forecasts, method = c("mase","smape")){
  method <- match.arg(method)
  mase_cal <- function(insample, outsample, forecasts) {
    stopifnot(is.ts(insample))
    #Used to estimate MASE
    frq <- frequency(insample)
    forecastsNaiveSD <- rep(NA,frq)
    for (j in (frq+1):length(insample)){
      forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
    }
    masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)

    outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
    mase <- (abs(outsample-forecasts))/masep
    return(mase)
  }

  smape_cal <- function(outsample, forecasts) {
    #Used to estimate sMAPE
    outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
    smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
    return(smape)
  }
  if(method == "smape")
    return(mase_cal(insample,outsample,forecasts))
  else
    return(smape_cal(insample,outsample,forecasts))
}

