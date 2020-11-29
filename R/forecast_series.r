#' forecast the coming time series
#'
#' @param dataset A list the elements having a \code{ts} object with the name \code{x}.
#' @param method the way you want to forecast the series(aic,feature based or combine the two methods).
#' @param features NULL if you just use the model package provides.
#' @param error_model the error model.
#' @param trend_model the trend model.
#' @param seasonality_model the seasonality model.
#'
#'
#' @return A list with the elements having the following structure
#'
#' @importFrom lightgbm lgb.load
#' @importFrom stats frequency
#' @importFrom stats predict
#' @importFrom forecast ets
#' @importFrom forecast forecast
#' @export
forecast_series <- function(dataset, method = c("combine","aic","fb"),
                           features = NULL, error_model = NULL,
                           trend_model = NULL, seasonality_model = NULL) {
  error_list<-c("A","M")
  trend_list<-c("A","Ad","N")
  seasonality_list<-c("A","M","N")
  if(is.null(error_model)){
    file<-system.file("extdata", "lgb_error.model", package = "msETS")
    lgb_error<-lgb.load(file)
  }
  else
    lgb_error<-error_model
  if(is.null(trend_model)){
    file<-system.file("extdata", "lgb_trend.model", package = "msETS")
    lgb_trend<-lgb.load(file)
  }
  else
    lgb_trend<-trend_model
  if(is.null(seasonality_model))
    lgb_seasonality<-lgb.load(system.file("extdata", "lgb_seasonality.model", package = "msETS"))
  else
    lgb_seasonality<-seasonality_model
  if(is.null(features)){
    ts_sample<-THA_features(dataset)
    features<-data.frame(ts_sample[[1]]$features)
    for (i in 2:length(ts_sample)){
      features<-rbind(features,ts_sample[[i]]$features)
    }
  }
  pred_error <- predict(lgb_error,data.matrix(features), reshape = TRUE)
  pred_trend <- predict(lgb_trend,data.matrix(features), reshape = TRUE)
  pred_seasonality <- predict(lgb_seasonality,data.matrix(features), reshape = TRUE)
  predict_res<-c()
  for (i in 1:length(dataset)){
    ets_method<-paste(error_list[round(pred_error[i])+1],
                      trend_list[which.max(pred_trend[i,])],
                      seasonality_list[which.max(pred_seasonality[i,])],sep = "")
    predict_res<-cbind(predict_res,ets_method)
  }
  result<-list()
  foreach (i = 1:length(dataset))%do%{
    tdata<-ts_sample[[i]]
    if (tdata$period == "Yearly" | tdata$period == "YEARLY"){
      f<-1
    }else if(tdata$period == "Quarterly" | tdata$period == "QUARTERLY"){
      f<-4
    }else if(tdata$period == "Monthly" | tdata$period == "MONTHLY"){
      f<-12
    }
    timeseries <- tdata$x
    if (tdata$period=="Yearly" | tdata$period == "YEARLY"){
      if (substring(predict_res[i],nchar(predict_res[i]),nchar(predict_res[i]))!="N"){
        predict_res[i]<-paste(substring(predict_res[i],1,nchar(predict_res[i])-1),"N",sep = "")
      }
    }
    if (substring(predict_res[i],3,3)=="d"){
      npars <- 9L
      if (substring(predict_res[i],4,4) != "N") {
        npars <- npars + f
      }
      if (npars+1>tdata$n){
        m<-paste(substring(predict_res[i],1,2),substring(predict_res[i],4,4),sep = "")
        ts_model<-try(ets(timeseries,model = m, damped =FALSE,restrict = FALSE))
      }
      else{
        m<-paste(substring(predict_res[i],1,2),substring(predict_res[i],4,4),sep = "")
        ts_model<-try(ets(timeseries,model = m, damped =TRUE,restrict = FALSE))

      }
      if ('try-error' %in% class(ts_model)){
        print(m)
      }
    }else{
      ts_model<-try(ets(timeseries,model = predict_res[i],damped = FALSE,restrict = FALSE))
      if ('try-error' %in% class(ts_model)){
        print(predict_res[i])
      }
    }
    if(method=="aic"){
      ts_model1<-ets(timeseries,ic="aic")
      f_result1<-forecast(ts_model1,tdata$h)
      result<-c(result,list((f_result1$mean+f_result$mean)/2))
    }else if(method=="fb"){
      f_result<-forecast(ts_model,tdata$h)
      result<-c(result,list(f_result$mean))
    }else{
      ts_model1<-ets(timeseries,ic="aic")
      f_result1<-forecast(ts_model1,tdata$h)
      f_result<-forecast(ts_model,tdata$h)
      result<-c(result,list((f_result1$mean+f_result$mean)/2))
    }
    return(result)
  }
}

