#' train model with new features.
#'
#' For each series in \code{dataset}, point and interval forecasting performance
#' are evaluated for all methods in \code{methods} of \code{\link{ts_forec}}
#' with regard to MASE, sMAPE, MSIS, Spread, Coverage and Upper coverage.
#' @param train_features the features for the generated time series.
#'
#'
#' @return three classifer of each ets component
#' @importFrom lightgbm lgb.Dataset
#' @importFrom lightgbm lgb.train
#' @importFrom stats filter
#' @export
train_model <- function(train_features) {
  load(system.file("extdata", "train_data.rData", package = "msETS"))
  for (i in 1:120000){
    train_features[i,"y_error"]<-train_data[[i]]$y_error
    train_features[i,"y_trend"]<-train_data[[i]]$y_trend
    train_features[i,"y_seasonality"]<-train_data[[i]]$y_seasonality
  }
  error_list<-c("A","M")
  trend_list<-c("A","Ad","N")
  seasonality_list<-c("A","M","N")
  train_features$valid_inx <- sample(c(1,0),nrow(train_features),replace = TRUE,prob = c(0.2,0.8))
  train_features$y_error<-as.factor(train_features$y_error)
  train_features$y_trend<-as.factor(train_features$y_trend)
  train_features$y_seasonality<-as.factor(train_features$y_seasonality)
  train_features$y_error[1]
  train_features$y_trend[1]
  train_features$y_seasonality[1]
  summary(train_features$y_error)
  summary(train_features$y_trend)
  summary(train_features$y_seasonality)
  train_features$y_error<-as.numeric(train_features$y_error)-1
  train_features$y_trend<-as.numeric(train_features$y_trend)-1
  train_features$y_seasonality<-as.numeric(train_features$y_seasonality)-1

  bia1 <- train_features %>% filter(valid_inx==0) %>%
    select(-y_method,-y_error,-y_trend,-y_seasonality,-valid_inx) %>% data.matrix()
  bia_error <- (train_features %>% filter(valid_inx==0))$y_error
  bia_trend <- (train_features %>% filter(valid_inx==0))$y_trend
  bia_seasonality <- (train_features %>% filter(valid_inx==0))$y_seasonality
  bia2 <- train_features %>% filter(valid_inx==1) %>%
    select(-y_method,-y_error,-y_trend,-y_seasonality,-valid_inx) %>% data.matrix()
  bia_error2 <- (train_features %>% filter(valid_inx==1))$y_error
  bia_trend2<- (train_features %>% filter(valid_inx==1))$y_trend
  bia_seasonality2 <- (train_features %>% filter(valid_inx==1))$y_seasonality
  # error
  dtrain_error <- lgb.Dataset(data = bia1,
                              label = bia_error)
  dtest_error <- lgb.Dataset.create.valid(dtrain_error,
                                          data = bia2,
                                          label = bia_error2)
  valids_error <- list(test = dtest_error)

  param_error <- list(max_depth = 10,
                      num_leaves =80, # 70/80 default,2^(max_depth)
                      min_data_in_leaf= 5,
                      learning_rate = 0.05, # smaller,slower,maybe more accurate
                      is_unbalance=TRUE, # unbalance TrainingSet
                      nthread = 3,
                      verbose = 1,
                      objective ='binary',
                      metric = c('binary_logloss', 'auc'))
  lgb_error <- lgb.train(params=param_error,
                         data=dtrain_error,
                         nrounds = 1000,
                         early_stopping_rounds = 5,
                         valids = valids_error,
                         bagging_fraction = 0.5, # random sample ratio from trainSet
                         bagging_freq = 10, # random sample freq from trainSet
                         bagging_seed = 1) # set.seed

  # trend
  dtrain_trend <- lgb.Dataset(data = bia1,
                              label = bia_trend)
  dtest_trend <- lgb.Dataset.create.valid(dtrain_trend,
                                          data = bia2,
                                          label = bia_trend2)
  valids_trend <- list(test = dtest_trend)

  param_trend <- list(max_depth = 10,
                      num_leaves =70, # 70/80 default,2^(max_depth)
                      min_data_in_leaf=10,
                      learning_rate = 0.1, # smaller,slower,maybe more accurate
                      is_unbalance=TRUE, # unbalance TrainingSet
                      nthread = 3,
                      verbose = 10,
                      objective = "multiclass",
                      metric = "multi_error",
                      num_class = 3)
  lgb_trend <- lgb.train(params=param_trend,
                         data=dtrain_trend,
                         nrounds = 1000,
                         early_stopping_rounds = 10,
                         valids = valids_trend,
                         bagging_fraction = 0.7, # random sample ratio from trainSet
                         bagging_freq = 10, # random sample freq from trainSet
                         bagging_seed = 1) # set.seed


  # seasonality

  dtrain_seasonality <- lgb.Dataset(data = bia1,
                                    label = bia_seasonality)
  dtest_seasonality <- lgb.Dataset.create.valid(dtrain_seasonality,
                                                data = bia2,
                                                label = bia_seasonality2)
  valids_seasonality <- list(test = dtest_seasonality)

  param_seasonality <- list(max_depth = 10,
                            num_leaves =70, # 70/80 default,2^(max_depth)
                            min_data_in_leaf=10,
                            learning_rate = 0.05, # smaller,slower,maybe more accurate
                            is_unbalance=TRUE, # unbalance TrainingSet
                            nthread = 3,
                            verbose = 1,
                            objective = "multiclass",
                            metric = "multi_error",
                            num_class = 3)
  lgb_seasonality <- lgb.train(params=param_seasonality,
                               data=dtrain_seasonality,
                               nrounds = 1000,
                               early_stopping_rounds = 50,
                               valids = valids_seasonality,
                               bagging_fraction = 0.7, # random sample ratio from trainSet
                               bagging_freq = 10, # random sample freq from trainSet
                               bagging_seed = 1) # set.seed
  classifers<-list("lgb_error"=lgb_error,"lgb_trend"=lgb_trend,"lgb_seasonality"=lgb_seasonality)
  return(classifers)
}

