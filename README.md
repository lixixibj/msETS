`msETS`
========

An ETS model components selection framework considering local and global information of time series.

The R package `msETS` provides implementations of the ETS model component selection, and you can also use our generated data to build your own classifie, see our [paper](https://github.com/q1225634819/msETS) for the details.

Installation
------------

You can install the package `fuma` from [GitHub Repository](https://github.com/q1225634819/msETS) with:

``` r
devtools::install_github("q1225634819/msETS")
```
The structure of the project
----------------------------
![](project-structure.png)

Usage
-----

This part explains how to reproduce the results for our working paper. The feature-based and information criterion framework for ETS model selection is applied in this part to obtain the forecasts and prediction intervals of M4 dataset for the confidence level 95\%.

### The reference dataset and test dataset

To forecast M4 dataset, reference dataset that originates from the same population with the M4 dataset in feature spaces is generated by [GRATIS](https://arxiv.org/abs/1903.02787). [Kang, Hyndman & Li (2020)](https://onlinelibrary.wiley.com/doi/full/10.1002/sam.11461) demonstrated the superior diversity and coverage of the generated time series compared with M4 dataset in two-dimensional feature spaces.

``` r
# packages we need
library(msETS)
library(M4comp2018)
library(parallel)
library(doParallel)
library(foreach)
ncores <- detectCores()

# summarize the distribution of sample size on the M4 dataset
dataset <- M4
```

### Feature extraction for M4 data

We use THA_features to get the features of each time series.

``` r
# get the features of time series
dataset<-THA_features(dataset, n.cores=ncores)
test_features<-data.frame(dataset[[1]]$features)
for (i in 2:length(dataset)){
  test_features<-rbind(test_features,dataset[[i]]$features)
}
```

### Prediction of M4 data

``` r
# get the forecast result of each series
results <- forecast_series(dataset, "combine", features = test_features)

# calculate the accuracy of series
Yearly=data.frame()
Monthly=data.frame()
Quarterly=data.frame()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)
foreach (i = 1:length(dataset))%do%{
  tdata<-dataset[[i]]
  mase<-f_accuracy(tdata$x, tdata$xx, results, method="mase")
  if (tdata$period == "Yearly"){
    Yearly[nrow(Yearly)+1,"1"]<-mase[1]
    Yearly[nrow(Yearly),"1-2"]<-error_horizons(mase,1,2)
    Yearly[nrow(Yearly),"3-4"]<-error_horizons(mase,3,4)
    Yearly[nrow(Yearly),"5-6"]<-error_horizons(mase,5,6)
    Yearly[nrow(Yearly),"1-6"]<-error_horizons(mase,1,6)
  }else if(tdata$period == "Quarterly"){
    Quarterly[nrow(Quarterly)+1,"1"]<-mase[1]
    Quarterly[nrow(Quarterly),"1-3"]<-error_horizons(mase,1,3)
    Quarterly[nrow(Quarterly),"4-6"]<-error_horizons(mase,4,6)
    Quarterly[nrow(Quarterly),"7-8"]<-error_horizons(mase,7,8)
    Quarterly[nrow(Quarterly),"1-8"]<-error_horizons(mase,1,8)
  }else if(tdata$period == "Monthly"){
    Monthly[nrow(Monthly)+1,"1"]<-mase[1]
    Monthly[nrow(Monthly),"1-6"]<-error_horizons(mase,1,6)
    Monthly[nrow(Monthly),"7-12"]<-error_horizons(mase,7,12)
    Monthly[nrow(Monthly),"13-18"]<-error_horizons(mase,13,18)
    Monthly[nrow(Monthly),"1-18"]<-error_horizons(mase,1,18)
  }
}
```

### Train your own classifier

You can also train your own model if you get some other features using our method.

``` r
# get our data to extract features
data <- ge_data()

# train your own model
model_list<-train_model(your_own_features)
error<-model_list$lgb_error
trend<-model_list$lgb_trend
seasonality<-model_list$lgb_seasonality
```



References
----------


