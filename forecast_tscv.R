#---Load Libraries ---------------------------------------------------------------------
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(tidyverse)
library(ggdist)
library(ggthemes)



#------------Load tsibble ---------------------------------------------------------------------------

vaccine_administrated_tsb <- read_rds("data/vaccine_administrated_tsb.rds")

#-----------transform multiple time series to one time series -------------------------------------

vaccine_total <- vaccine_administrated_tsb |> index_by(month) |> summarise(dose_adminstrated = sum(dose_adminstrated))



#---------- define forecast horizon and also size of test data ------------------------------------
forecast_horizon <- 12
percentage_test <- 0.2

#--------- split data into train and test data sets ----------------------------------------------

test <- vaccine_total |> filter_index(as.character(max(vaccine_total$month) - round(percentage_test*length(unique(vaccine_total$month)))+1) ~ .)

train <- vaccine_total |> filter_index(. ~ as.character(max(vaccine_total$month) - (round(percentage_test*length(unique(vaccine_total$month))))))

nrow(test)

#------------- make multiple test data sets using cross validation method ---------------------------

train_tscv <- vaccine_total |> filter_index(. ~ as.character(max(vaccine_total$month) - (forecast_horizon))) |>
     stretch_tsibble(.init = length(unique(train$month)), .step = 1)

train_tscv

#-------- fit data to different models using the cross validation test data----------------------------------------------------------

fit <- train_tscv |> model(
  average = MEAN(dose_adminstrated),
  snaive = SNAIVE(dose_adminstrated),
  exponential_smoothing = ETS(dose_adminstrated),
  arima = ARIMA(dose_adminstrated),
  regression = TSLM(dose_adminstrated)
  
)

#--- forecast using the different models ------------------------------------------------------------

forecast_vaccine_total_tscv <- fit |> forecast(h = forecast_horizon)

#---- test accuracy of the different models ------------------------------------------------------

forecast_accuracy_tscv <- forecast_vaccine_total_tscv |> accuracy(vaccine_total,
                                                             measures = list(point_accuracy_measures,
                                                                             interval_accuracy_measures,
                                                                             distribution_accuracy_measures))


forecast_accuracy_tscv |> select(.model, RMSE,MAE,MASE,RMSSE, winkler,CRPS)

#----------- train/fit using actual data into the best model--------------------------------------------------------

fit_best <- vaccine_total |> model(expontential_smoothing = ETS(dose_adminstrated))

#forecast for the horizon ----------------------------------------------------------

best_forecast <- fit_best |> forecast(h = forecast_horizon)

#make visuals

ggplot(data = best_forecast, mapping = aes(x = month, ydist = dose_adminstrated))+
  stat_halfeye()+
  geom_line(data =filter_index(vaccine_total, "2020 Apr" ~ .), mapping = aes(x = month, y = dose_adminstrated, colour ="Data"))+
  ggthemes::theme_few()

#Extract data to csv-----------------------------------------------------------------

best_forecast_interval <- best_forecast |> hilo(level = 90) |> unpack_hilo(`90%`)

write.csv(best_forecast_interval,"best_forecast_interval.csv")


#--------------------- residual diagnostic -------------------------------

fit_best |> gg_tsresiduals()
