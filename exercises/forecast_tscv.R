library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(tidyverse)
library(ggdist)
library(ggthemes)

vaccine_administrated_tsb <- read_rds("data/vaccine_administrated_tsb.rds")

vaccine_total <- vaccine_administrated_tsb |> index_by(month) |> summarise(dose_adminstrated = sum(dose_adminstrated))

forecast_horizon <- 12
percentage_test <- 0.2

test <- vaccine_total |> filter_index(as.character(max(vaccine_total$month) - round(percentage_test*length(unique(vaccine_total$month)))+1) ~ .)

train <- vaccine_total |> filter_index(. ~ as.character(max(vaccine_total$month) - (round(percentage_test*length(unique(vaccine_total$month))))))

nrow(test)

train_tscv <- vaccine_total |> filter_index(. ~ as.character(max(vaccine_total$month) - (forecast_horizon))) |>
     stretch_tsibble(.init = length(unique(train$month)), .step = 1)

train_tscv

fit <- train_tscv |> model(
  average = MEAN(dose_adminstrated),
  snaive = SNAIVE(dose_adminstrated),
  exponential_smoothing = ETS(dose_adminstrated),
  arima = ARIMA(dose_adminstrated),
  regression = TSLM(dose_adminstrated)
  
)



forecast_vaccine_total_tscv <- fit |> forecast(h = forecast_horizon)

forecast_accuracy_tscv <- forecast_vaccine_total_tscv |> accuracy(vaccine_total,
                                                             measures = list(point_accuracy_measures,
                                                                             interval_accuracy_measures,
                                                                             distribution_accuracy_measures))


forecast_accuracy_tscv |> select(.model, RMSE,MAE,MASE,RMSSE, winkler,CRPS)

fit_best <- vaccine_total |> model(expontential_smoothing = ETS(dose_adminstrated))

best_forecast <- fit_best |> forecast(h = forecast_horizon)

ggplot(data = best_forecast, mapping = aes(x = month, ydist = dose_adminstrated))+
  stat_halfeye()+
  geom_line(data =filter_index(vaccine_total, "2020 Apr" ~ .), mapping = aes(x = month, y = dose_adminstrated, colour ="Data"))+
  ggthemes::theme_few()

best_forecast_interval <- best_forecast |> hilo(level = 90) |> unpack_hilo(`90%`)

write.csv(best_forecast_interval,"best_forecast_interval.csv")


#--------------------- residual diagnostic -------------------------------

fit_best |> gg_tsresiduals()
