library(forecast)
library(tidyverse)

time_series_data <- speed_df %>% group_by(hour_of_day) %>% summarise(mean_speed = mean(speed_mph_mean, na.rm = TRUE))
time_series <- ts(time_series_data$mean_speed)
model <- auto.arima(time_series)
n_forecast <- 24
forecast_result <- forecast(model, h=n_forecast)
plot(forecast_result)
