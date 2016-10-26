## ----echo = FALSE, eval = FALSE------------------------------------------
#  library(countyweather)
#  options("noaakey" = Sys.getenv("noaakey"))

## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE
)

## ----eval = FALSE--------------------------------------------------------
#  any(grepl("^\\.Renviron", list.files("~", all.files = TRUE)))

## ----eval = FALSE--------------------------------------------------------
#  noaakey=your_emailed_key
#  

## ------------------------------------------------------------------------
options("noaakey" = Sys.getenv("noaakey"))

## ----warning = FALSE, message = FALSE, echo = 1:2, eval = FALSE----------
#  andrew_precip <- daily_fips(fips = "12086", date_min = "1992-08-01",
#                              date_max = "1992-08-31", var = "prcp")
#  save(andrew_precip, file = "data/andrew_precip.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/andrew_precip.RData")

## ------------------------------------------------------------------------
names(andrew_precip)

## ------------------------------------------------------------------------
head(andrew_precip$daily_data)

## ----fig.width = 7, fig.height = 3---------------------------------------
library(ggplot2)
ggplot(andrew_precip$daily_data, aes(x = date, y = prcp, color = prcp_reporting)) + 
  geom_line() + geom_point() + theme_minimal() + 
  xlab("Date in 1992") + ylab("Daily rainfall (mm)") + 
  scale_color_continuous(name = "# stations\nreporting")

## ----warning = FALSE-----------------------------------------------------
andrew_precip$station_map

## ------------------------------------------------------------------------
andrew_precip$station_metadata

## ----echo = 1:2, eval = FALSE--------------------------------------------
#  andrew_hourly <- hourly_fips(fips = "12086", year = 1992,
#                             var = c("wind_speed", "temperature"))
#  save(andrew_hourly, file = "data/andrew_hourly.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/andrew_hourly.RData")

## ------------------------------------------------------------------------
names(andrew_hourly)

## ------------------------------------------------------------------------
head(andrew_hourly$hourly_data)

## ------------------------------------------------------------------------
andrew_hourly_data <- as.data.frame(andrew_hourly$hourly_data) 

library(countytimezones)

## ----echo = 1:2, eval = FALSE--------------------------------------------
#  andrew_hourly_data <- add_local_time(df = andrew_hourly_data, fips = "12086",
#                                       datetime_colname = "date_time")
#  save(andrew_hourly_data, file = "data/andrew_hourly_data.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/andrew_hourly_data.RData")

## ------------------------------------------------------------------------
head(andrew_hourly_data)

## ----fig.width = 7, fig.height = 3, message = FALSE, warning = FALSE-----
library(dplyr)
library(lubridate)
to_plot <- andrew_hourly$hourly_data %>%
  filter(months(date_time) == "August")
ggplot(to_plot, aes(x = date_time, y = wind_speed,
                    color = wind_speed_reporting)) + 
  geom_line() + theme_minimal() + 
  xlab("Date in August 1992") + 
  ylab("Wind speed (m / s)") + 
  scale_color_continuous(name = "# stations\nreporting")

## ------------------------------------------------------------------------
andrew_hourly$station_map

## ------------------------------------------------------------------------
andrew_hourly$station_metadata

## ----eval = FALSE--------------------------------------------------------
#  fl_counties <- c("12086", "12087", "12011")
#  
#  write_daily_timeseries(fips = fl_counties, date_min = "1992-08-01",
#                   date_max = "1992-08-31", var = "prcp",
#                   out_directory = "~/Documents/andrew_data")

## ----eval = FALSE--------------------------------------------------------
#  plot_daily_timeseries("prcp", data_directory = "~/Documents/andrew_data/data",
#                        plot_directory = "~/Documents/andrew_data/plots",
#                        date_min = "1992-08-01", date_max = "1992-08-31")

## ----warning = FALSE, message = FALSE, echo = FALSE, eval = FALSE--------
#  two <- daily_fips(fips = "12087", date_min = "1992-08-01",
#                              date_max = "1992-08-31", var = "prcp")
#  save(two, file = "data/two.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/two.RData")

## ----warning = FALSE, message = FALSE, echo = FALSE, eval = FALSE--------
#  three <- daily_fips(fips = "12011", date_min = "1992-08-01",
#                              date_max = "1992-08-31", var = "prcp")
#  save(three, file = "data/three.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/three.RData")

## ----echo = FALSE, fig.width = 8, fig.height = 2-------------------------
oldpar <- par(mfrow = c(1, 3))
df <- andrew_precip$daily_data
plot(df$date, df$prcp, type = "l", col = "red", main = "12086", xlab = "date", 
     ylab = "prcp", xlim = c(as.Date("1992-08-01"), as.Date("1992-08-31")))

df2 <- two$daily_data
plot(df2$date, df2$prcp, type = "l", col = "red", main = "12087", xlab = "date", 
     ylab = "prcp", xlim = c(as.Date("1992-08-01"), as.Date("1992-08-31")))

df3 <- three$daily_data
plot(df3$date, df3$prcp, type = "l", col = "red", main = "12011", xlab = "date", 
     ylab = "prcp", xlim = c(as.Date("1992-08-01"), as.Date("1992-08-31")))
par(oldpar)

## ----echo = 1:5, eval = FALSE--------------------------------------------
#  not_averaged <- daily_fips(fips = "12086",
#                             date_min = "1992-08-01",
#                             date_max = "1992-08-31",
#                             var = "prcp", average_data = FALSE,
#                             station_label = TRUE)
#  save(not_averaged, file = "data/not_averaged.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/not_averaged.RData")

## ------------------------------------------------------------------------
not_averaged_data <- not_averaged$daily_data
head(not_averaged_data)
unique(not_averaged_data$id)

## ----fig.width = 7, fig.height = 3, warning = FALSE, message = FALSE-----
library(ggplot2)
ggplot(not_averaged_data, aes(x = date, y = prcp, 
                         colour = id)) + 
  geom_line() + 
  theme_minimal() 

## ----warning = FALSE, message = FALSE, fig.width = 7---------------------
not_averaged$station_map

## ----echo = FALSE--------------------------------------------------------
qual <- data.frame(code = c(0, 1, 2, 3, 4, 5, 6, 7, 9), definition = c(" Passed gross limits check", "Passed all quality control checks", "Suspect", "Erroneous", "Passed gross limits check , data originate from an NCEI data source", "Passed all quality control checks, data originate from an NCEI data source", "Suspect, data originate from an NCEI data source", "Erroneous, data originate from an NCEI data source", "Passed gross limits check if element is present"))

library(knitr)
kable(qual, format = "markdown")

## ----echo = 1:2, eval = FALSE--------------------------------------------
#  ex <- hourly_fips("12086", 1992, var = c("wind_speed", "wind_speed_quality"),
#                    average_data = FALSE)
#  save(ex, file = "data/ex.RData")

## ----echo = FALSE--------------------------------------------------------
load("data/ex.RData")

## ------------------------------------------------------------------------
ex_data <- ex$hourly_data
head(ex_data)

## ------------------------------------------------------------------------
ex_data$wind_speed_quality <- as.numeric(ex_data$wind_speed_quality)
ex_data$wind_speed[ex_data$wind_speed_quality %in% c(2, 3, 6, 7)] <- NA

## ----echo = FALSE, warning = FALSE, message = FALSE----------------------
daily_vars <- data.frame(variables = c("prcp", "snow", "snwd", "tmax", "tmin"), 
                         description = c("precipitation", "snowfall", "snow depth", "maximum temperature", "minumum temperature"), 
                         units = c("mm", "mm", "mm", "degrees Celsius", "degrees Celsius"), 
                         most_extreme_value = c("1100 mm", "1600 mm", "11500 mm", "57 degrees C", "-62 degrees C"))
library(knitr)
kable(daily_vars, format = "markdown", col.names = c("Variable", "Description",
                                                     "Units",
                                                     "Most extreme value"))

## ----echo = FALSE--------------------------------------------------------
hourly_vars <- data.frame(Variable = c("wind_direction", "wind_speed", 
                                        "ceiling_height", "visibility_distance", 
                                        "temperature", "temperature_dewpoint", 
                                        "air_pressure"), 
                          Description = c("The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing", 
                                          "The rate of horizontal travel of air past a fixed point", 
                                          "The height above ground level of the lowest cloud or obscuring phenomena layer aloft with 5/8 or more summation total sky cover, which may be predominately opaque, or the vertical visibility into a surface-based obstruction", 
                                          "The horizontal distance at which an object can be seen and identified", 
                                          "The temperature of the air", 
                                          "The temperature to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur", 
                                          "The air pressure relative to Mean Sea Level"), 
                          Units = c("Angular Degrees", "Meters per Second",
                                    "Meters", "Meters", "Degrees Celsuis", 
                                    "Degrees Celsius", "Hectopascals"), 
                          Minimum = c("1", "0", "0", "0", "-93.2", "-98.2", "860"), 
                          Maximum = c("360", "90", "22000 (indicates 'Unlimited')", "160000", "61.8", 
                                      "36.8", "1090"))

## ----echo = FALSE, warning = FALSE, message = FALSE----------------------
library(pander)
pander::pander(hourly_vars, split.cell = 75, split.table = Inf)

## ----eval = FALSE--------------------------------------------------------
#  Error in getOption("noaakey", stop("need an API key for NOAA data")) :
#    need an API key for NOAA data

## ----eval = FALSE--------------------------------------------------------
#  Warning message:
#  Error: (400) - Token parameter is required.

## ------------------------------------------------------------------------
options("noaakey" = Sys.getenv("noaakey"))

