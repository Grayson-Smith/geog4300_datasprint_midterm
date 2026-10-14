#Numbats
library(tidyverse)

install.packages("weatherOz")
library(weatherOz)

numbat_data<-read_csv("numbats.csv") %>%
  filter(!is.na(eventDate)) %>%
  mutate(event_oz=paste0(year(eventDate),
                         str_pad(month(eventDate),2,pad="0"),
                         str_pad(day(eventDate),2,pad="0")))
summary(numbat_data)

x <- get_data_drill(
  longitude  = 153.0,
  latitude   = -27.5,
  start_date = "20240101",
  end_date   = "20241231",
  values     = c("max_temp", "min_temp", "rain"),
  api_key    = "jshannon@uga.edu"
)

numbat_wx<-function(row_sel){
  numbat_sel<-numbat_data[row_sel,]
  
  get_data_drill(
    longitude=numbat_sel$decimalLongitude,
    latitude=numbat_sel$decimalLatitude,
    start_date=numbat_sel$event_oz,
    end_date=numbat_sel$event_oz,
    values     = c("max_temp", "min_temp", "rain"),
    api_key    = "jshannon@uga.edu"
  ) %>%
    rename(decimalLatitude=latitude,
           decimalLongitude=longitude,)
}

weather_data<-map_df(1:nrow(numbat_data),numbat_wx)
numbat_wx<-numbat_data %>%
  rename(date=day) %>%
  left_join(weather_data)
