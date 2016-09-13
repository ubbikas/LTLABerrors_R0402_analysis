library(rvest)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)

all_data <- list()
all_dates <- seq(as.Date("2015-01-01"), 
                 as.Date("2016-06-30"), 
                 by = "day")

measurements <- c("Mean Temperature",
                  "Max Temperature",
                  "Min Temperature",
                  "Dew Point",
                  "Average Humidity",
                  "Maximum Humidity",
                  "Minimum Humidity",
                  "Precipitation",
                  "Sea Level Pressure",
                  "Wind Speed",
                  "Max Wind Speed")


for (date in all_dates) {
  
  date <- as.Date(date, origin = "1970-01-01")
  d = day(date)
  m = month(date)
  y = year(date)
    
  url <- paste0("https://www.wunderground.com/history/airport/EYVI/", 
                y, "/", m,"/",d,
                "/DailyHistory.html?req_city=Vilnius+International&",
                "req_state=&req_statename=Lithuania&reqdb.zip=00000&",
                "reqdb.magic=5&reqdb.wmo=26730")
  
  data <- read_html(url)

  print("################################################")
  print(date)
  print("--------")  
  
  data %>%
    html_nodes("#historyTable") %>%
    html_text() %>%
    gsub("\n", "", .) %>%
    gsub("\t", "", .) %>%
    gsub("Â", "", .) -> data1
  

  date_data <- list()
  
  for (i in measurements) {
    data1 %>%
      gregexpr(paste0(i,"\\s*.\\d+\\.?\\d*"), .) -> x
    
    data1 %>%
      regmatches(x) %>%
      unlist() %>%
      gsub(i, "", .) %>%
      gsub(" ", "", .) %>%
      .[1] -> value
    date_data[[i]] <- value
    print(paste0(i,": ", value))
  }
  
  date_data[["Date"]] <- date
  all_data[[as.character(date)]] <- as.data.frame(date_data)

  Sys.sleep(5)
}

all_data_df <- do.call(rbind, all_data)

# write.csv(all_data_df, "daily_temperatures.csv")

daily_data <- read.csv("daily_temperatures.csv", stringsAsFactors = FALSE)
daily_data$X <- NULL
