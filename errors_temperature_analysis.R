suppressPackageStartupMessages(require(caret))
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(pROC))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(sqldf))
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(scales))
suppressPackageStartupMessages(require(viridis))
suppressPackageStartupMessages(require(broom))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(ggvis))
suppressPackageStartupMessages(require(shiny))


# nustatomas darbinis katalogas priklausomai nuo kompo
if (Sys.info()[[4]] == "ANDRIUS-PC")
  setwd("D:/-=Works=-/R/GitHub/LTLABerrors_R0402_analysis")

if (Sys.info()[[4]] == "LTLAB-DEV")
  setwd("C:/R/GitHub/LTLABerrors_R0402_analysis")


DBFileName <- "LTLABerrors.sqlite"
errors_db_sqlite <- src_sqlite(DBFileName)
errors_sqlite <- tbl(errors_db_sqlite, "HOUR_ERRORS")

daily_temp <- read.csv("daily_temperatures.csv", stringsAsFactors = FALSE)
temp <- read.csv("temp.csv", stringsAsFactors = FALSE)


errors_sqlite %>%
  select(PackageTypeName, NumberOfLAlignPlacements, NumberOfCAlignPlacements) %>%
  mutate(padeta = NumberOfLAlignPlacements + NumberOfCAlignPlacements) %>%
  group_by(PackageTypeName) %>%
  summarize(padeta = sum(padeta)) %>%
  arrange(desc(padeta)) %>%
  filter(padeta > 30000) %>%
  as.data.frame() %>%
  .[["PackageTypeName"]] -> pop_packages
  

errors_sqlite %>%
  filter(PackageTypeName %in% pop_packages) %>%
  group_by(Date, PackageTypeName) %>%
  select(-Time, -Machine, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,3:13])) %>%
  mutate(., Placed  = rowSums(.[ ,14:15])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  mutate(Date = format(as.Date(Date), format = "%Y-%m")) %>%
  filter(Date != "2014-12",
         Date != "2016-07") %>%
  group_by(Date, PackageTypeName) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  left_join(temp, by = "Date") -> data


data %>%
  group_by(PackageTypeName) %>%
  do(model = lm(Percent ~ Temp, data = .)) %>%
  tidy(model) %>%
  ungroup() %>%
  filter(term == "Temp",
         p.value < 0.05,
         abs(estimate) > 0.01) %>%
  arrange(p.value) -> temp_dependant
temp_dependant # temp_dependant$PackageTypeName


data %>% 
  #filter(PackageTypeName %in% temp_dependant$PackageTypeName) %>%
  filter(PackageTypeName %in% c("R0402", "SH1252-W-04-AT")) %>%
  ggplot(aes(x = Temp, 
             y = Percent, 
             colour = factor(PackageTypeName))) +
  geom_point(size = 3) +
  stat_smooth(fill = NA)


data %>% 
  #filter(PackageTypeName %in% temp_dependant$PackageTypeName) %>%
  filter(PackageTypeName %in% c("R0402", "SH1252-W-04-AT")) %>%
  ggplot(aes(x = Date, 
             y = Percent, 
             group = PackageTypeName,
             colour = factor(PackageTypeName))) +
  geom_line(size = 2) +
  geom_line(aes(x = Date,
                y = Temp/3),
            colour = "green",
            size = 2)
  

data %>% 
  filter(PackageTypeName == "R0402") %>%
  ggplot(aes(x = Temp, 
             y = Percent, 
             colour = factor(PackageTypeName))) +
  geom_point(size = 3) +
  stat_smooth()


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date, PackageTypeName) %>%
  select(-Time, -Machine, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,3:13])) %>%
  mutate(., Placed  = rowSums(.[ ,14:15])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  mutate(Date = format(as.Date(Date), format = "%Y-%m")) %>%
  group_by(Date, PackageTypeName) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  left_join(temp, by = "Date") %>%
  rename(Temperature = Temp) -> monthly_data 


monthly_data %>% 
  ggplot(aes(x = Date, y = Percent)) +
  geom_bar(stat = "identity") +
  geom_abline(slope = 0, 
              intercept = 0.2,
              color = "red") +
  scale_y_continuous(breaks=c(0, 0.2, 1, 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

monthly_data %>% 
  ggplot(aes(x = Temperature, y = Percent)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE)


summary(lm(Percent ~ Temperature, data = monthly_data))

monthly_data %>% 
  mutate(Temperature = pmin(5, Temperature)) -> monthly_data_capped

monthly_data_capped %>%
  ggplot(aes(x = Temperature, y = Percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


summary(lm(Percent ~ Temperature, data = monthly_data_capped))


### DAILY -----------------------------------

errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date, PackageTypeName) %>%
  select(-Time, -Machine, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,3:13])) %>%
  mutate(., Placed  = rowSums(.[ ,14:15])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date, PackageTypeName) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>%
  left_join(daily_temp, by = "Date") %>%
  ungroup() -> data


data_no_date <- data %>% 
                select(-Date, -PackageTypeName, -ErrorsSum, -Placed, -X)

tidy_data <- data %>% 
             select(-PackageTypeName, -ErrorsSum, -Placed, -X) %>%
             gather(measurement, value, -Percent, - Date) %>%
             as.data.frame()


model <- lm(Percent ~ Mean.Temperature, data = data)
summary(model)

model <- lm(Percent ~ Min.Temperature, data = data)
summary(model)


model2 <- lm(Percent ~ Mean.Temperature + Average.Humidity + Precipitation, 
             data = data)
summary(model2)


ggplot(data, aes(x = as.Date(Date), y = Percent)) +
  geom_line(aes(group = 1, 
                colour = "Percent"), 
            size = 0.8) +
  geom_line(aes(y = Min.Temperature, 
                group = 1,
                colour = "Temperature"), 
            size = 0.8) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  scale_colour_manual(name = "",
                      values = c("Percent" = "grey25",
                                 "Temperature" = "red")) +
  xlab("Date") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = Min.Temperature, y = Percent)) +
  geom_point(size = 2) +
  stat_smooth(fill = NA)


tidy_data %>%
  group_by(measurement) %>%
  do(model = lm(Percent ~ value, data = .)) %>%
  tidy(model) %>%
  filter(term == "value") %>%
  ungroup() %>%
  arrange(p.value) %>% 
  select(-std.error, -statistic)




# Capped data -----------------------------------


tidy_data %>%
  filter(measurement == "Min.Temperature") %>%
  mutate(capped_temp = ifelse(value > 0, 0, value)) -> capped_data


ggplot(capped_data, aes(x = as.Date(Date), y = Percent)) +
  geom_line(aes(group = 1, 
                colour = "Percent"), 
            size = 0.8) +
  geom_line(aes(y = capped_temp, 
                group = 1,
                colour = "Temperature"), 
            size = 0.8) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  scale_colour_manual(name = "",
                      values = c("Percent" = "grey25",
                                 "Temperature" = "red")) +
  xlab("Date") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


summary(lm(Percent ~ capped_temp, data = capped_data))

ggplot(capped_data, aes(x = capped_temp, y = Percent)) +
  geom_point(size = 3) +
  stat_smooth(fill = NA, method = "lm")


### Lagged values -----------------------------------------------------------------

errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date, PackageTypeName) %>%
  select(-Time, -Machine, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,3:13])) %>%
  mutate(., Placed  = rowSums(.[ ,14:15])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date, PackageTypeName) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>%
  right_join(daily_temp, by = "Date") %>%
  ungroup() -> data


data_no_date <- data %>% 
  select(-PackageTypeName, -ErrorsSum, -Placed, -X)

data_no_date %>% 
  mutate(MeanTemperaturelag1 = lag(Mean.Temperature, 1),
         MeanTemperaturelag2 = lag(Mean.Temperature, 2),
         MeanTemperaturelag3 = lag(Mean.Temperature, 3),
         MaxTemperaturelag1 = lag(Max.Temperature, 1),
         MaxTemperaturelag2 = lag(Max.Temperature, 2),
         MaxTemperaturelag3 = lag(Max.Temperature, 3),
         MinTemperaturelag1 = lag(Min.Temperature, 1),
         MinTemperaturelag2 = lag(Min.Temperature, 2),
         MinTemperaturelag3 = lag(Min.Temperature, 3),
         DewPointlag1 = lag(Dew.Point, 1),
         DewPointlag2 = lag(Dew.Point, 2),
         DewPointlag3 = lag(Dew.Point, 3),
         AverageHumiditylag1 = lag(Average.Humidity, 1),
         AverageHumiditylag2 = lag(Average.Humidity, 2),
         AverageHumiditylag3 = lag(Average.Humidity, 3),
         MaximumHumiditylag1 = lag(Maximum.Humidity, 1),
         MaximumHumiditylag2 = lag(Maximum.Humidity, 2),
         MaximumHumiditylag3 = lag(Maximum.Humidity, 3),
         MinimumHumiditylag1 = lag(Minimum.Humidity, 1),
         MinimumHumiditylag2 = lag(Minimum.Humidity, 2),
         MinimumHumiditylag3 = lag(Minimum.Humidity, 3),
         Precipitationlag1 = lag(Precipitation, 1),
         Precipitationlag2 = lag(Precipitation, 2),
         Precipitationlag3 = lag(Precipitation, 3)) %>%
  na.omit() %>% 
  select(-Date) -> data_lag


summary(lm(Percent ~ ., data = data_lag))


tidy_data_lag <- data_lag %>%
  gather(measurement, value, -Percent, - Date) %>%
  as.data.frame()


tidy_data_lag %>%
  group_by(measurement) %>%
  do(model = lm(Percent ~ value, data = .)) %>%
  tidy(model) %>%
  filter(term == "value") %>%
  ungroup() %>%
  arrange(p.value) %>% View()


ggplot(data_lag, aes(x = Date, y = Percent)) +
  geom_line(aes(group = 1), size = 1) +
  geom_line(aes(y = DewPointlag1, 
                group = 2), 
            color = "red", size = 1) +
  geom_line(aes(y = Dew.Point, 
                group = 2), 
            color = "green", size = 1)


tidy_data_lag %>%
  filter(measurement %in% c("Min.Temperature", "MinTemperaturelag1", 
                            "MinTemperaturelag2", "MinTemperaturelag3")) %>%
  filter(Percent > 0.5) %>%
  group_by(measurement) %>%
  do(model = lm(Percent ~ value, data = .)) %>%
  tidy(model) %>%
  filter(term == "value") %>%
  ungroup() %>%
  arrange(p.value)


data_no_date %>% 
  transmute(Percent = Percent,
            Min.Temperature = Min.Temperature,
            MinTemperaturelag1 = lag(Min.Temperature, 1),
            MinTemperaturelag2 = lag(Min.Temperature, 2),
            MinTemperaturelag3 = lag(Min.Temperature, 3),
            MinTemperaturelag4 = lag(Min.Temperature, 4),
            MinTemperaturelag5 = lag(Min.Temperature, 5),
            MinTemperaturelag6 = lag(Min.Temperature, 6),
            MinTemperaturelag7 = lag(Min.Temperature, 7),
            MinTemperaturelag8 = lag(Min.Temperature, 8),
            MinTemperaturelag9 = lag(Min.Temperature, 9)) %>% 
  na.omit() %>% 
  filter(Percent > 0.5) %>%
  cor()


data_no_date %>% 
  transmute(Percent = Percent,
            Min.Temperature = Min.Temperature,
            MinTemperaturelag1 = lag(Min.Temperature, 1),
            MinTemperaturelag2 = lag(Min.Temperature, 2),
            MinTemperaturelag3 = lag(Min.Temperature, 3),
            MinTemperaturelag4 = lag(Min.Temperature, 4),
            MinTemperaturelag5 = lag(Min.Temperature, 5),
            MinTemperaturelag6 = lag(Min.Temperature, 6),
            MinTemperaturelag7 = lag(Min.Temperature, 7),
            MinTemperaturelag8 = lag(Min.Temperature, 8),
            MinTemperaturelag9 = lag(Min.Temperature, 9)) %>% 
  na.omit() %>% 
  filter(Percent > 0.4) %>%
  gather(measurement, value, -Percent) %>%
  mutate(value = ifelse(value > 0, 0, value)) %>%
  group_by(measurement) %>%
  do(model = lm(Percent ~ value, data = .)) %>%
  tidy(model) %>%
  filter(term == "value") %>%
  ungroup() %>%
  arrange(p.value) %>%
  mutate(pvalueround = round(p.value, 4)) %>% View()


data_no_date %>% 
  transmute(Date = Date,
            Percent = Percent,
            Min.Temperature = Min.Temperature,
            MinTemperaturelag1 = lag(Min.Temperature, 1),
            MinTemperaturelag2 = lag(Min.Temperature, 2),
            MinTemperaturelag3 = lag(Min.Temperature, 3),
            MinTemperaturelag4 = lag(Min.Temperature, 4),
            MinTemperaturelag5 = lag(Min.Temperature, 5),
            MinTemperaturelag6 = lag(Min.Temperature, 6),
            MinTemperaturelag7 = lag(Min.Temperature, 7),
            MinTemperaturelag8 = lag(Min.Temperature, 8),
            MinTemperaturelag9 = lag(Min.Temperature, 9)) %>% 
  na.omit() %>% 
  filter(Percent > 0.4) %>%
  ggplot(aes(x = Date, y = Percent)) +
  geom_line(aes(group = 1), size = 1) +
  geom_line(aes(y = pmin(MinTemperaturelag1, 0), 
                group = 2), 
            color = "red", size = 1) +
  ylim(c(-30,15))


data_no_date %>% 
  transmute(Date = Date,
            Percent = Percent,
            Min.Temperature = Min.Temperature,
            MinTemperaturelag1 = lag(Min.Temperature, 1),
            MinTemperaturelag2 = lag(Min.Temperature, 2),
            MinTemperaturelag3 = lag(Min.Temperature, 3),
            MinTemperaturelag4 = lag(Min.Temperature, 4),
            MinTemperaturelag5 = lag(Min.Temperature, 5),
            MinTemperaturelag6 = lag(Min.Temperature, 6),
            MinTemperaturelag7 = lag(Min.Temperature, 7),
            MinTemperaturelag8 = lag(Min.Temperature, 8),
            MinTemperaturelag9 = lag(Min.Temperature, 9)) %>% 
  na.omit() %>% 
  ggvis(x = ~as.Date(Date), y = ~Percent) %>% 
  layer_lines() %>%
  layer_lines(y = ~pmin(MinTemperaturelag2, 0),
              stroke := "red") %>%
  scale_datetime("x", nice = "week")
  
  
i <- 0
mtc1 <- reactive({
  invalidateLater(2000, NULL);
  data_no_date %>% 
    transmute(Date = Date,
              Percent = Percent,
              Temperature = lag(Min.Temperature, i)) %>%
    na.omit() -> data
  i <<- i+1
  data
})


mtc1 %>% 
  ggvis(x = ~as.Date(Date), y = ~Percent) %>% 
  layer_lines() %>%
  layer_lines(y = ~pmin(Temperature, 0),
              stroke := "red") %>%
  scale_numeric("y", 
                domain = c(-23, 15), 
                nice = FALSE, 
                clamp = TRUE) %>%
  scale_datetime("x", nice = "week")


### Testing bigger lags -------------------------------------------------


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% # islaukoma ~90% dienu!
  right_join(daily_temp, by = "Date") %>%
  select(Date, Percent, Min.Temperature, Placed) -> data


data_lag <- function(lagval, data) {
  data_new <- data
  data_new$lag <- lagval
  data_new %>%
    mutate(min_temp = lag(Min.Temperature, lagval),
           min_temp_cap = ifelse(min_temp > 0, 0, min_temp)) %>%
    select(Date, Percent, min_temp, min_temp_cap, lag) %>%
    na.omit()
}


lapply(0:30, data_lag, data = data) %>% 
  bind_rows() %>%
  filter(Percent > 0.2) %>%
  group_by(lag) %>%
  do(model = lm(Percent ~ min_temp_cap, data = .)) %>%
  tidy(model) %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>%
  mutate("Reiksmingas" = ifelse(p.value < 0.001, "Taip", "Ne")) %>%
  ggplot(aes(x = lag, y = estimate)) +
  geom_line() +
  geom_point(aes(color = Reiksmingas), size = 2.3) +
  geom_text(aes(label = lag), nudge_y = 0.02, size = 2.5)


data_lag(1, data = data) %>%
  filter(Percent > 0.4) %>%
  ggplot(aes(x = Date, y = Percent)) +
  geom_line(aes(group = 1), size = 1) +
  geom_line(aes(y = min_temp_cap, 
                group = 2), 
            color = "red", size = 1)


lapply(0:9, data_lag, data = data) %>% 
  bind_rows() %>%
  mutate(lag = paste0("Lag - ", lag)) %>%
  ggplot(aes(x = min_temp, y = Percent, group = min_temp)) +
  geom_boxplot() +
  geom_smooth(color = "red", aes(group=1)) +
  facet_wrap(~lag, ncol = 2)

### Percent on warm days  ------------------------------------------------

errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% # islaukoma ~90% dienu!
  right_join(daily_temp, by = "Date") %>%
  select(-ErrorsSum, -Placed, -X) %>% 
  na.omit() %>% 
  filter(Min.Temperature > 0) -> pos_temp_data


lm(Percent ~ ., data = pos_temp_data %>% select(-Date)) %>% 
  summary()
  

pos_temp_data %>% 
  ggplot(aes(x = as.Date(Date), y = Percent)) +
  geom_line(aes(group = 1, 
                colour = "Percent"), 
            size = 0.8,
            alpha = 0.8) +
  geom_line(aes(y = Min.Temperature, 
                group = 1,
                colour = "Temperature"), 
            size = 0.8,
            alpha = 0.8) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  scale_colour_manual(name = "",
                      values = c("Percent" = "grey25",
                                 "Temperature" = "red")) +
  xlab("Date") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Comparing weekdays ------------------------------------------------

errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% 
  right_join(daily_temp, by = "Date") %>%
  select(Date, Percent, Min.Temperature, Placed) %>%
  na.omit() %>%
  mutate(min_temp = Min.Temperature,
         min_temp_cap = ifelse(min_temp > 0, 0, min_temp),
         Weekday = wday(Date, label = TRUE, abbr = FALSE)) %>%
  select(Date, Percent, min_temp, min_temp_cap, Weekday) -> data


data %>% 
  group_by(Weekday) %>% 
  summarize(Mean_percent = round(mean(Percent), 2),
            Standard_deviation = round(sd(Percent), 2),
            Number_of_days = n()) %>% 
  knitr::kable()


oneway.test(Percent ~ Weekday, data = data)


summary(lm(Percent~Weekday, data = data))


data %>% 
  ggplot(aes(x = Weekday, y = Percent, group = Weekday)) +
  geom_boxplot(varwidth = TRUE)


data %>% 
  ggplot(aes(x = min_temp, y = Percent)) +
  geom_point(size = 3) +
  stat_smooth() +
  facet_wrap(~weekday, ncol = 2)
  

data %>% 
  ggplot(aes(Percent)) +
  geom_histogram(data = data %>% select(-Weekday),
                 bins = 70,
                 fill = "grey60") +
  geom_histogram(aes(fill = Weekday),
                 bins = 70) +
  facet_wrap(~Weekday, ncol = 2) +
  guides(fill = FALSE) +
  theme_bw()


### Comparing machines ------------------------------------------------


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Machine, Date) %>%
  select(-PackageTypeName, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,3:13])) %>%
  mutate(., Placed  = rowSums(.[ ,14:15])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Machine, Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 500) %>%
  right_join(daily_temp, by = "Date") %>%
  select(-ErrorsSum, -X) %>%
  ungroup() %>%
  mutate(MinTempLag1 = lag(Min.Temperature, 1),
         MinTempLag2 = lag(Min.Temperature, 2),
         MinTempLag3 = lag(Min.Temperature, 3),
         MinTempLag4 = lag(Min.Temperature, 4)) %>%
  rowwise() %>%
  mutate(MinTemp = min(Min.Temperature,
                       MinTempLag1,
                       MinTempLag2,
                       MinTempLag3,
                       MinTempLag4)) %>%
  select(Machine, Date, Placed, Percent, MinTemp) %>%
  na.omit() -> data


data %>%
  group_by(Machine) %>%
  summarise(n_days = n(),
            placements = sum(Placed))


data %>%
  ggplot(aes(x = Machine, y = Percent)) +
  geom_violin(aes(color = Machine)) +
  theme_bw()


data %>%
  filter(Percent > 0.4) %>%
  mutate(MinTempCap = ifelse(MinTemp > 0, 0, MinTemp)) %>%
  group_by(Machine) %>%
  do(model = lm(Percent ~ MinTempCap, data = .)) %>%
  tidy(model) %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>%
  arrange(p.value)


data %>%
  filter(Percent > 0.2) %>%
  mutate(MinTempCap = ifelse(MinTemp > 0, 0, MinTemp),
         Date = as.Date(Date)) %>%
  ggplot(aes(x = Date, y = Percent)) +
  geom_line(size = 1) +
  geom_line(aes(y = MinTempCap), 
            color = "red", size = 1) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  facet_wrap(~Machine, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data %>%
  mutate(MinTempCap = ifelse(MinTemp > 0, 0, MinTemp)) %>%
  ggplot(aes(x = MinTempCap, y = Percent)) +
  geom_point(size = 3) +
  stat_smooth(fill = NA, method = "lm") +
  facet_wrap(~Machine, nrow = 2) +
  theme_bw()


### Comparing robots  ------------------------------------------------


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Machine, PMSlot, Date) %>%
  select(-PackageTypeName, -Time, -ProcessProgramID, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ , 4:14])) %>%
  mutate(., Placed  = rowSums(.[ , 15:16])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Machine, PMSlot, Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% # ~84% duomenu
  right_join(daily_temp, by = "Date") %>%
  select(-X) %>%
  ungroup() %>%
  mutate(MinTempLag1 = lag(Min.Temperature, 1),
         MinTempLag2 = lag(Min.Temperature, 2),
         MinTempLag3 = lag(Min.Temperature, 3),
         MinTempLag4 = lag(Min.Temperature, 4)) %>%
  rowwise() %>%
  mutate(MinTemp = min(Min.Temperature,
                       MinTempLag1,
                       MinTempLag2,
                       MinTempLag3,
                       MinTempLag4)) %>%
  select(Machine, PMSlot, Date, Placed, ErrorsSum, Percent, MinTemp) %>%
  na.omit() -> PMslot_data


PMslot_data %>%
  group_by(Machine, PMSlot) %>%
  summarise(n_days = n(),
            placements = sum(Placed),
            errors = sum(ErrorsSum),
            percent = round(errors*100 / placements,4)) -> PMslot_data_group

print(PMslot_data_group)


PMslot_data_group %>%
  ggplot(aes(x = PMSlot, y = n_days)) +
  geom_bar(stat = "identity") +
  facet_grid(~Machine) +
  geom_text(aes(label = PMSlot), nudge_y = 7, size = 3) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")


PMslot_data_group %>%
  ggplot(aes(x = PMSlot, y = placements)) +
  geom_bar(stat = "identity") +
  facet_grid(~Machine) +
  geom_text(aes(y = placements, label = PMSlot), 
            nudge_y = 70000, size = 3) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")


PMslot_data %>%
  ggplot(aes(x = as.factor(PMSlot), y = Percent)) +
  geom_boxplot(aes(color = Machine), varwidth = TRUE) +
  ylim(c(0, 10)) +
  theme_bw() +
  facet_grid(~Machine) +
  theme(legend.position = "none")


PMslot_data %>%
  mutate(MinTempCap = ifelse(MinTemp > 0, 0, MinTemp)) %>%
  group_by(Machine, PMSlot) %>%
  do(model = lm(Percent ~ MinTempCap, data = .)) %>% 
  tidy(model) %>%
  filter(term != "(Intercept)") %>%
  ungroup() %>%
  select(Machine, PMSlot, estimate, p.value) %>%
  mutate(significant = ifelse(p.value < 0.05, "YES", ""))


PMslot_data %>%
  mutate(MinTempCap = ifelse(MinTemp > 0, 0, MinTemp)) %>%
  ggplot(aes(x = MinTempCap, y = Percent)) +
  geom_point(size = 1) +
  stat_smooth(method = "lm") +
  ylim(c(0, 10)) +
  facet_grid(Machine ~ PMSlot) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6))


# prametimo procentas apribotas max 10%
errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Machine, PMSlot, Date) %>%
  select(-PackageTypeName, -Time, -ProcessProgramID, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ , 4:14])) %>%
  mutate(., Placed  = rowSums(.[ , 15:16])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Machine, PMSlot, Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% # ~84% duomenu
  right_join(daily_temp, by = "Date") %>%
  select(-X) %>%
  ungroup() %>%
  mutate(MinTempLag1 = lag(Min.Temperature, 1),
         MinTempLag2 = lag(Min.Temperature, 2),
         MinTempLag3 = lag(Min.Temperature, 3),
         MinTempLag4 = lag(Min.Temperature, 4)) %>%
  rowwise() %>%
  mutate(MinTemp = min(Min.Temperature,
                       MinTempLag1,
                       MinTempLag2,
                       MinTempLag3,
                       MinTempLag4)) %>%
  select(Machine, PMSlot, Date, Placed, ErrorsSum, Percent, MinTemp) %>%
  na.omit()  %>%
  filter(Percent > 0.0) %>%
  mutate(MinTempCap = ifelse(MinTemp > 0, 0, MinTemp),
         Date = as.Date(Date),
         Percent = pmin(Percent, 15)) %>%
  ggplot(aes(x = Date, y = Percent)) +
  geom_line(size = 0.8) +
  geom_line(aes(y = MinTempCap), 
            color = "red", size = 0.8) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  facet_grid(PMSlot ~ Machine) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Percent of below zero days  ------------------------------------------------


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  select(-PackageTypeName, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType, -Machine) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>%
  right_join(daily_temp, by = "Date") %>%
  select(-ErrorsSum, -X) %>%
  ungroup() %>%
  mutate(MinTempLag1 = lag(Min.Temperature, 1),
         MinTempLag2 = lag(Min.Temperature, 2),
         MinTempLag3 = lag(Min.Temperature, 3),
         MinTempLag4 = lag(Min.Temperature, 4)) %>%
  rowwise() %>%
  mutate(MinTemp5 = min(Min.Temperature,
                       MinTempLag1,
                       MinTempLag2,
                       MinTempLag3,
                       MinTempLag4)) %>%
  select(Date, Placed, Percent, MinTemp5, Min.Temperature) %>%
  na.omit() %>% 
  mutate(below_zero5 = MinTemp5 < 0,
         below_zero = Min.Temperature < 0) -> days5_min_temp_data


days5_min_temp_data %>% 
  ggplot(aes(Percent)) +
  geom_histogram(bins = 200) +
  geom_vline(xintercept = c(0.2), color = "red")


percent_seq <- seq(0, 10, 0.2)
seq_length <- length(percent_seq)
below_zero_days_df <- data.frame(min_errors_percent = rep(0, seq_length),
                                 below_zero5_days_percent = rep(0, seq_length),
                                 below_zero_days_percent = rep(0, seq_length),
                                 n_days = rep(0, seq_length))


for (i in 1:length(percent_seq)) {
  percent <- percent_seq[i]
  below_zero_days_df[i, 1] <- percent
  
  days5_min_temp_data %>% 
    filter(Percent > percent) %>% 
    summarize(below_zero5_percent = mean(below_zero5)) %>% 
    .[[1]] %>% 
    round(2) -> below_zero_days_df[i, 2]
  
  days5_min_temp_data %>% 
    filter(Percent > percent) %>% 
    summarize(below_zero_percent = mean(below_zero)) %>% 
    .[[1]] %>% 
    round(2) -> below_zero_days_df[i, 3]
  
  days5_min_temp_data %>% 
    filter(Percent > percent) %>% 
    summarize(n_days = n()) %>% 
    .[[1]] %>% 
    round(2) -> below_zero_days_df[i, 4]
}


below_zero_days_df %>% 
  ggplot(aes(x = min_errors_percent,
             y = below_zero_days_percent)) +
  geom_line(size = 1, aes(color = "Current day")) +
  geom_line(aes(y = below_zero5_days_percent, 
                color = "Last 5 days"),
            size = 1) +
  geom_abline(intercept = below_zero_days_df$below_zero_days_percent[1],
              slope = 0,
              color = "skyblue3",
              linetype="longdash") +
  annotate("text", 
           x = 8, 
           y = below_zero_days_df$below_zero_days_percent[1] + 0.03,
           color = "skyblue3",
           label = "Mean percent of days below zero") +
  geom_abline(intercept = below_zero_days_df$below_zero5_days_percent[1],
              slope = 0,
              color = "blue",
              linetype="longdash") +
  annotate("text", 
           x = 8, 
           y = below_zero_days_df$below_zero5_days_percent[1] + 0.03,
           color = "blue",
           label = "Mean percent of days below zero") +
  scale_x_continuous(breaks = c(0:10), 
                     labels = paste0(">", c(0:10),"%")) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     labels = paste0(seq(0, 100, 20),"%")) +
  labs(x = "Minimum errors percent",
       y = "Percent of days below zero") +
  scale_colour_manual(name = "",
                      values = c("Current day" = "skyblue3",
                                 "Last 5 days" = "blue")) +
  theme_bw()


### Grouped by package ---------------------------------------


errors_sqlite %>%
  select(PackageTypeName, NumberOfLAlignPlacements, NumberOfCAlignPlacements) %>%
  mutate(Placed = NumberOfLAlignPlacements + NumberOfCAlignPlacements) %>%
  group_by(PackageTypeName) %>%
  summarize(Placed = sum(Placed)) %>%
  filter(Placed > 10000) %>% 
  arrange(desc(Placed)) %>%
  as.data.frame() %>%
  .[["PackageTypeName"]] -> pop_packages


errors_sqlite %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  filter(PackageTypeName %in% pop_packages) %>%
  group_by(Date, PackageTypeName) %>%
  select(-Time, -Machine, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,3:13])) %>%
  mutate(., Placed  = rowSums(.[ ,14:15])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date, PackageTypeName) %>%
  summarise(ErrorsSum = sum(ErrorsSum),
            Placed = sum(Placed),
            Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  left_join(daily_temp, by = "Date") %>% 
  ungroup() %>% 
  mutate(Date = as.Date(Date)) -> top_package_data



top_package_data %>% 
  group_by(PackageTypeName) %>% 
  summarize(ErrorsSum = sum(ErrorsSum),
            PlacedSum = sum(Placed),
            Percent = ErrorsSum/PlacedSum*100) %>% 
  select(PackageTypeName, ErrorsSum, PlacedSum, Percent) %>% 
  arrange(-ErrorsSum) %>% 
  head(15) %>% 
  knitr::kable(digits = 2)


top_package_data %>%
  filter(Placed > 500) %>% 
  group_by(PackageTypeName) %>%
  do(model = lm(Percent ~ Mean.Temperature, data = .)) %>%
  tidy(model) %>%
  ungroup() %>%
  filter(term != "(Intercept)",
         p.value < 0.01,
         abs(estimate) > 0.01) %>%
  arrange(p.value) %>% 
  select(PackageTypeName, estimate, p.value) -> temp_dependant

temp_dependant %>% 
  knitr::kable(digits = c(1,2,16))


top_package_data %>% 
  filter(PackageTypeName %in% temp_dependant$PackageTypeName) %>%
  ggplot(aes(x = Mean.Temperature, 
             y = Percent)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm") +
  ylim(c(0,10)) +
  facet_wrap(~PackageTypeName) +
  theme_bw()


top_package_data %>% 
  filter(PackageTypeName == "SOD128") %>% 
  ggplot(aes(x = Date, y = Percent)) +
  geom_line(aes(color = "Percent"),
            size = 1) +
  geom_line(aes(y = Mean.Temperature,
                color = "Temperature"), 
            size = 1) +
  geom_point(aes(color = "Percent"),
             size = 2) +
  geom_point(aes(y = Mean.Temperature,
                color = "Temperature"), 
             size = 2) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  scale_colour_manual(name = "",
                      values = c("Percent" = "grey25",
                                 "Temperature" = "red")) +
  labs(y = "Percent/Temperature",
      title = "SOD128") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

top_package_data %>% 
  filter(PackageTypeName == "SH1252-W-04-AT") %>% 
  lm(Percent ~ Mean.Temperature, data = .) %>% 
  summary()


top_package_data %>% 
  filter(PackageTypeName == "SH1252-W-04-AT",
         Placed > 500) %>% 
  rowwise() %>% 
  mutate(Mean.Temperature = min(-Mean.Temperature, 0)) %>% 
  ggplot(aes(x = Date, y = Percent*3)) +
  geom_segment(aes(xend = Date, 
                   yend = 0)) +
  geom_point(aes(color = "Percent"),
             size = 2) +
  stat_smooth(se = FALSE, 
              color = "grey25",
              span = 0.15) +
  stat_smooth(aes(y = Mean.Temperature),
              se = FALSE,
              color = "red",
              span = 0.15) +
  geom_segment(aes(xend = Date, 
                   y = Mean.Temperature,
                   yend = 0,
                   color = "Temperature")) +
  geom_point(aes(y = Mean.Temperature,
                 color = "Temperature"), 
             size = 2) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(-25, 25, 5), labels = abs(seq(-25, 25, 5))) +
  scale_colour_manual(name = "",
                      values = c("Percent" = "grey25",
                                 "Temperature" = "red")) +
  labs(y = "Percent/Temperature",
       title = " SH1252-W-04-A") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################################################################
### Prediction (regression) ---------------------------------------
###################################################################


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  dplyr::summarise(ErrorsSum = sum(ErrorsSum),
                   Placed = sum(Placed),
                   Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% 
  right_join(daily_temp, by = "Date") %>%
  select(-ErrorsSum, -Placed, -X) %>% 
  mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) -> all_dates_full_data



for (cname in colnames(all_dates_full_data[,-c(1,2,14),])) {
  for (lagvalue in 1:5) {
    newname <- paste0(cname, "Lag",lagvalue)
    temp_lag <- lag(all_dates_full_data[cname] %>% .[[1]], lagvalue) 
    all_dates_full_data[[newname]] <- temp_lag
  }
}


all_dates_full_data %>% 
  select(-Date) %>% 
  rowwise() %>% 
  mutate(Min.Temperature.Cap = pmin(Min.Temperature, 0),
         Min.TemperatureLag1.Cap = pmin(Min.TemperatureLag1, 0),
         Min.TemperatureLag2.Cap = pmin(Min.TemperatureLag2, 0),
         Min.TemperatureLag3.Cap = pmin(Min.TemperatureLag3, 0),
         Min.TemperatureLag4.Cap = pmin(Min.TemperatureLag4, 0),
         Min.TemperatureLag5.Cap = pmin(Min.TemperatureLag5, 0)) %>% 
  na.omit() -> all_dates_full_data


set.seed(2016)


inTraining <- createDataPartition(all_dates_full_data$Percent, p = .80, list = FALSE)
training <- all_dates_full_data[ inTraining, ]
testing  <- all_dates_full_data[-inTraining, ]


data.frame(real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = real,
                color = "Real values")) +
  geom_point(aes(y = real,
                 color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black")) 


fitControl <- trainControl(method = "cv",
                           number = 10)


## Random forest


tunegrid_rf <- expand.grid(.mtry=c(1:15))


model_rf <- train(Percent ~ ., 
                  data = training, 
                  method = "rf",
                  tuneGrid = tunegrid_rf,
                  trControl = fitControl,
                  importance = T)

print(model_rf)
plot(model_rf)


varImp(model_rf, scale = TRUE)


data.frame(prediction_rf = predict(model_rf, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_rf,
                color = "Prediction RF")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction RF" = "red")) 

getTrainPerf(model_rf)


rmse_testing_rf <- caret::RMSE(predict(model_rf, newdata = testing), 
                               testing$Percent)
rmse_testing_rf


## Random forest small


importantVars <- data.frame(var = varImp(model_rf, scale = TRUE)$importance  %>% 
                                  row.names(),
                            varImp(model_rf, scale = TRUE)$importance[1],
                            row.names = NULL,
                            stringsAsFactors = FALSE) %>% 
                 arrange(-Overall) %>% 
                 head(10) %>% 
                 .[[1]] %>% 
                 c("Percent",.)


model_rf_small <- train(Percent ~ ., 
                        data = training[ ,importantVars], 
                        method = "rf",
                        tuneGrid = tunegrid_rf,
                        trControl = fitControl,
                        importance = T)


print(model_rf_small)
plot(model_rf_small)


varImp(model_rf_small, scale = TRUE)


data.frame(prediction_rf_small = predict(model_rf_small, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_rf_small,
                color = "Prediction RF small")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction RF small" = "red"))

getTrainPerf(model_rf_small)


rmse_testing_rf_small <- caret::RMSE(predict(model_rf_small, newdata = testing), 
                                     testing$Percent)
rmse_testing_rf_small


## GBM


tunegrid_gbm <- expand.grid(interaction.depth = c(1,2,3,4),
                            n.trees = seq(1, 100, 10),
                            shrinkage = c(0.1,0.05),
                            n.minobsinnode = c(3,5,7))


model_gbm <- train(Percent ~ ., 
                   data = training, 
                   method = "gbm",
                   tuneGrid = tunegrid_gbm,
                   trControl = fitControl,
                   verbose = FALSE)


print(model_gbm)
plot(model_gbm)


varImp(model_gbm, scale = TRUE)


data.frame(prediction_gbm = predict(model_gbm, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_gbm,
                color = "Prediction GBM")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction GBM" = "red")) 


getTrainPerf(model_gbm)


rmse_testing_gbm <- caret::RMSE(predict(model_gbm, newdata = testing), 
                                testing$Percent)
rmse_testing_gbm


## GBM small


importantVars <- data.frame(var = varImp(model_gbm, scale = TRUE)$importance  %>% 
                              row.names(),
                            varImp(model_gbm, scale = TRUE)$importance[1],
                            row.names = NULL,
                            stringsAsFactors = FALSE) %>% 
  arrange(-Overall) %>% 
  head(10) %>% 
  .[[1]] %>% 
  c("Percent",.)


model_gbm_small <- train(Percent ~ ., 
                         data = training[ ,importantVars],
                         method = "gbm",
                         tuneGrid = tunegrid_gbm,
                         trControl = fitControl,
                         verbose = FALSE)


print(model_gbm_small)
plot(model_gbm_small)


varImp(model_gbm_small, scale = TRUE)


data.frame(prediction_bgm_small = predict(model_gbm_small, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_bgm_small,
                color = "Prediction GBM small")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction GBM small" = "red"))

getTrainPerf(model_gbm_small)


rmse_testing_gbm_small <- caret::RMSE(predict(model_gbm_small, newdata = testing), 
                                     testing$Percent)
rmse_testing_gbm_small


# lmStepAIC


model_lmStepAIC <- train(Percent ~ ., 
                         data = training, 
                         method = "lmStepAIC",
                         trControl = fitControl,
                         verbose = FALSE,
                         importance = T)


print(model_lmStepAIC)


data.frame(prediction_lmStep = predict(model_lmStepAIC, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_lmStep,
                color = "Prediction lmStep")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction lmStep" = "red")) 


getTrainPerf(model_lmStepAIC)


rmse_testing_lmStepAIC <- caret::RMSE(predict(model_lmStepAIC, newdata = testing), 
                                      testing$Percent)
rmse_testing_lmStepAIC


# lasso

tunegrid_lasso <- expand.grid(fraction  = seq(0.01, 0.3, 0.01))


model_lasso <- train(Percent ~ ., 
                     data = training, 
                     method = "lasso",
                     tuneGrid = tunegrid_lasso,
                     trControl = fitControl)


print(model_lasso)


data.frame(prediction_lasso = predict(model_lasso, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_lasso,
                color = "Prediction lasso")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction lasso" = "red")) 


getTrainPerf(model_lasso)


rmse_testing_lasso <- caret::RMSE(predict(model_lasso, newdata = testing), 
                                  testing$Percent)
rmse_testing_lasso


# pcr

model_pcr <- train(Percent ~ ., 
                   data = training, 
                   method = "pcr",
                   trControl = fitControl,
                   importance = T)


print(model_pcr)


data.frame(prediction_pcr = predict(model_pcr, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_pcr,
                color = "Prediction pcr")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction pcr" = "red")) 


getTrainPerf(model_pcr)


rmse_testing_pcr <- caret::RMSE(predict(model_pcr, newdata = testing), 
                                testing$Percent)
rmse_testing_pcr


# knn


tunegrid_knn <- expand.grid(k  = seq(1, 20, 1))

model_knn <- train(Percent ~ ., 
                   data = training, 
                   method = "knn",
                   tuneGrid = tunegrid_knn,
                   trControl = fitControl,
                   importance = T)

print(model_knn)


data.frame(prediction_knn = predict(model_knn, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_knn,
                color = "Prediction knn")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction knn" = "red")) 


getTrainPerf(model_knn)


rmse_testing_knn <- caret::RMSE(predict(model_knn, newdata = testing), 
                                testing$Percent)
rmse_testing_knn


# LM 1 variable


model_lm1var <- train(Percent ~ ., 
                      data = training[, c("Percent", "Min.TemperatureLag1.Cap")], 
                      method = "lm",
                      trControl = fitControl,
                      importance = T)

print(model_lm1var)
model_lm1var %>% summary()


data.frame(prediction_lm1var = predict(model_lm1var, newdata = testing),
           real = testing$Percent,
           val = 1:length(testing$Percent)) %>% 
  ggplot(aes(x = val)) +
  geom_line(aes(y = prediction_lm1var,
                color = "Prediction lm1var")) +
  geom_line(aes(y = real,
                color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black",
                                 "Prediction lm1var" = "red")) 


getTrainPerf(model_lm1var)


rmse_testing_lm1var <- caret::RMSE(predict(model_lm1var, newdata = testing), 
                                   testing$Percent)
rmse_testing_lm1var


# Regression results


results <- resamples(list(rf = model_rf,
                          rf_small = model_rf_small,
                          gbm = model_gbm,
                          gbm_small = model_gbm_small,
                          lmStepAIC = model_lmStepAIC,
                          lasso = model_lasso,
                          pcr = model_pcr,
                          knn = model_knn,
                          lm1var = model_lm1var))

summary(results)

bwplot(results)

dotplot(results)


###################################################################
### Prediction (classification) -----------------------------------
###################################################################


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
         -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  dplyr::summarise(ErrorsSum = sum(ErrorsSum),
                   Placed = sum(Placed),
                   Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% 
  right_join(daily_temp, by = "Date") %>%
  select(-ErrorsSum, -Placed, -X) %>% 
  mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) -> all_dates_full_data



for (cname in colnames(all_dates_full_data[,-c(1,2,14),])) {
  for (lagvalue in 1:5) {
    newname <- paste0(cname, "Lag",lagvalue)
    temp_lag <- lag(all_dates_full_data[cname] %>% .[[1]], lagvalue) 
    all_dates_full_data[[newname]] <- temp_lag
  }
}


all_dates_full_data %>% 
  select(-Date) %>% 
  rowwise() %>% 
  mutate(Min.Temperature.Cap = pmin(Min.Temperature, 0),
         Min.TemperatureLag1.Cap = pmin(Min.TemperatureLag1, 0),
         Min.TemperatureLag2.Cap = pmin(Min.TemperatureLag2, 0),
         Min.TemperatureLag3.Cap = pmin(Min.TemperatureLag3, 0),
         Min.TemperatureLag4.Cap = pmin(Min.TemperatureLag4, 0),
         Min.TemperatureLag5.Cap = pmin(Min.TemperatureLag5, 0)) %>% 
  na.omit() -> all_dates_full_data


all_dates_full_data %>% 
  ggplot(aes(Percent)) +
  geom_histogram(bins = 400) +
  geom_vline(xintercept = 0.5, 
             color = "red",
             linetype = 2)


all_dates_full_data %>% 
  mutate(Percent_bin = as.factor(ifelse(Percent < 0.2, 
                                        "Below", 
                                        "Above"))) %>% 
  select(-Percent) -> all_dates_full_data_bin
  

all_dates_full_data_bin %>% 
  select(Percent_bin) %>% 
  table()


set.seed(2016)

inTrainingC <- createDataPartition(all_dates_full_data_bin$Percent_bin, p = .80, list = FALSE)
trainingC <- all_dates_full_data_bin[ inTrainingC, ]
testingC  <- all_dates_full_data_bin[-inTrainingC, ]

testingC$Percent_bin %>% 
  table()


data.frame(real = testingC$Percent_bin,
           val = 1:length(testingC$Percent_bin)) %>% 
  ggplot(aes(x = val)) +
  geom_point(aes(y = real,
                 color = "Real values")) +
  scale_colour_manual(name = "",
                      values = c("Real values" = "black")) 


fitControl <- trainControl(method = "cv",
                           number = 10)


## GBM


tunegrid_gbm <- expand.grid(interaction.depth = c(1,2),
                            n.trees = seq(1,100,10),
                            shrinkage = c(0.1),
                            n.minobsinnode = c(3,5,7))


modelC_gbm <- train(Percent_bin ~ ., 
                    data = trainingC,
                    method = "gbm",
                    trControl = fitControl,
                    tuneGrid = tunegrid_gbm,
                    verbose = FALSE)

modelC_gbm
plot(modelC_gbm)


test_pred_gbm <- predict(modelC_gbm, testingC)

confusionMatrix(test_pred_gbm, testingC$Percent_bin)   

test_pred_gbm_prob <- predict(modelC_gbm, testingC, type="prob")


test_pred_gbm_ROC <- roc(predictor = test_pred_gbm_prob$Below,
                         response = testingC$Percent_bin,
                         levels = rev(levels(testingC$Percent_bin)))

test_pred_gbm_ROC$auc

plot(test_pred_gbm_ROC, main = "GBM ROC")


varImp(modelC_gbm, scale = TRUE)


# GBM small


importantVars <- data.frame(var = varImp(modelC_gbm, scale = TRUE)$importance  %>% 
                              row.names(),
                            varImp(modelC_gbm, scale = TRUE)$importance[1],
                            row.names = NULL,
                            stringsAsFactors = FALSE) %>% 
  arrange(-Overall) %>% 
  head(10) %>% 
  .[[1]] %>% 
  c("Percent_bin",.)


tunegrid_gbm <- expand.grid(interaction.depth = c(1,2),
                            n.trees = seq(1,100,10),
                            shrinkage = c(0.1),
                            n.minobsinnode = c(3,5,7))


modelC_gbm_small <- train(Percent_bin ~ ., 
                          data = trainingC[ ,importantVars],
                          method = "gbm",
                          tuneGrid = tunegrid_gbm,
                          trControl = fitControl,
                          verbose = FALSE)

modelC_gbm_small
plot(modelC_gbm_small)


test_pred_gbm_small <- predict(modelC_gbm_small, testingC)

confusionMatrix(test_pred_gbm_small, testingC$Percent_bin)   

test_pred_gbm_small_prob <- predict(modelC_gbm_small, testingC, type="prob")


test_pred_gbm_ROC <- roc(predictor = test_pred_gbm_small_prob$Below,
                         response = testingC$Percent_bin,
                         levels = rev(levels(testingC$Percent_bin)))

test_pred_gbm_ROC$auc

plot(test_pred_gbm_ROC, main = "GBM SMALL ROC")


varImp(modelC_gbm_small, scale = TRUE)


## Random forest


tunegrid_rf <- expand.grid(.mtry=c(1:15))


modelC_rf <- train(Percent_bin ~ ., 
                   data = trainingC, 
                   method = "rf",
                   tuneGrid = tunegrid_rf,
                   trControl = fitControl,
                   importance = T)

print(modelC_rf)
plot(modelC_rf)


varImp(modelC_rf, scale = TRUE)


test_pred_modelC_rf <- predict(modelC_rf, testingC)

confusionMatrix(test_pred_modelC_rf, testingC$Percent_bin)   

test_pred_modelC_rf_prob <- predict(modelC_rf, testingC, type="prob")


test_pred_modelC_rf_ROC <- roc(predictor = test_pred_modelC_rf_prob$Below,
                               response = testingC$Percent_bin,
                               levels = rev(levels(testingC$Percent_bin)))

test_pred_modelC_rf_ROC$auc

plot(test_pred_modelC_rf_ROC, main = "RF ROC")


# Random forest small


importantVars <- data.frame(var = varImp(modelC_rf, scale = TRUE)$importance  %>% 
                              row.names(),
                            varImp(modelC_rf, scale = TRUE)$importance[1],
                            row.names = NULL,
                            stringsAsFactors = FALSE) %>% 
  arrange(-Above) %>% 
  head(10) %>% 
  .[[1]] %>% 
  c("Percent_bin",.)


tunegrid_rf <- expand.grid(.mtry=c(1:15))


modelC_rf_small <- train(Percent_bin ~ ., 
                         data = trainingC[ ,importantVars],
                         method = "rf",
                         tuneGrid = tunegrid_rf,
                         trControl = fitControl,
                         importance = T)


print(modelC_rf_small)
plot(modelC_rf_small)


varImp(modelC_rf_small, scale = TRUE)


test_pred_modelC_rf_small <- predict(modelC_rf_small, testingC)

confusionMatrix(test_pred_modelC_rf_small, testingC$Percent_bin)   

test_pred_modelC_rf_small_prob <- predict(modelC_rf_small, testingC, type="prob")


test_pred_modelC_rf_small_ROC <- roc(predictor = test_pred_modelC_rf_small_prob$Below,
                                     response = testingC$Percent_bin,
                                     levels = rev(levels(testingC$Percent_bin)))

test_pred_modelC_rf_small_ROC$auc

plot(test_pred_modelC_rf_small_ROC, main = "RF SMALL ROC")


# LDA


modelC_lda <- train(Percent_bin ~ ., 
                    data = trainingC,
                    method = "lda",
                    trControl = fitControl,
                    importance = T)


print(modelC_lda)


test_pred_modelC_lda <- predict(modelC_lda, testingC)

confusionMatrix(test_pred_modelC_lda, testingC$Percent_bin)   

test_pred_modelC_lda_prob <- predict(modelC_lda, testingC, type="prob")


test_pred_modelC_lda_ROC <- roc(predictor = test_pred_modelC_lda_prob$Below,
                                response = testingC$Percent_bin,
                                levels = rev(levels(testingC$Percent_bin)))

test_pred_modelC_lda_ROC$auc

plot(test_pred_modelC_lda_ROC, main = "LDA ROC")


# RPART2


tunegrid_rpart2 <- expand.grid(maxdepth = c(1))

modelC_rpart2 <- train(Percent_bin ~ ., 
                       data = trainingC,
                       method = "rpart2",
                       tuneGrid = tunegrid_rpart2,
                       trControl = fitControl)


print(modelC_rpart2)
plot(modelC_rpart2)
modelC_rpart2$finalModel


test_pred_modelC_rpart2 <- predict(modelC_rpart2, testingC)

confusionMatrix(test_pred_modelC_rpart2, testingC$Percent_bin)   



# stepLDA


tunegrid_stepLDA <- expand.grid(maxvar = c(2),
                                direction = "forward")

modelC_stepLDA <- train(Percent_bin ~ ., 
                        data = trainingC,
                        method = "stepLDA",
                        tuneGrid = tunegrid_stepLDA,
                        trControl = fitControl)


print(modelC_stepLDA)
plot(modelC_stepLDA)
modelC_stepLDA$finalModel


test_pred_modelC_stepLDA <- predict(modelC_stepLDA, testingC)

confusionMatrix(test_pred_modelC_stepLDA, testingC$Percent_bin)



# svmRadial


modelC_svmRadial <- train(Percent_bin ~ ., 
                          data = trainingC,
                          method = "svmRadial",
                          trControl = fitControl)


print(modelC_svmRadial)
plot(modelC_svmRadial)
modelC_svmRadial$finalModel


test_pred_modelC_svmRadial <- predict(modelC_svmRadial, testingC)

confusionMatrix(test_pred_modelC_svmRadial, testingC$Percent_bin)


# xgbLinear


modelC_xgbLinear <- train(Percent_bin ~ ., 
                          data = trainingC,
                          method = "xgbLinear",
                          trControl = fitControl)


print(modelC_xgbLinear)
plot(modelC_xgbLinear)


test_pred_modelC_xgbLinear <- predict(modelC_xgbLinear, testingC)

confusionMatrix(test_pred_modelC_xgbLinear, testingC$Percent_bin)


# xgbTree


modelC_xgbTree <- train(Percent_bin ~ ., 
                        data = trainingC,
                        method = "xgbTree",
                        trControl = fitControl)


print(modelC_xgbTree)
plot(modelC_xgbTree)


test_pred_modelC_xgbTree <- predict(modelC_xgbTree, testingC)

confusionMatrix(test_pred_modelC_xgbTree, testingC$Percent_bin)


# glm1var


modelC_glm1var <- train(Percent_bin ~ ., 
                        data = trainingC[,c("Percent_bin",
                                            "Min.TemperatureLag1.Cap")],
                        method = "glm",
                        trControl = fitControl)


print(modelC_glm1var)
summary(modelC_glm1var$finalModel)


test_pred_modelC_glm1var <- predict(modelC_glm1var, testingC)

confusionMatrix(test_pred_modelC_glm1var, testingC$Percent_bin)


# Classification results


resultsC <- resamples(list(rf = modelC_rf,
                           rf_small = modelC_rf_small,
                           gbm = modelC_gbm,
                           gbm_small = modelC_gbm_small,
                           lda = modelC_lda,
                           rpart2 = modelC_rpart2,
                           stepLDA = modelC_stepLDA,
                           svmRadial = modelC_svmRadial,
                           xgbLinear = modelC_xgbLinear,
                           xgbTree = modelC_xgbTree,
                           glm1var = modelC_glm1var))


bwplot(resultsC)

dotplot(resultsC)


cat("\014")
confusionMatrix(test_pred_gbm, testingC$Percent_bin) 
confusionMatrix(test_pred_gbm_small, testingC$Percent_bin)
confusionMatrix(test_pred_modelC_rf, testingC$Percent_bin)  
confusionMatrix(test_pred_modelC_rf_small, testingC$Percent_bin)  
confusionMatrix(test_pred_modelC_lda, testingC$Percent_bin)  
confusionMatrix(test_pred_modelC_rpart2, testingC$Percent_bin)
confusionMatrix(test_pred_modelC_stepLDA, testingC$Percent_bin)
confusionMatrix(test_pred_modelC_svmRadial, testingC$Percent_bin)
confusionMatrix(test_pred_modelC_xgbLinear, testingC$Percent_bin)
confusionMatrix(test_pred_modelC_xgbTree, testingC$Percent_bin)
confusionMatrix(test_pred_modelC_glm1var, testingC$Percent_bin)


###################################################################
### Prediction with 3 variables (classification) ------------------
###################################################################


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  dplyr::select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
                -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  dplyr::select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  dplyr::summarise(ErrorsSum = sum(ErrorsSum),
                   Placed = sum(Placed),
                   Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% 
  right_join(daily_temp, by = "Date") %>%
  dplyr::select(-ErrorsSum, -Placed, -X) %>% 
  mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) -> all_dates_full_data



for (cname in colnames(all_dates_full_data[,-c(1,2,14),])) {
  for (lagvalue in 1:5) {
    newname <- paste0(cname, "Lag",lagvalue)
    temp_lag <- lag(all_dates_full_data[cname] %>% .[[1]], lagvalue) 
    all_dates_full_data[[newname]] <- temp_lag
  }
}


all_dates_full_data %>% 
  dplyr::select(-Date) %>% 
  rowwise() %>% 
  mutate(Min.Temperature.Cap = pmin(Min.Temperature, 0),
         Min.TemperatureLag1.Cap = pmin(Min.TemperatureLag1, 0),
         Min.TemperatureLag2.Cap = pmin(Min.TemperatureLag2, 0),
         Min.TemperatureLag3.Cap = pmin(Min.TemperatureLag3, 0),
         Min.TemperatureLag4.Cap = pmin(Min.TemperatureLag4, 0),
         Min.TemperatureLag5.Cap = pmin(Min.TemperatureLag5, 0)) %>% 
  dplyr::select(Percent, Max.TemperatureLag2, 
                Min.TemperatureLag1.Cap, Min.TemperatureLag3) %>% 
  na.omit() -> all_dates_full_data


all_dates_full_data %>% 
  ggplot(aes(Percent)) +
  geom_histogram(bins = 400) +
  geom_vline(xintercept = 0.2, 
             color = "red",
             linetype = 2)


all_dates_full_data %>% 
  mutate(Percent_bin = as.factor(ifelse(Percent < 0.2, 
                                        "Below", 
                                        "Above"))) %>% 
  dplyr::select(-Percent) -> all_dates_full_data_bin


all_dates_full_data_bin %>% 
  dplyr::select(Percent_bin) %>% 
  table()


set.seed(2016)

inTrainingC <- createDataPartition(all_dates_full_data_bin$Percent_bin, p = .80, list = FALSE)
trainingC <- all_dates_full_data_bin[ inTrainingC, ]
testingC  <- all_dates_full_data_bin[-inTrainingC, ]

testingC$Percent_bin %>% 
  table()


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


## gbm


tunegrid_gbm <- expand.grid(interaction.depth = c(2),
                            n.trees = c(11),
                            shrinkage = c(0.1),
                            n.minobsinnode = c(3))


modelC_gbm <- train(Percent_bin ~ ., 
                    data = trainingC,
                    method = "gbm",
                    metric = "ROC",
                    trControl = fitControl,
                    tuneGrid = tunegrid_gbm,
                    verbose = FALSE)

modelC_gbm

getTrainPerf(modelC_gbm)

varImp(modelC_gbm, scale = TRUE)


## rf


tunegrid_rf <- expand.grid(.mtry=c(1))


modelC_rf <- train(Percent_bin ~ ., 
                   data = trainingC, 
                   method = "rf",
                   metric = "ROC",
                   tuneGrid = tunegrid_rf,
                   trControl = fitControl,
                   importance = T)

print(modelC_rf)

getTrainPerf(modelC_rf)

varImp(modelC_rf, scale = TRUE)


# lda


modelC_lda <- train(Percent_bin ~ ., 
                    data = trainingC,
                    method = "lda",
                    metric = "ROC",
                    trControl = fitControl,
                    importance = T)


print(modelC_lda)

getTrainPerf(modelC_lda)


# RPART2


tunegrid_rpart2 <- expand.grid(maxdepth = c(1))

modelC_rpart2 <- train(Percent_bin ~ ., 
                       data = trainingC,
                       method = "rpart2",
                       metric = "ROC",
                       tuneGrid = tunegrid_rpart2,
                       trControl = fitControl)


print(modelC_rpart2)
modelC_rpart2$finalModel

getTrainPerf(modelC_rpart2)


# svmRadial


modelC_svmRadial <- train(Percent_bin ~ ., 
                          data = trainingC,
                          method = "svmRadial",
                          metric = "ROC",
                          trControl = fitControl)


print(modelC_svmRadial)

modelC_svmRadial$finalModel

getTrainPerf(modelC_svmRadial)


# xgbLinear


tunegrid_xgbLinear <- expand.grid(nrounds = c(50),
                                  lambda = c(0),
                                  alpha = c(0.1),
                                  eta = c(0.3))

modelC_xgbLinear <- train(Percent_bin ~ ., 
                          data = trainingC,
                          method = "xgbLinear",
                          metric = "ROC",
                          tuneGrid = tunegrid_xgbLinear,
                          trControl = fitControl)


print(modelC_xgbLinear)

getTrainPerf(modelC_xgbLinear)


# xgbTree


tunegrid_xgbTree <- expand.grid(nrounds = c(150),
                                max_depth = c(1),
                                gamma = c(0),
                                eta = c(0.4),
                                colsample_bytree = c(0.8),
                                min_child_weight = c(1))


modelC_xgbTree <- train(Percent_bin ~ ., 
                        data = trainingC,
                        method = "xgbTree",
                        metric = "ROC",
                        tuneGrid = tunegrid_xgbTree,
                        trControl = fitControl)


print(modelC_xgbTree)

getTrainPerf(modelC_xgbTree)


# glm


modelC_glm <- train(Percent_bin ~ ., 
                    data = trainingC,
                    method = "glm",
                    metric = "ROC",
                    trControl = fitControl)


print(modelC_glm)
summary(modelC_glm$finalModel)

getTrainPerf(modelC_glm)


# glm1var


modelC_glm1var <- train(Percent_bin ~ ., 
                        data = trainingC[,c("Percent_bin",
                                            "Min.TemperatureLag1.Cap")],
                        method = "glm",
                        metric = "ROC",
                        trControl = fitControl)


print(modelC_glm1var)

getTrainPerf(modelC_glm1var)


# Classification with 3 variables results

resultsC <- resamples(list(rf = modelC_rf,
                           gbm = modelC_gbm,
                           lda = modelC_lda,
                           rpart2 = modelC_rpart2,
                           svmRadial = modelC_svmRadial,
                           xgbLinear = modelC_xgbLinear,
                           xgbTree = modelC_xgbTree,
                           glm = modelC_glm,
                           glm1var = modelC_glm1var))


# bwplot(resultsC)

dotplot(resultsC)

confusionMatrix(modelC_gbm)


# save(modelC_gbm, file = "models/gbm_modelC_for_errors.RData")
# save(modelC_rpart2, file = "models/rpart_modelC_for_errors.RData")
# save(modelC_glm1var, file = "models/glm_modelC_for_errors.RData")


###################################################################
### Final predictions from loaded models (classification) ---------
###################################################################


suppressPackageStartupMessages(require(caret))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(knitr))

# nustatomas darbinis katalogas priklausomai nuo kompo
if (Sys.info()[[4]] == "ANDRIUS-PC")
  setwd("D:/-=Works=-/R/GitHub/LTLABerrors_R0402_analysis")

if (Sys.info()[[4]] == "LTLAB-DEV")
  setwd("C:/R/GitHub/LTLABerrors_R0402_analysis")

DBFileName <- "LTLABerrors.sqlite"
errors_db_sqlite <- src_sqlite(DBFileName)
errors_sqlite <- tbl(errors_db_sqlite, "HOUR_ERRORS")
daily_temp <- read.csv("daily_temperatures.csv", stringsAsFactors = FALSE)


errors_sqlite %>%
  filter(PackageTypeName == c("R0402")) %>%
  filter(Date >= "2015-01-01", Date < "2016-07-01") %>%
  group_by(Date) %>%
  dplyr::select(-PackageTypeName, -Machine, -Time, -ProcessProgramID, -PMSlot, 
                -FeederSlot, -FeederLane, -FeederType) %>%
  summarise_each(funs(sum)) %>% 
  as.data.frame() %>%
  mutate(., ErrorsSum  = rowSums(.[ ,2:12])) %>%
  mutate(., Placed  = rowSums(.[ ,13:14])) %>%
  dplyr::select(-(AutoRepickAfterPickError:NumberOfCAlignPlacements)) %>%
  group_by(Date) %>%
  dplyr::summarise(ErrorsSum = sum(ErrorsSum),
                   Placed = sum(Placed),
                   Percent = as.numeric(paste(round(ErrorsSum/Placed*100,2), sep = ""))) %>%
  filter(Placed > 1000) %>% 
  right_join(daily_temp, by = "Date") %>%
  dplyr::select(-ErrorsSum, -Placed, -X) %>% 
  mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) -> all_dates_full_data



for (cname in colnames(all_dates_full_data[,-c(1,2,14),])) {
  for (lagvalue in 1:5) {
    newname <- paste0(cname, "Lag",lagvalue)
    temp_lag <- lag(all_dates_full_data[cname] %>% .[[1]], lagvalue) 
    all_dates_full_data[[newname]] <- temp_lag
  }
}


all_dates_full_data %>% 
  dplyr::select(-Date) %>% 
  rowwise() %>% 
  mutate(Min.Temperature.Cap = pmin(Min.Temperature, 0),
         Min.TemperatureLag1.Cap = pmin(Min.TemperatureLag1, 0),
         Min.TemperatureLag2.Cap = pmin(Min.TemperatureLag2, 0),
         Min.TemperatureLag3.Cap = pmin(Min.TemperatureLag3, 0),
         Min.TemperatureLag4.Cap = pmin(Min.TemperatureLag4, 0),
         Min.TemperatureLag5.Cap = pmin(Min.TemperatureLag5, 0)) %>% 
  dplyr::select(Percent, Max.TemperatureLag2, 
                Min.TemperatureLag1.Cap, Min.TemperatureLag3) %>% 
  na.omit() -> all_dates_full_data


all_dates_full_data %>% 
  mutate(Percent_bin = as.factor(ifelse(Percent < 0.2, 
                                        "Below", 
                                        "Above"))) %>% 
  .[c(2,3,4,5,1)]-> all_dates_full_data_bin


predict_errors <- function(data) {
  suppressPackageStartupMessages({
    load("models/gbm_modelC_for_errors.RData")
    load("models/rpart_modelC_for_errors.RData")
    load("models/glm_modelC_for_errors.RData")
    
    print("Error chance:")
    print(paste0("GBM:   ", 
                 round(predict(modelC_gbm, data, type = "prob")$Above*100, 0),
                 "%"))
    print(paste0("RPART: ", 
                 round(predict(modelC_rpart2, data, type = "prob")$Above*100, 0),
                 "%"))
    print(paste0("GLM:   ", 
                 round(predict(modelC_glm1var, data, type = "prob")$Above*100, 0),
                 "%"))
  })
}


test_data <- all_dates_full_data_bin[sample(dim(all_dates_full_data_bin)[1], 1), ]
cat("\014")
kable(test_data %>% 
      mutate(Percent_bin = ifelse(Percent_bin == "Above", "Error", "Good")))
predict_errors(test_data)


##################################################################################
### Predictions of good/bad placement of given day (classification) --------------
##################################################################################


suppressPackageStartupMessages(require(caret))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(rvest))
suppressPackageStartupMessages(require(scales))


# nustatomas darbinis katalogas priklausomai nuo kompo
if (Sys.info()[[4]] == "ANDRIUS-PC")
  setwd("D:/-=Works=-/R/GitHub/LTLABerrors_R0402_analysis")

if (Sys.info()[[4]] == "LTLAB-DEV")
  setwd("C:/R/GitHub/LTLABerrors_R0402_analysis")


predict_errors <- function(data) {
  suppressPackageStartupMessages({
    load("models/gbm_modelC_for_errors.RData")
    load("models/rpart_modelC_for_errors.RData")
    load("models/glm_modelC_for_errors.RData")
    
    cat("Error chance:", "\n")
    cat(paste0("GBM:   ", 
               round(predict(modelC_gbm, data, type = "prob")$Above*100, 0),
               "%"),
        "\n")
    cat(paste0("RPART: ", 
               round(predict(modelC_rpart2, data, type = "prob")$Above*100, 0),
               "%"),
        "\n")
    cat(paste0("GLM:   ", 
               round(predict(modelC_glm1var, data, type = "prob")$Above*100, 0),
               "%"),
        "\n")
  })
}


predict_date_errors <- function(date) {
  
  pb <- txtProgressBar(style = 3)
  
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
  
  date <- as.Date(date)
  
  all_dates <- c(date - days(1),
                 date - days(2),
                 date - days(3))

  all_data <- list()
  
  inc <- 0

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
    }
    
    date_data[["Date"]] <- date
    all_data[[as.character(date)]] <- as.data.frame(date_data, stringsAsFactors = FALSE)
    
    Sys.sleep(3)
    inc <- inc + 0.3
    setTxtProgressBar(pb, inc)
  }
  
  all_data_df <- do.call(rbind, all_data)
  
  all_dates <- as.character(all_dates)
  
  data.frame(Max.TemperatureLag2 = all_data_df[all_dates[2], "Max.Temperature"] %>% 
                                   as.integer(),
             Min.TemperatureLag3 = all_data_df[all_dates[3], "Min.Temperature"] %>% 
                                   as.integer(),
             Min.TemperatureLag1.Cap = all_data_df[all_dates[1], "Min.Temperature"] %>% 
                                       as.integer() %>% 
                                       pmin(0)) -> data
  
  setTxtProgressBar(pb, 1)
  close(pb)
  
  cat("-----------------------------------------------------------------", "\n")
  cat("---", as.character(date), "---", "\n")
  cat("-----------------------------------------------------------------", "\n")
  print(data)
  cat("-----------------------------------------------------------------", "\n")
  predict_errors(data)
}

predict_date_errors("2016-01-14")









