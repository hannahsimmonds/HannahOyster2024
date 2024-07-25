#Packages
library(tidyverse)

#Read in the datat
man_Do_Temp  <- read.csv("man_Do_Temp.csv")
chelsea_Do_Temp <- read.csv("chelsea_Do_Temp.csv")
thorn_Do_Temp <- read.csv("thorn_Do_Temp.csv")
hood_Do_Temp <- read.csv("hood_Do_Temp.csv")

#inspect the data
str(chelsea_Do_Temp)
str(thorn_Do_Temp)
str(hood_Do_Temp)
##merge all data into one file
allDoTemp <- rbind(man_Do_Temp, chelsea_Do_Temp, hood_Do_Temp, thorn_Do_Temp) %>% filter(datetime != "NA")

# Convert the Date.Time..GMT.07.00 column to date-time format
allDoTemp <- allDoTemp %>%
  mutate(Date.Time..GMT.07.00 = mdy_hm(Date.Time..GMT.07.00, tz = "Etc/GMT+7"))

##R can't handle our date and time format, use lubridate to fix this
str(allDoTemp)

allDoTemp$datetime <- mdy_hm(allDoTemp$datetime)

allDoTemp$datetime<-mdy_hm(allDoTemp$datetime)
#Check end of data 
tail(allDoTemp)

##Too much data, par it down to after 4/30/24

# Filter out rows where Date.Time..GMT.07.00 is less than or equal to "2024-04-30 12:00:00"
filtered_data <- allDoTemp %>%
  filter(Date.Time..GMT.07.00 >= ymd_hms("2024-04-30 12:00:00", tz = "Etc/GMT+7"))

allDoTemp %>% 
  filter(date >= "2024-04-30 12:00:00")
# Converting to POSIXct datetime
filtered_data$Date.Time..GMT.07.00 <- as.POSIXct(filtered_data$Date.Time..GMT.07.00, format = "%Y-%m-%d %H:%M:%S")
filtered_data <- filtered_data[order(filtered_data$Date.Time..GMT.07.00), ]

## make plot of Temp and DO
ggplot(data = filtered_data, aes(x = Date.Time..GMT.07.00, y = Temp_C, color = Site)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Site Temperature Over Time", x = " ", y = "Temperature, Celcius", color = "Site")
  
DO_overtime <- ggplot(data = filtered_data, aes(x = Date.Time..GMT.07.00, y = Temp_C, color = Site)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Site Temperature Over Time", x = " ", y = "Temperature, Celcius", color = "Site")


##average values by day by site, create graphs

#temp
daily_tempavg <- filtered_data %>% 
  group_by(Site, Date = as.Date(floor_date(Date.Time..GMT.07.00, "day"))) %>% #group by date
  summarize(Average_Temp = mean(Temp_C, na.rm = TRUE)) #get averages

ggplot(data= daily_tempavg, aes(x = Date, y = Average_Temp, color = Site)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Temperature Over Time", x = " ", y = "Temperature, Celcius", color = "Site")

#DO
daily_DOavg <- filtered_data %>% 
  group_by(Site, Date = as.Date(floor_date(Date.Time..GMT.07.00, "day"))) %>%
  summarize(Average_DO =mean(DO_mgL, na.rm = TRUE))

ggplot(data= daily_DOavg, aes(x = Date, y = Average_DO, color = Site)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Dissolved O2 Over Time", x = " ", y = "DO, mg/L", color = "Site")



  

