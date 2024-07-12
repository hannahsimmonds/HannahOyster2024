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

##R can't handle our date and time formate, use lubridate to fix this
str(allDoTemp)

allDoTemp$datetime <- mdy_hm(allDoTemp$datetime)

allDoTemp$datetime<-mdy_hm(allDoTemp$datetime)
#Check end of data 
tail(allDoTemp)

##Too much data, par it down to after 4/30/24
# Filter out rows where Date.Time..GMT.07.00 is less than or equal to "2024-04-30 12:00:00"
filtered_data <- allDoTemp %>%
  filter(Date.Time..GMT.07.00 >= ymd_hms("2024-04-30 12:00:00", tz = "Etc/GMT+7"))