read.csv('Combined Carbonate Chemistry data.csv')
head(Combined_Carbonate_Chemistry_data)
sitemonthalk<-(Combined_Carbonate_Chemistry_data[,c(1,2,14)])
site<-(Combined_Carbonate_Chemistry_data[,c(1)])
month<-(Combined_Carbonate_Chemistry_data[,c(2)])
alk<-(Combined_Carbonate_Chemistry_data[,c(14)])
library(dplyr)
head(select(site, month, alk))
alkbymonth <- Combined_Carbonate_Chemistry_data %>% 
  group_by(Site, Month) %>% 
  summarize(mean=mean(Lab_alkalinity),error=sd(Lab_alkalinity)) 
alkbymonth

library(tidyverse)
ggplot(data=alkbymonth, aes(x=Month, y=mean, color=Site, group=Site))+
  geom_line()
  
pHbymonth <- Combined_Carbonate_Chemistry_data %>%
  group_by(Site, Month) %>% 
  summarize(pH=mean(Lab_spec_pH), error=sd(Lab_spec_pH))
baralkbymonth <- alkbymonth %>%
  