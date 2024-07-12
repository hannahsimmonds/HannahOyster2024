read.csv('Combined Carbonate Chemistry data.csv')
combined carbonate chemistry data
head('combined carbonate chemistry data.csv')
library(tidyverse)
ncol(Combined_Carbonate_Chemistry_data)
head(Combined_Carbonate_Chemistry_data[,c(2,13)])
monthxalk<-Combined_Carbonate_Chemistry_data[,c(2,13)]
head(monthxalk)
ggplot(data=Combined_Carbonate_Chemistry_data, aes(x=Month, y=Lab_alkalinity))
plot1<-(ggplot(data=Combined_Carbonate_Chemistry_data, aes(x=Month, y=Lab_alkalinity)))
ggplot(data=Combined_Carbonate_Chemistry_data, aes(x=Month, y=Lab_spec_pH))
plot2<-(ggplot(data=Combined_Carbonate_Chemistry_data, aes(x=Month, y=Lab_spec_pH)))

New DF <- Old DF %>% 
  group_by((Site, Month))
  summarize(mean=mean())