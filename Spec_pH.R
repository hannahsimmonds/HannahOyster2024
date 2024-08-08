##install packages
library(tidyverse)
library(lubridate)

##read in the data
Spec_2324 <- read.csv("Spec_2324.csv")

# Convert month names to numeric month values
Spec_2324$Month <- month(month(ymd(paste("2000-", Spec_2324$Month, "-01", sep=""))))


Spec_2324 <- Spec_2324 %>%
  mutate(Month_Year = format(make_date(Year, Month, 1), "%Y-%m"))

write.csv(Spec_2324, "Spec_2324.csv")

#Calculate averages by date and site
averages <- Spec_2324 %>%
  group_by(Site, Month_Year) %>%
  summarise(average_pH = mean(pH, na.rm = TRUE), .groups = 'drop')



averages$Month_Year <- as.Date(paste0(averages$Month_Year, "-01"), format = "%Y-%m-%d")

# plot
pH_plot = ggplot(data = averages, aes(x = Month_Year, y = average_pH, color = Site)) +
  geom_line(size= .75, na.rm = TRUE) +
  labs(
    title = "Sampled Seawater pH Across Time and Sites",
    x = "",
    y = "pH",
    color = "Site"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")+
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    )

pH_plot
