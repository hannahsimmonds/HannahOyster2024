##CReate a map for the oyster ploidy project.
#Based on Nick Changs script


# Load packages -----------------------------------------------------------
library(sf)
library(tidyverse)
#library(maptools)## This package has been depreciated and removed
library(ggspatial)
library(here)
library(shadowtext)#adds shadows under the text
library(usmap)
library(usmapdata)#Us map package for the overall map of the USA
library(cowplot)
library(grid)

# Read in data ------------------------------------------------------------

#shapefile of Washington's coastlines
shore = st_read("MajorShorelines.shp")# Read in Washington data
st_drivers()

# Creates a data frame with the coordinates of the sites used in t --------
Site = c("Thorndyke Bay", "Chelsea", "Manchester", "Hood Head") 
decLat = c(47.807765, 47.128306, 47.573497, 47.883779)
decLong = c(-122.739104, -122.960099, -122.545168, -122.613236)
farmType = c("Intertidal", "Intertidal", "Subtidal", "Subtidal")
Sites = data.frame(Site, decLat, decLong, farmType)




# Create a site map of puget sound ----------------------------------------
PS_site_map = ggplot() +
  #creating a shapefile layer, making the fill blue to represent water
  geom_sf(data = shore, fill = "#A4BED5FF", col = "#FED789FF")+#shapefiles can be downloaded for specific arfeas
  #Creating points on the map for each study site
  geom_point(data = Sites,
             aes(x = decLong, 
                 y = decLat, 
                 fill = Site,
                 shape = farmType), #assigning color to farm type
             size = 5, 
             col = "black") +
  #picking what colors I want each farm type to be
  scale_fill_manual(
    values =c("#C77CFF", '#F8766D', "#00BFC4", "#7CAE00" ),
    name="Site"
  )+
  scale_shape_manual(values = c(21, 22), name= "Farm Type")+
  #labs(title = "Map of Study Sites", 
  # subtitle = "Puget Sound, Washington, USA", 
  #caption = "Data: WA DOT") +
  #getting rid of axis labels
  xlab("Lattitude") + 
  ylab("Longitude") +
  #setting the lat and long limits of the map
  coord_sf(xlim = c(-123.4, -121.3), ylim = c(47, 48.2))+
  #changing the theme to minimal
  theme_minimal() +
  # Labelling each point with the name of each site
  geom_text(aes(x = decLong - .23, y = decLat, label = Site),  #Sets the location of the markers, using shadowtext for clarity
                  col = 'black', 
                  family = "Times") + 
  # Adding a scale bar
  annotation_scale(location= "bl", width_hint = .25) +
  # Adding a north arrow
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_fancy_orienteering()) +
  #changing the text style
  theme(title = element_text(face = "bold", family = "Times"),
        #changing the color of the land area (because I only have a shapefile for the sea, the land color is the same as background)
        panel.background = element_rect(fill ='#FED789FF'), 
        # Getting rid of the ticks on each axis
        #axis.text.x = element_blank(), 
        #axis.text.y = element_blank(),
        #changing the position of the legend so that is is on the map and not off to the side
        legend.position.inside = c(0.85, 0.1),
        #giving the legend a white background with a black outline
        legend.background = element_rect(fill = "white", color = "black"),
        #making the grid lines transparent
        panel.grid.major=element_line(colour="transparent"),
        panel.grid.minor=element_line(colour="transparent"),
        legend.key.size = unit(0.5, "cm")  # Adjust size of legend keys if needed
  ) +
  guides(
    fill = guide_none(),  # Remove legend for Site (fill)
    shape = guide_legend(title = "Farm Type")  # Keep legend for Farm Type
  ) 

PS_site_map


# Create a Westcoast inset map --------------------------------------------
#uses package usmap data
#plotusamap returns a ggplot object
insetmap <- plot_usmap(include = .west_region, exclude = c("AK", "HI"),#You can also select regions using plot_usmap(include = .south_region)
           fill = "#6B6969")+ 
  scale_fill_continuous()+theme(panel.background = element_rect(fill='#8BAAB8'))
                                
  
insetmap

insetmap <- insetmap +
  coord_sf(
    xlim = c(-2200000, -1140000),
    #ylim = c(5086142, 5104660),
    expand = FALSE
  )
#plot_usmap(include = c("CA", "WA"))
mapdat <- us_map( regions=c("states","state","counties","county"), include=c(), exclude=c())
#Add an inset map
ggdraw(PS_site_map) +
  draw_plot(
    {
      insetmap + 
        coord_sf(
          xlim = c(-1234000, -1220000),
          ylim = c(4700000, 4820000),
          expand = FALSE) +
        theme(legend.position = "none")
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.58, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.46, 
    height = 0.46)

