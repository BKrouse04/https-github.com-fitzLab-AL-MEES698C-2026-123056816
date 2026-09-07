# Libraried and installed packages listed in HW PDF
#install.packages("daymetr")
library(daymetr)
library(dplyr) 
#install.packages("tidygeocoder")
library(tidygeocoder) 
library(ggplot2)

### High latitude is: Anchorage, AK
### Medium latitude: Chicago, IL
### Low latitude: Miama, FL


### Created a data frame using the data.frame function with two columns, named
### Location and Address, and three rows. The rows are high, med, and low
### locations, which each correspond to the high, medium, and low latitude
### levels. The address column then contains a simple address for each
### location/latitude level. This dataframe is called locations
locations <- data.frame(
  Location= c(
    "High",
    "Med",
    "Low"
  ),
  Address = c(
    "Anchorage, Alaska, USA",
    "Chicago, Illinois, USA",
    "Miami, Florida, USA"
  )
)

### Used the geocode function from tidygeocoder to retrieve latitude and
### longitude coordinates for each of the chosen locations. Geocode by default
### adds new columns titled lat and long. I chose the method arcgis
locations_cord <- locations %>%
  geocode(
    address = Address,
    method = "arcgis"
  )

### Created the empty data frame that will store the data produced by the
### following for loop
daymet_all <- data.frame()

### Created a for loop that will iterate over each row of my location_cord data
### frame. For each location (i), daymet will download the weather data using
### the longitude and latitude coordinates, from 1980-2025, and then store
### everything in the empty dataframe (daymet_all) created in the previous step.
### Resulting dataframe has 50,370 rows, for 365 * 46 * 3. Daymet is only
### returning 365 days a year, regardless of whether it was a leap year. There
### were no arguments to override to fix this issue.
for (i in 1:nrow(locations_cord)) {
  
  # Download Daymet data for location i
  daymet_data <- download_daymet(
    lat = locations_cord$lat[i],
    lon = locations_cord$long[i],
    start = 1980,
    end = 2025
  )
  
  # Extract daily data and add the location column/identifier
  daymet_data <- daymet_data$data %>%
    mutate(
      Location = locations_cord$Location[i]
    )
  
  # Add to the big dataframe
  daymet_all <- bind_rows(daymet_all, daymet_data)
}

### Created a daily average temperature, and store it in a dataframe called
### daymet_temp, that only includes, location, year, day, min, max, and now mean
### temperature
daymet_temp <- daymet_all %>%
  select(Location, year, yday, tmax..deg.c., tmin..deg.c.) %>%
  mutate(
    tmean = (tmax..deg.c. + tmin..deg.c.) / 2
  )

### Created a new dataframe called summer_temp, that first filters out all
### "non-summer days" and only includes days from 170 to 260. Then groups by
### each location and year, and calculates the mean temperature for the summer.
summer_temp <- daymet_temp %>%
  filter(yday >= 170, yday <= 260) %>% 
  group_by(Location, year) %>%
  summarise(
    summer_mean_temp = mean(tmean, na.rm = TRUE),
    .groups = "drop"
  )

### Changed the location levels to factors so I could avoid them being in
### alphabetical order in the graph key
summer_temp$Location <- factor(
  summer_temp$Location,
  levels = c("High", "Med", "Low")
)

### Plotted average summer temperatures across the years in our study. Used the
### function ggplot, with year on the x axis and mean temperature on the y axis.
### Color was used to denote location. Plotted using both geom_point and
### geom_line to get point and line data, as well as using geom_smooth (default
### loess) to show the overall smooth trend of each location. Adjusted axis
### titles, legend/location names, and titled the graph. Named plot summer_plot
summer_plot <- ggplot(summer_temp, aes(x= year, y= summer_mean_temp, color= Location))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  ggtitle("Change in Yearly Average Summer Temperatures (°C) Across Three Locations")+
  labs(
    x= "Year", y= "Average Summer Temperature (°C)", color= "Location"
  )+
  scale_color_discrete(
    name = "Location",
    labels = c(
      "High" = "Anchorage, AK (High Latitude)",
      "Med" = "Chicago, IL (Medium Latitude)",
      "Low" = "Miami, FL (Low Latitude)"
    )
  )

### view plot
summer_plot

### save plot and title it
ggsave(
    "summer_temperature_plot.png",
    plot = summer_plot,
    width = 10,
    height = 7,
    units = "in",
    dpi = 300
  )



### Question 6. I would tell my grandfather and uncle that summers are getting
### hotter, but the rate of increase differs by latitude and location. For
### example, Anchorage increased by roughly 2.5 degrees celsius, while Miami did
### not increase quite as strongly. Chicago was a lot more variable across time
### than the other two locations, but still generally increased. I am fairly
### confident that I would see similar trends if I chose other locations as
### well. However, without testing, I can not be positive this is the case,
### particularly with the difference in trends across the chosen locations.
### Additionally, I would want to use statistical models to test whether the
### change is significantly increasing. I would also want to include data from
### years stretching back to before the industrial revolution, which I believe
### will show a more exaggerated increase over time. I would also do an analysis
### where I calculate based on maximum temperature, and not averaged from the
### minimum and maximum temperatures for each day. This also may show a stronger
### increase in temperatures.


### Winter Analysis


### Created a new dataframe called winter_temp. This first uses mutate to create
### an object (column) called winter_year that if the julian day (yday) is
### greater than 350, than use that year, but if not, then to use the previous
### year in the title. With this, February of 2000 would be classified as Winter
### of 1999. Then I filter out all days not in winter, ie between 60 and 350
### days. I then grouped by location and year, and found the mean of the minimum
### winter temperature for that year.
winter_temp <- daymet_temp %>%
  mutate(
    winter_year = if_else(yday >= 350, year, year - 1)
  ) %>%
  filter(
    yday >= 350 | yday <= 60
  ) %>%
  group_by(Location, winter_year) %>%
  summarise(
    winter_mean_tmin = mean(tmin..deg.c., na.rm = TRUE),
    .groups = "drop"
  )


### Changed the location levels to factors so I could avoid them being in
### alphabetical order in the graph key
winter_temp$Location <- factor(
  winter_temp$Location,
  levels = c("High", "Med", "Low")
)

### Plotted using ggplot from my dataframe winter_temp, with year on the x axis
### and mean minimum winter temperature on the y axis. Used color to denote
### between locations, as well as both geom_point and geom_line. Geom_smooth
### (default) was also used to show general smoothed trends. Then created a plot
### title, legend key, and axis titles for the graph. Named it winter_plot
winter_plot <- ggplot(winter_temp, aes(x= winter_year, y= winter_mean_tmin, color= Location))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  ggtitle("Change in Yearly Average Minimum Winter Temperatures (°C) Across Three Locations")+
  labs(
    x= "Year", y= "Average Minimum Winter Temperature (°C)", color= "Location"
  )+
  scale_color_discrete(
    name = "Location",
    labels = c(
      "High" = "Anchorage, AK (High Latitude)",
      "Med" = "Chicago, IL (Medium Latitude)",
      "Low" = "Miami, FL (Low Latitude)"
    )
  )

### view plot
winter_plot

### save plot and title it
ggsave(
  "winter_temperature_plot.png",
  plot = winter_plot,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300
)


### Question 8. Chicago and Miama have had similar trends of increasing, but
### Anchorage starts to get colder, on average, after 2015. The increasing trend
### for Miami and Chicago does appear to be stronger in winter than in summer,
### however, I averaged over daily minimum and maximum temperatures rather than
### just use the maximum values for summer. As such, I hesitate to fully compare
### the two plots, as their calculations were different. I would also want to
### include more analyses with more locations, particularly with the decrease in
### temperature in Anchorage. I would need more locations, and preferably data
### from years before 1980 to make any confident conclusions about the patterns
### shown in the graphs.
