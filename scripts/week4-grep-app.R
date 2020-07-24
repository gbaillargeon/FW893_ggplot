# Create a vector that holds the quantile values for the 20, 40, 60, and 80th percentile of stnPressure.
# Create a new column called pressureLevel that creates five evenly spaced levels for stnPressure
# Make a boxplot of windSusSpeed vs pressureLevel
# Find the dates for the three outliers at the lowest pressure level.  The three outliers are also the three highest windSusSpeed for the year.
# Label the outliers for the lowest pressure level with the dates on the plot.

#Part 1: Create vector with quantile values for stnPressure
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );


#values are TRUE if they contain RA or SN, FALSE otherwise
#grepl returns true false 
#grep returns an index
precipDays = grepl(x=weatherData$weatherType, pattern="RA|SN");

# days will take values from 1 to 366 (the number of rows)
for(day in 1:nrow(weatherData))  
{
  if(precipDays[day] == TRUE)  # day had either RA or SN
  {
    weatherData$precipitation[day] = 1;   # set precip to 1
  }
  else   # day had neither RA nor SN
  {
    weatherData$precipitation[day] = 0;   # set precip to 0
  }
}
weatherData$precipitation = as.character(weatherData$precipitation);

#plotting
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=precipitation, y=relHum),
               na.rm=TRUE,
               outlier.shape = 20, 
               outlier.color = "red",
               outlier.alpha = 0.6, 
               outlier.size = 3 ) +
  theme_bw() +
  labs(title = "Humidity vs. Precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity (%)",
       y = "Precipitation");

plot(thePlot);


