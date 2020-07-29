# Create a vector that holds the quantile values for the 20, 40, 60, and 80th percentile of stnPressure.
# Create a new column called pressureLevel that creates five evenly spaced levels for stnPressure
# Make a boxplot of windSusSpeed vs pressureLevel
# Find the dates for the three outliers at the lowest pressure level.  The three outliers are also the three highest windSusSpeed for the year.
# Label the outliers for the lowest pressure level with the dates on the plot.

#Part 1: Create vector with quantile values for stnPressure
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

#quantile finds percentile values
#probs gives the quantile values we want
stnPressureQuant = quantile(weatherData$stnPressure, 
                            probs=c(.20, .40, .60, .80));


#Part 2: iterate through pressureLevel to assign 5 even levels 
for(day in 1:nrow(weatherData)) # nrow(weatherData) = 366
{
  if(weatherData$stnPressure[day] <= stnPressureQuant[1]) 
  {
    weatherData$pressureLevel[day] = "Very Low";
  }

  else if(weatherData$stnPressure[day] > stnPressureQuant[1] && 
          weatherData$stnPressure[day] <= stnPressureQuant[2]) 
  {
    weatherData$pressureLevel[day] = "Low";
  }
  else if(weatherData$stnPressure[day] > stnPressureQuant[2] &&
          weatherData$stnPressure[day] <= stnPressureQuant[3])
  {
    weatherData$pressureLevel[day] = "Medium";
  }
 else if(weatherData$stnPressure[day] > stnPressureQuant[3] &&
         weatherData$stnPressure[day] <= stnPressureQuant[4])
 {
   weatherData$pressureLevel[day] = "Medium-High";
 }
  
 else
  {
    weatherData$pressureLevel[day] = "High";
  }
}
#save new dataframe
write.csv(weatherData, file="data/LansingNOAA2016-6.csv");

#Part 3-5
#ordering pressure from low to high
pressureFact = factor(weatherData$pressureLevel,
                     levels=c("Very Low", "Low", "Medium", "Medium-High", "High"))

#find outliers - 3 highest windSusSpeed
#put wind speeds in order of highest to lowestest speed
highWindSusSpeed = order(weatherData$windSusSpeed, decreasing = TRUE);
topSpeeds = weatherData$windSusSpeed[highWindSusSpeed[1:3]];
topDates = weatherData$date[highWindSusSpeed[1:3]];

#plotting
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed),
               na.rm=TRUE,
               outlier.shape = 20, 
               outlier.color = "red",
               outlier.alpha = 0.6, 
               outlier.size = 3 ) +
  annotate(geom="text", x=1, y=topSpeeds[1], color="darkorange",label=topDates[1])+
  annotate(geom="text", x=1.5, color = "blue", y=topSpeeds[2], label=topDates[2])+
  annotate(geom="text", x=2, color="black",  y=topSpeeds[3], label=topDates[3])+
  theme_bw() +
  labs(title = "Wind Speed v. Pressure Level",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure Level",
       y = "Wind Speed");
plot(thePlot);


