{source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#make 30th and 70th percentil for relHum
humidityQuant = quantile(weatherData$relHum, 
                         probs=c(.30, .70));

#Part 2: iterate through pressureLevel to assign 3 even levels 
for(day in 1:nrow(weatherData)) 
{
  if(weatherData$relHum[day] <= humidityQuant[1]) 
  {
    weatherData$humidityLevel[day] = "Low";
  }
  
  else if(weatherData$relHum[day] > humidityQuant[1] &&
          weatherData$relHum[day] <= humidityQuant[2])
  {
    weatherData$humidityLevel[day] = "Medium";
  }
  
  else
  {
    weatherData$humidityLevel[day] = "High";
  }
}

### Creating Plot
windLabels = c(Low = "Light Winds",
               Medium = "Medium Winds",
               High = "Strong Winds");

thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=humidityLevel, y=stnPressure, 
                          fill=factor(windDir)), 
               na.rm=TRUE)+
  theme_bw() +
  scale_x_discrete(limits = c("Low", "Medium", "High")) +
  facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High")),
             labeller=as_labeller(windLabels)) +
  labs(title = "Humidity Levels vs. Pressure",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity Levels",
       y = "Pressure",
       fill = "Wind Direction");
plot(thePlot);
}