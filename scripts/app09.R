source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#Need to convert Precip to numerics excluding 'T'

for(i in 1:nrow(weatherData))
{
  if(weatherData[i, "precip"] == "T")
  {
    weatherData[i, "precipVal"] =0.005;
  }
  else
  {
    weatherData[i, "precipVal"] = weatherData[i, "precip"];
  }
}

weatherData$precipVal = as.numeric(weatherData$precipVal)
avgTemp=weatherData$avgTemp

#Text Plot of Precip vs. Humidity
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=precipVal, y=relHum, color=1:nrow(weatherData), label=avgTemp)) +
  scale_color_gradientn(colors = c("blue", "darkgreen", "red"),
                        breaks=c(0.8, 0.65, 0.3),
                        labels=c("hot", "moderate", "cold"))+
  scale_y_continuous(trans="log10")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.5, 45)) +
  labs(title = "Humidity vs. Precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Precipitation",
       y = "Relative Humidity",
       color = "Temperature");
plot(thePlot);