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

for(i in 1:nrow(weatherData))
{
  if(avgTemp <= 55)
  {
    medTemps=c();
  }
  if(avgTemp > 55 && avgTemp<76)
  { 
     hotTemps=C();
  }
  else
  {
    coldTemps=c();
  }
}

#Text Plot of Precip vs. Humidity
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=precipVal, y=relHum, color=1:nrow(weatherData), label=avgTemp)) +
  scale_color_gradientn(colors = c("blue", "darkgreen", "red"),
                        breaks=c(hotTemps, medTemps, coldTemps),
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