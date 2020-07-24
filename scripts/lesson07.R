source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: A different way to arrange x-axis values
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  scale_x_discrete(limits=c("North", "East", "South", "West")) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 2: Group boxplots by wind speed levels
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=windSpeedLevel),
               na.rm=TRUE) +
  scale_x_discrete(limits=c("North", "East", "South", "West")) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);


### Part 3: Re-order group as factors
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 4: Changing the legend title
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)",
       fill = "Wind Speeds");
plot(thePlot);

### Part 5: Adding color using rgb()
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  scale_fill_manual(values = c(rgb(red=1, green=1, blue=0),        # low
                               rgb(red=1, green=0.2, blue=0),      # medium
                               rgb(red=0.5, green=0, blue=0.8))) + # high
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)",
       fill = "Wind Speeds");   # changes the legend (fill) title
plot(thePlot);