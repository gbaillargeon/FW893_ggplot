source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );
#### Part 1: Create a Humidity vs. Temperature scatterplot
thePlot = ggplot(data=weatherData) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 2: Use dates instead of points
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, label=date)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 3: Reformat the dates
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, label=date),
            color="darkgreen", 
            size=2.5) +   # change size
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 4: Add gradient colors
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 5: Customize gradient colors
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","orange","red","green","blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 6: Customize legend
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","brown","red","green","blue"),
                        breaks=c(91, 183, 274),
                        labels=c("Mar-21", "Jun-21", "Sep-21")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(40, units="pt")) + # height of legend
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity",
       color = "Dates"); # give a better explanation! title of legend⤷
plot(thePlot);

#### Part 6: Customize legend
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","brown","red","green","blue"),
                        breaks=c(91, 183, 274),  # these are guesses
                        labels=c("Mar-21", "Jun-21", "Sep-21")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(40, units="pt")) +  # height of legend
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity",
       color = "Dates");            # give a better explanation! title of legend⤷
plot(thePlot);

#### Part 7: grep to find indexes
springIndex = grep(weatherData$date, pattern="3-21");
summerIndex = grep(weatherData$date, pattern="6-21");
fallIndex = grep(weatherData$date, pattern="9-21");
winterIndex = grep(weatherData$date, pattern="12-21");

#### Part 8: Add seasons to the legend
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","brown","red","green","blue"),
                        breaks=c(winterIndex, springIndex,
                                 summerIndex, fallIndex),
                        labels=c("winter","spring",
                                 "summer","fall")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(40, units="pt")) +  # height of legend
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity",
       color = "Dates");    # title of legend
plot(thePlot);