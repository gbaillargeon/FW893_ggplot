source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

which(weatherData$weatherType == "RA") # find rainy days
#### Part 1: using grep to find days with a specific weather event
rainyDays = grep(weatherData$weatherType, pattern="RA");   # any day with rain
breezyDays = grep(weatherData$weatherType, pattern="BR");  # any breezy day

#### Part 2: Scatterplot for Humidity vs. Temperature on breezy days
plot1 = ggplot(data=weatherData[breezyDays,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Humidity vs. Temperature (Breezy Days)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(plot1);

rainyAndBreezy = intersect(rainyDays, breezyDays);
#### Part 3: Combine event using set operations
rainyAndBreezy = intersect(rainyDays, breezyDays); # days with rain AND wind
rainyOrBreezy = union(rainyDays, breezyDays);      # days with rain OR wind
rainyNotBreezy = setdiff(rainyDays, breezyDays);   # days with rain but NOT wind
breezyNotRainy = setdiff(breezyDays, rainyDays);   # days with wind but NOT rain

#### Part 4: Creating plots for all rainy day/breezy day combinations
plot2 = ggplot(data=weatherData[rainyDays,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Humidity vs. Temperature (rainy days)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot3 = ggplot(data=weatherData[rainyAndBreezy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Rainy AND Breezy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot4 = ggplot(data=weatherData[rainyOrBreezy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Rainy or Breezy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot5 = ggplot(data=weatherData[rainyNotBreezy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Rainy and NOT Breezy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot6 = ggplot(data=weatherData[breezyNotRainy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Breezy and NOT Rainy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

#### Part 5: Arranging plots on one canvas by rows 
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, 
             nrow=3);
#### Part 6: Arranging plots on canvas by columns
grid.arrange(plot6, plot5, plot4, plot3, plot2,
             ncol=3);
#### Part 7: Customize arrangements using matrix
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             layout_matrix = rbind(c(4,5,6),
                                   c(3,2,1)));
#### Part 8: Add empty spaces to customized arrangement
grid.arrange(plot3, plot4, plot5,
             layout_matrix = rbind(c(NA,1,2),
                                   c(3,NA,NA)));

#### Part 9: Extending plots across rows and columns
grid.arrange(plot1, plot2, plot3, plot4, 
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,NA),
                                   c(4,3,3),
                                   c(4,NA,NA)));



