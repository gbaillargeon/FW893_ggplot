{
rm(list=ls());
options(show.error.locations = TRUE);

source(file="scripts/reference.R");
weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                       stringsAsFactors = FALSE);

#### Part 1: Add year to date values ####
# a) save the date vector from the data frame to the variable theDate
theDate = weatherData[["date"]]; 
# append (paste) "-2016" to all values in theDate
theDate = paste(theDate, "-2016", sep="");
# c) Save the values in Date format
theDate = as.Date(theDate, format="%m-%d-%Y");
# d) Save theDate back to the data frame as a new column
weatherData[["dateYr"]] = theDate; 

### Part 2: Changing temperature from F to C
#max Temp
maxTemp = weatherData[["maxTemp"]]
maxTemp_C = (5/9)*(maxTemp - 32)
weatherData[["maxTemp_C"]] = maxTemp_C
#min Temp
minTemp = weatherData[["minTemp"]]
minTemp_C = (5/9)*(minTemp - 32)
weatherData[["minTemp_C"]] = minTemp_C
#avgTemp
avgTemp = weatherData[["avgTemp"]]
avgTemp_C = (5/9)*(avgTemp - 32)
weatherData[["avgTemp_C"]] = avgTemp_C

###Part 3: Plotting temperature vs date as line plot
plotData = ggplot(data=weatherData) + 
  geom_line(mapping=aes(x=dateYr, y=maxTemp_C), #first line plot
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp_C), #second line plot
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp_C), #smooting function
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (C)") +
  
#Formatting the Plot
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1),
        axis.text.x = element_text(color="black", family="mono", size=9),
        axis.title.x = element_text(color="blue", family="mono", size=9),
        axis.title.y = element_text(color="red", family="mono", size=9))+
  scale_y_continuous(limits = c(-10,35),
                     breaks = seq(from=-10, to=35, by=15)) +
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-09-21")),
               date_breaks = "8 weeks",
               date_labels = format("%b-%d-%Y"));
plot(plotData);
}