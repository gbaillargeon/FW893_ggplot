#loading dataframe 
{
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

theDate = weatherData[ , "dateYr"]; #create dateYr column

#Indexing the bimonth using which() - faster than using for loop and if statements
JanFeb = which(theDate < as.Date("03-01-2016", format="%m-%d-%Y")); 
MarApr = which(theDate >= as.Date("03-01-2016", format="%m-%d-%Y") &
                 theDate < as.Date("05-01-2016", format="%m-%d-%Y"));
MayJun =  which(theDate >= as.Date("05-01-2016", format="%m-%d-%Y") &
                  theDate < as.Date("07-01-2016", format="%m-%d-%Y"));
JulAug =  which(theDate >= as.Date("07-01-2016", format="%m-%d-%Y") &
                  theDate < as.Date("09-01-2016", format="%m-%d-%Y"));
SepOct =  which(theDate >= as.Date("09-01-2016", format="%m-%d-%Y") &
                  theDate < as.Date("11-01-2016", format="%m-%d-%Y"));
NovDec = which(theDate >= as.Date("11-01-2016", format="%m-%d-%Y"));

#create bimonth column name
weatherData[, "biMonth"] = "" 

#fill bimonth column with values
weatherData[JanFeb, "biMonth"] = "JanFeb";
weatherData[MarApr, "biMonth"] = "MarApr";
weatherData[MayJun, "biMonth"] = "MayJun";
weatherData[JulAug, "biMonth"] = "JulAug";
weatherData[SepOct, "biMonth"] = "SepOct";
weatherData[NovDec, "biMonth"] = "NovDec";

#Create histogram of relative humidity
plotData = ggplot( data=weatherData ) + 
  geom_histogram(mapping=aes(x=relHum, y=..count..),
                 binwidth=4,
                 color="grey50",
                 fill="lightblue") +
  theme_classic() +
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity (%)",
       y = "Count");
plot(plotData);

#Create Histogram for bimonthly values
plotData = ggplot(data=weatherData) +
  geom_histogram(mapping=aes(x=relHum, y=..count..),
                 bins=40,
                 color="grey50",
                 fill="lightblue") +
  theme_classic() +
  facet_grid( facet= biMonth ~ .) +
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity (%)",
       y = "Count");
plot(plotData);

#Create Stacked Histogram
plotData = ggplot(data=weatherData) + 
  geom_histogram(mapping=aes(x=relHum, y=..count.., fill=biMonth),
                 bins=40,
                 color="grey50",
                 position="stack") +
  theme_classic() +
  geom_vline(mapping=aes(xintercept=mean(weatherData[JulAug, "relHum"])),
             color="darkblue",
             size=1.2) +
  geom_vline(mapping=aes(xintercept=mean(weatherData[JanFeb, "relHum"])),
             color="black",
             size=1.2) +
  theme(legend.position = c(x=0.13, y=0.65))+
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity (%)",
       y = "Count");
plot(plotData);
}

