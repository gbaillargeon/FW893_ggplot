#app 10 multipanel plots

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

tempDept=weatherData$tempDept

#isolate weather type conditions 

daysRain = grep(weatherData$weatherType, pattern="RA")
daysSnow = grep(weatherData$weatherType, pattern="SN")
daysFog = grep(weatherData$weatherType, pattern="FG")

#calcualate average TempDept per condition

avgRain = mean(weatherData[daysRain,]$tempDept)
avgSnow = mean(weatherData[daysSnow,]$tempDept)
avgFog = mean(weatherData[daysFog,]$tempDept)
#combo conditions 
rainandfog = intersect(daysRain, daysFog)
rainandnotfog = setdiff(daysRain, daysFog)

rainorsnow = union(daysRain, daysSnow)
avgRorS = mean(weatherData[rainorsnow,]$tempDept)
rainandsnow = intersect(daysRain, daysSnow)
avgRS = mean(weatherData[rainandsnow,]$tempDept)


#Histogram 1 - Rainy Days
hist1=ggplot( data=weatherData$daysRain) +
  geom_histogram( mapping=aes(x=tempDept, y=..count..),
                  bins=40,
                  color="black",
                  fill="gray")+
  theme_classic()+
  geom_vline(mapping=aes(xintercept=median(avgRain)),
             color="purple",
             size=1)+
  geom_text(x=5, y=8, label="Average tempDept=5.01", color="red")+
  labs(title = "TempDept for Rainy Days",
       subtitle = "Lansing, Michigan",
       x="Temperature Departure",
       y="Frequency");
plot(hist1);

#Histogram 2 - Snowy Days
hist2=ggplot( data=weatherData$daysSnow) +
  geom_histogram( mapping=aes(x=tempDept, y=..count..),
                  bins=40,
                  color="black",
                  fill="gray")+
  theme_classic()+
  geom_vline(mapping=aes(xintercept=median(avgSnow)),
             color="purple",
             size=1)+
  geom_text(x=5, y=8, label="Average tempDept=-4.1", color="red")+
  labs(title = "TempDept for Snowy Days",
       subtitle = "Lansing, Michigan",
       x="Temperature Departure",
       y="Frequency");
plot(hist2);

#Histogram 3 - Foggy Days
hist3=ggplot( data=weatherData$daysFog) +
  geom_histogram( mapping=aes(x=tempDept, y=..count..),
                  bins=40,
                  color="black",
                  fill="gray")+
  theme_classic()+
  geom_vline(mapping=aes(xintercept=median(avgFog)),
             color="purple",
             size=1)+
  geom_text(x=5, y=8, label="Average tempDept=2.58", color="red")+
  labs(title = "TempDept for Foggy Days",
       subtitle = "Lansing, Michigan",
       x="Temperature Departure",
       y="Frequency");
plot(hist3);

#Histogram of TempDept and RAIN+SNOW
hist4=ggplot( data=weatherData$rainandsnow) +
  geom_histogram( mapping=aes(x=tempDept, y=..count..),
                  bins=40,
                  color="black",
                  fill="gray")+
  theme_classic()+
  geom_vline(mapping=aes(xintercept=median(avgRS)),
             color="purple",
             size=1)+
  geom_text(x=5, y=8, label="Average tempDept=0.47", color="red")+
  labs(title = "TempDept for Rainy and Snowy Days",
       subtitle = "Lansing, Michigan",
       x="Temperature Departure",
       y="Frequency");
plot(hist4);

hist5=ggplot( data=weatherData$rainorsnow) +
  geom_histogram( mapping=aes(x=tempDept, y=..count..),
                  bins=40,
                  color="black",
                  fill="gray")+
  theme_classic()+
  geom_vline(mapping=aes(xintercept=median(avgRorS)),
             color="purple",
             size=1)+
  geom_text(x=5, y=8, label="Average tempDept=1.95", color="red")+
  labs(title = "TempDept for Rainy or Snowy Days",
       subtitle = "Lansing, Michigan",
       x="Temperature Departure",
       y="Frequency");
plot(hist5);

#put all plots on one canvas
grid.arrange(hist1, hist2, hist4, 
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,NA),
                                   c(4,NA,NA)));