{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE);
  
  #### Create a column the give whether there was precipitation
  
  #### grepl produces a Boolean vector the same length as the data: 
  #      values are TRUE if they contain RA or SN, FALSE otherwise
  daysWithPrecip = grepl(x=weatherData$weatherType, pattern="RA|SN");
  
  # days will take values from 1 to 366 (the number of rows)
  for(day in 1:nrow(weatherData))  
  {
    if(daysWithPrecip[day] == TRUE)  # day had either RA or SN
    {
      weatherData$precipitation[day] = 1;   # set precip to 1
    }
    else   # day had neither RA nor SN
    {
      weatherData$precipitation[day] = 0;   # set precip to 0
    }
  }
  
  # GGPlot cannot factor a numeric column -- need to convert column to string (characters)
  weatherData$precipitation = as.character(weatherData$precipitation);
  
  # Have GGPlot make boxes 
  plot1 = ggplot(data=weatherData) +   
    geom_boxplot(mapping=aes(x=precipitation, y=relHum)) +
    theme_bw() +
    scale_x_discrete(labels=c("No Precip", "Precip")) +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Precipitation",
         y = "Humidity");
  plot(plot1);
  
  # Get data for boxplot
  y = weatherData$relHum[weatherData$precipitation==0]; 
  y2 = weatherData$relHum[weatherData$precipitation==1];
  
  yMed = median(y);
  y2Med = median(y2);
  
  yMean = mean(y);
  y2Mean = mean(y2);
  
  ySD = sd(y);
  y2SD = sd(y2);
  
  #### find the 1st and 3rd quartiles (0.25 and 0.75 quantiles) of the vector
  y25 = quantile(y, probs = 0.25);
  y75 = quantile(y, probs = 0.75);
  
  y2_25= quantile(y, probs = .25);
  y2_75 = quantile(y, probs=.75);
  
  # interquartile range 
  IQR = y75 - y25;
  IQR2 = y2_75 - y2_25;
  
  # Find the low and high point of the whiskers 
  whiskerHighTemp = y75 + 1.5*IQR;   
  whiskerLowTemp = y25 - 1.5*IQR;
  
  whisker2HighTemp = y75 + 1.5*IQR2;  
  whisker2LowTemp = y25 - 1.5*IQR2;
  .  
  yHigh = y[y > y75 & y <= whiskerHighTemp];
  whiskerHigh = max(yHigh);                  
  yLow = y[y >= whiskerLowTemp & y < y25];   
  whiskerLow = min(yLow);                    
  
  y2High = y2[y2 > y2_75 & y2 <= whisker2HighTemp];
  whisker2High = max(y2High);                  
  y2Low = y2[y2 >= whisker2LowTemp & y2 < y2_25];   
  whisker2Low = min(y2Low); 
  
  #plotting humidity and precipitation
  plot2 = ggplot() +
    theme_bw() +
    
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Precipitation",
         y = "Humidity") +
    
    scale_x_continuous(breaks = c(1,2),
                       labels = c("Precip", "No Precip")) +
    
    # box (1st quarter to 3rd quarter)
    annotate(geom="rect",
             xmin = 0.7, xmax = 1.3, 
             ymin = y25, ymax = y75,
             fill = "transparent",
             color = "black") +
    annotate(geom="rect",
             xmin = 0.7, xmax = 1.3, 
             ymin = y2_25, ymax = y2_75,
             fill = "transparent",
             color = "black") +
    
    # median line
    annotate(geom="segment",
             x = 0.7, xend=1.3,
             y = yMed, yend=yMed) +
    annotate(geom="segment",
             x = 0.7, xend=1.3,
             y = y2Med, yend=y2Med) +
    # high whisker
    annotate(geom="segment",
             x = 1, xend=1,
             y = y75, yend=whiskerHigh) +  
    annotate(geom="segment",
             x = 2, xend=3,
             y = y2_75, yend=whisker2High) +
    
    #low whisker
    annotate(geom="segment",
             x = 1, xend=1,
             y = y25, yend=whiskerLow) + 
    annotate(geom="segment",
             x = 2, xend=1,
             y = y2_25, yend=whisker2Low) + 
    
    # mean line
    annotate(geom="segment",
             x=0.8, xend=1.2,       
             y=yMean, yend=yMean,   
             color = "blue") +
    annotate(geom="segment",
             x=0.8, xend=1.2,       
             y=y2Mean, yend=y2Mean,   
             color = "blue") +
    
    # SD
    annotate(geom="text",
             x = 1,
             y = c(yMean+ySD, yMean-ySD),
             label = "ySD",
             color = "blue")+
  annotate(geom="text",
           x = 2,
           y = c(y2Mean+y2SD, y2Mean-yS2D),
           label = "y2SD",
           color = "blue");
  
  plot(plot2);
}  