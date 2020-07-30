source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

wType = weatherData$weatherType
wType = substr(wType, start = 1, stop = 2);
dates = as.Date(weatherData$dateYr); 
months = format(dates, format="%b");
weatherData$month = months;
#cool days bar plot
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays, fill=wType), 
           width=0.6) +
  scale_fill_manual(values = c("green", "purple", "lightblue", 
                               "pink", "yellow","orange", "brown", "cyan"))+
  theme_bw() +
  labs(title = "Cool Days During the Year",
       subtitle = "Lansing, Michigan: 2016",
       x = "Months",
       y = "Cool Days",
       fill = "Weather Type");
plot(thePlot);

#heat days bar plot
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=heatDays, fill=wType), 
           width=0.6) +
  scale_fill_manual(values = c("green", "purple", "lightblue", 
                               "pink", "yellow","orange", "brown", "cyan"))+
  scale_x_discrete(limits = month.abb) +
  geom_hline(mapping = aes( yintercept= sum(coolDays)),
             color="black",
             size=1.5,
             linetype=2) +
  geom_text(x=6, y=900, label="Total Cool Days", color="black")
  theme_bw() +
  labs(title = "Heat Days During the Year",
       subtitle = "Lansing, Michigan: 2016",
       x = "Months",
       y = "Heat Days",
       fill = "Weather Type");
plot(thePlot);
  
#combine heat and cool days in one bar plot
  thePlot = ggplot(data=weatherData) +
    geom_col(mapping=aes(x=month, y=coolDays), 
             width=0.4, fill = "red") +
    geom_col(mapping=aes(x=month, y=heatDays), 
             width=0.4, fill="blue") +
    theme_bw() +
    labs(title = "Heating and Cooling Days",
         subtitle = "Lansing, Michigan: 2016",
         x = "Months",
         y = "All Heat and Cool Days");
  plot(thePlot);