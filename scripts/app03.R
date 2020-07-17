source(file="scripts/reference.R");   # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE);

#### Part 1: Plot average wind speed vs daily temperature departure ####
plotData = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=windSpeed, y=abs(tempDept)),
              color=rgb(red = 0, green = 0.6, blue = 0.6),
              size=2,
              shape=18,
              alpha = 0.7) +
  theme_bw() +
  labs(title = "Average Wind Speed vs. Daily Temperature Departure ",
       subtitle = "Lansing, Michigan (2016)",
       x = "Average Wind Speed (mph)",
       y = "Daily Temperature Departure (F)") +
  theme(axis.title.x=element_text(size=14,color="black", face="italic"),
        axis.title.y=element_text(size=14, angle=90, color="orangered4"), 
        plot.title=element_text(size=18, face="bold", 
                                color ="darkblue"),
        plot.subtitle=element_text(size=12, face="bold.italic", 
                                   color ="darkblue", family="serif"))+
  geom_smooth( mapping=aes(x=windSpeed, y=abs(tempDept)), 
               method="lm",
               color="black", 
               size=.8, 
               linetype=1, 
               fill="darkgray",
               level = .99);
plot(plotData);


ggsave(
  filename = "Images/appo3.jpeg",
  plot = plotData,
  scale = 1,
  width = 12,
  height = 7,
  units = "cm",
  dpi = 300);

ggsave(
  filename = "Images/png images/appo3.png",
  plot = plotData,
  scale = 1,
  width = 6,
  height = 8,
  units = "in",
  dpi = 300);
