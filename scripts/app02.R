rm(list=ls());                         # clear Console Window
options(show.error.locations = TRUE);  # show line numbers on error
library(package=ggplot2);              # include all GGPlot2 functions

# execute the lines of code from reference.r
source(file="scripts/reference.r");    

# read in CSV file and save the content to packageData
packageData = read.csv(file="data/accdeaths.csv");

#plot the data
plotData = ggplot( data=packageData ) + 
  geom_point( mapping=aes(x=time, y=value) ) +
  ggtitle( label="Accidental Deaths from 1973-1978" ) +
  ylab(label = "Accidental Deaths")  +
  xlab(label = "Year") +
  scale_x_continuous( breaks = seq(from=0, to=1979, by=.5) ) +
  scale_y_continuous( breaks = seq(from=7000, to=110000, by=2000) ) +
  theme( axis.text.x=element_text(angle=45, hjust=1) );
plot(plotData);

