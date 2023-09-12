## ##BikeShare EDA code
## 
##Libraries 
library(tidyverse) 
library(vroom) 
library(ggplot2)
bike <- vroom("/Users/cicizeng/Desktop/STA348/KaggleBikeShare/train.csv")

ggplot(data=bike, aes(x=humidity, y=count)) +
geom_point() +
geom_smooth(se=FALSE)


# numbers of total rentals changes in different seasons
#season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
ggplot(bike, aes(x = factor(season), y = count, fill = factor(season))) +
  geom_boxplot() +
  labs(title = "Distribution of Rentals by Season", x = "Season", y = "Count") +
  scale_x_discrete(labels = c("Spring", "Summer", "Fall", "Winter")) +
  theme_minimal()


###The casual rental numbers change with different weathers
ggplot(bike, aes(x = factor(weather), y = casual, fill = factor(weather))) +
  geom_bar(stat = "identity") +
  labs(title = "Casual Rentals by Weather Condition", x = "Weather Condition", y = "Casual Count") +
  scale_x_discrete(labels = c("Clear/Few clouds/Partly cloudy", "Mist + Cloudy", "Light Snow/Light Rain", "Heavy Rain + Thunderstorm/Snow + Fog")) +
  theme_minimal()

###
ggplot()+  
  geom_smooth(data=bike, aes(x=datetime, y=temp), color="blue")+  
  geom_smooth(data=bike, aes(x=datetime, y =atemp ), color = "red")+  
  labs(title = "Tempture(blue) vs feels like tempture(red)",x = "Date and Time", y = "Temperature" )


