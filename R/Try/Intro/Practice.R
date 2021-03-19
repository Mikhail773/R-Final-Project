library(nycflights13)
View(flights)
library(tidyverse)
library(magrittr)
library(ggplot2)
flights_mine <- mutate(flights, arr_delay + dep_delay)
View(flights_mine)

TacoMuncher007 <- tibble(flights)

write_csv(TacoMuncher007, "TacoMuncher007.csv")
read_csv("TacoMuncher007.csv")

AA <- flights %>% filter(carrier == "AA")
View(AA)

ggplot(data = flights_mine) + geom_point(mapping = aes(x = carrier, y = distance, color=carrier)) + ggtitle("Carrier vs Distance") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data = flights_mine) + geom_point(mapping = aes(x = origin, y = dest, color=carrier)) + ggtitle("Origin vs Destination") + theme(plot.title = element_text(hjust = 0.5))
ls()                                         
summary(flights_mine)
names(flights_mine)
unique(flights_mine$origin)
unique(flights_mine$dest)
unique(flights_mine$year)
