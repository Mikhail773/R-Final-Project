library(tidyverse)
# Read cars.csv
cars <- read_csv("Car/Data 2/cars.csv")
#car_data_edited <- select(car_data, -5) 
colSums(is.na(cars))
View(cars)
cars_edited <- select(cars, -12 & -20:-29)
View(cars_edited)
colSums(is.na(cars_edited))
