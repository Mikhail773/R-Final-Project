###################################################################################################
#
# CSC 335 Project: Used Cars Market in Belarus
#

#Import all the library's we are using
library(tidyverse)


###################################################################################################
#
# Evaluate the data
#

#Read our dataset into the cars var
cars <- read_csv("cars.csv")

#check the na's in the dataset
colSums(is.na(cars))
View(cars) #view the data


###################################################################################################
#
# Edit(modify) the data
#

#Removing unnecessary columns from cars. Store that data in cars_edited
cars_edited <- select(cars, -8 & -(12:13) & -(19:29))
View(cars_edited) #view the data

#Recode foreign language into their English meaning (location_region)
cars_edited <- cars_edited %>% mutate(location_region = recode(cars_edited$location_region,
             'Брестская обл.' = "Brest Region",
             'Витебская обл.' = "Vitebsk Region",
             'Гомельская обл.' = "Gomel Region",
             'Гродненская обл.' = "Grodno Region",
             'Минская обл.'  = "Minsk Region",
             'Могилевская обл.' = "Mogilev Region"
))

#Recode foreign language into their English meaning (manufacturer_name)
cars_edited <- cars_edited %>% mutate(manufacturer_name = recode(cars_edited$manufacturer_name,
             'ВАЗ' = "AvtoVAZ",
             'ГАЗ' = "GAZ",
             'ЗАЗ' = "ZAZ",
             'Москвич' = "Moskvitch",
             'УАЗ'  = "UAZ"
))

#Recode foreign language into their English meaning (model_name)
cars_edited <- cars_edited %>% mutate(model_name = recode(cars_edited$model_name,
             'Таврия' = "Tavria",
             '968м' = "968M",
             'Соболь' = "Sobol",
             'Луидор' = "Luidor",
             'ВИС'  = "VIS"
))

#Recode foreign language into their English meaning (engine_fuel)
cars_edited <- cars_edited %>% mutate(engine_fuel = recode(cars_edited$engine_fuel,

             'gas' = "gasoline",

))

#view the changes the mutate made
View(cars_edited) 

#Check if there are na's in the cars_edited
colSums(is.na(cars_edited)) 

#Getting the unique entries and displaying how often they appear
View(cars_edited %>% count(model_name)) 
View(cars_edited %>% count(manufacturer_name))
View(cars_edited %>% count(location_region))
View(cars_edited %>% count(year_produced))


###################################################################################################
#
# Plotting the data
#

summary(cars_edited)
ggplot(cars_edited,aes(location_region, manufacturer_name)) + geom_count()

ggplot(cars_edited,aes(year_produced, price_usd)) + geom_point() + geom_smooth()

ggplot(cars_edited, aes(price_usd, ..density..)) + geom_freqpoly(binwidth = 500)

cars_edited %>% group_by(manufacturer_name) %>% summarize(mean(price_usd)) %>% View()

ggplot(cars_edited,aes(color)) + geom_bar(aes(fill = location_region))

ggplot(cars_edited,aes(odometer_value, price_usd)) + geom_point(aes(color = is_exchangeable)) + geom_smooth()

ggplot(cars_edited,aes(year_produced, price_usd)) + geom_point(aes(color = body_type)) + geom_smooth()

ggplot(cars_edited) + geom_point(mapping = aes(x = odometer_value, y = price_usd, color = engine_fuel))

group_by(cars_edited, body_type) %>% summarise(price_mean = mean(price_usd)) -> mean_cars

ggplot(cars_edited) + geom_boxplot(mapping = aes(x=reorder(body_type,price_usd), y=price_usd))

ggplot(cars_edited) + geom_point(mapping = aes(x = body_type, y = price_usd, color = engine_fuel))
