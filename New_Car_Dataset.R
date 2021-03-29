library(tidyverse)
cars <- read_csv("cars.csv")
car_data_edited <- select(car_data, -5) 
colSums(is.na(cars))
View(cars)
cars_edited <- select(cars, -8 & -12 & -(19:29))
View(cars_edited)

cars_edited <- cars_edited %>% mutate(location_region = recode(cars_edited$location_region,
             'Брестская обл.' = "Brest Region",
             'Витебская обл.' = "Vitebsk Region",
             'Гомельская обл.' = "Gomel Region",
             'Гродненская обл.' = "Grodno Region",
             'Минская обл.'  = "Minsk Region",
             'Могилевская обл.' = "Mogilev Region"
))

cars_edited <- cars_edited %>% mutate(manufacturer_name = recode(cars_edited$manufacturer_name,
                                                               'ВАЗ' = "AvtoVAZ",
                                                               'ГАЗ' = "GAZ",
                                                               'ЗАЗ' = "ZAZ",
                                                               'Москвич' = "Moskvitch",
                                                               'УАЗ'  = "UAZ"
))
View(cars_edited)

cars_edited <- cars_edited %>% mutate(model_name = recode(cars_edited$model_name,
                                                                 'Таврия' = "Tavria",
                                                                 '968м' = "968M",
                                                                 'Соболь' = "Sobol",
                                                                 'Луидор' = "Luidor",
                                                                 'ВИС'  = "VIS"
))
View(cars_edited)


colSums(is.na(cars_edited))
View(cars_edited %>% count(model_name))
View(cars_edited %>% count(manufacturer_name))
View(cars_edited %>% count(location_region))

summary(cars_edited)
ggplot(cars_edited,aes(location_region, manufacturer_name)) + geom_count()
ggplot(cars_edited,aes(year_produced, price_usd)) + geom_point()

ggplot(cars_edited, aes(price_usd)) + geom_boxplot()

cars_edited %>% group_by(manufacturer_name) %>% summarize(mean(price_usd)) %>% View()

ggplot(cars_edited,aes(color)) + geom_bar(aes(fill = location_region))                  
