library(tidyverse)
cars <- read_csv("cars.csv")
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

cars_edited <- cars_edited %>% mutate(model_name = recode(cars_edited$model_name,
             'Таврия' = "Tavria",
             '968м' = "968M",
             'Соболь' = "Sobol",
             'Луидор' = "Luidor",
             'ВИС'  = "VIS"
))

cars_edited <- cars_edited %>% mutate(engine_fuel = recode(cars_edited$engine_fuel,

             'gas' = "gasoline",

))

View(cars_edited)

colSums(is.na(cars_edited))
View(cars_edited %>% count(model_name))
View(cars_edited %>% count(manufacturer_name))
View(cars_edited %>% count(location_region))
View(cars_edited %>% count(year_produced))

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
