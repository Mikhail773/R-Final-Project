###################################################################################################
#
# CSC 335 Project: Used Cars Market in Belarus
#

#Import all the library's we are using
library(tidyverse)
library(e1071) #SVM
library(car) #predict
library(caret) #partiiton
library(MASS) #stepwise
library(kernlab) #SVM
library(rpart) # Decision Tree Regression
library(randomForest) #  Random Forest Tree Regression
library(ranger) # RFT more than 53 factors
###################################################################################################
#
# Evaluate the data
#

#Read our dataset into the cars variable
cars <- read_csv("cars.csv")

View(cars) #view the data

###################################################################################################
#
# Edit(modify) the data: Recoding was done to the file and the CSV file was saved that way. 
# This prevented issues arising from locale settings and made working on the code easier as a group.
#

#Removing unnecessary columns from cars. Store that data in cars_edited
# -(19) is a column that shows the number of times a car has been upped. This column not descriptive and has been removed
# -(20:29) are boolean columns for various features. There is no description of what these features are and for that reason they have been omitted.
cars_edited <- cars %>% dplyr::select(-8 & -(12:13) & -(20:29))
View(cars_edited) #view the data

# This recoding was done on the csv file and saved permanently to avoid future locale issues.
# #Recode foreign language into their English meaning (location_region)
# cars_edited <-
#   cars_edited %>% mutate(
#     location_region = recode(
#       cars_edited$location_region,
#       'Брестская обл.' = "Brest Region",
#       'Витебская обл.' = "Vitebsk Region",
#       'Гомельская обл.' = "Gomel Region",
#       'Гродненская обл.' = "Grodno Region",
#       'Минская обл.'  = "Minsk Region",
#       'Могилевская обл.' = "Mogilev Region"
#     )
#   )
#
# Recode foreign language into their English meaning (manufacturer_name)
# cars_edited <-
#   cars_edited %>%   mutate(
#     manufacturer_name = recode(
#       cars_edited$manufacturer_name,
#       'ВАЗ' = "AvtoVAZ",
#       'ГАЗ' = "GAZ",
#       'ЗАЗ' = "ZAZ",
#       'Москвич' = "Moskvitch",
#       'УАЗ'  = "UAZ"
#     )
#   )
#
# Recode foreign language into their English meaning (model_name)
# cars_edited <-
#   cars_edited %>% mutate(
#     model_name = recode(
#       cars_edited$model_name,
#       'Таврия' = "Tavria",
#       '968м' = "968M",
#       'Соболь' = "Sobol",
#       'Луидор' = "Luidor",
#       'ВИС'  = "VIS"
#     )
#   )

# Recode engine_fuel to clear up confusion between compressed gas and gasoline
cars_edited <- cars_edited %>% mutate(engine_fuel = dplyr::recode(engine_fuel,'gas' = "compressed natural gas"))

# Check the na's in the dataset
colSums(is.na(cars))
# NA is in the categorical attribute engine-capacity

# Lets arbitrarily pick -1 to denote NA. (Engine-Capacity is categorical so this can be done)
cars_edited <- cars_edited %>% mutate(engine_capacity = coalesce(engine_capacity, -1))

# Check for Duplicates and remove them
which(duplicated(cars_edited))
cars_edited <- cars_edited %>% distinct()

# View the changes the mutate made
View(cars_edited)

###################################################################################################
#
# Split Dataset into Training and Testing for our Models
#
set.seed(123)
training.samples <- cars_edited$manufacturer_name %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- cars_edited[training.samples,]
test.data <- cars_edited[-training.samples,]

###################################################################################################
#
# Investigating Variables: Viewing the data with our modifications
#
# Bar Graphs:
#
# 1) What is the distribution of manufacturers?
# 4) Plotting cars by color and quantity
# 10) Bar graph Body type: count how many cars have the same body type
# 13) Bar graph Is exchangeable: Counting the number of cars that are exchangeable
#
# BOX PLOTS:
# 16) Box plot Number of photos: Graph to see how the data is skewed
# 18) Box plot Duration listed: investigating how our outliers look with our modifications
#
# Histogram PLOTS:
#
# 5) Histogram Odometer Value: Graph to see how the data is skewed
# 6) Histogram Year produced: Graph to see how the data is skewed
# 12) Number of cars with same price
# 15) Histogram Number of photos: Graph to see how the data is skewed
# 17) Histogram Up counter: investigating how our outliers look with our modifications
# 19) Histogram Duration listed: Graph to see how the data is skewed
#
# Pie Graph:
# 3) Plotting the number of cars with automatic or mechanical transmissions
# 7) Graph to show what fuel distribution
# 8) Pie graph to show engine type Distribution (Electric, Diesel, Gasoline)
# 11) Graph Drivetrain distribution:
# 14) Pie Graph Location region: Count the number of cars in a region
#
# TABLE:
#
# 2) A table to show unique car model names and quantity
# 9) Table for Engine capacity

# 1)What is the distribution of manufacturers?
ggplot(cars_edited, aes(y = manufacturer_name)) + geom_bar(aes(fill = manufacturer_name)) + geom_text(stat='count', aes(label=..count..), hjust=1)
# We can see a large difference in the amount cars for each manufacturers. Volkswagen, Opel, BMW, Audio, AvtoVAZ, Ford, Renault, and Mercedes-Benz are the majot manufacturers.

# 2) A table to show unique car model names and quantity
View(cars_edited %>% count(model_name))

# 3) Plotting the number of cars with automatic or mechanical transmissions
transmissionGrouped <- group_by(cars_edited, transmission)
transmissionCounted <- count(transmissionGrouped)
percentTransmission <- paste0(round(100*transmissionCounted$n/sum(transmissionCounted$n), 2), "%")
pie(transmissionCounted$n, labels = percentTransmission, main = "Transmission Distribution", col = rainbow(nrow(transmissionCounted)))
legend("right", c("Automatic", "Mechanical"), cex = 0.8,
       fill = rainbow(length(transmissionCounted)))
# Mechanical is significantly more common than Automatic. This will definitely be an attribute to consider in our final model

# 4) Plotting cars by color and quantity
ggplot(cars_edited, aes(x = color)) + geom_bar(stat = "count", aes(fill = color)) + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
# There is a lot of diversity in the colors and once again although there are categories with more values there is still a decent amount of variation

# 5) Histogram Odometer Value: Graph to see how the data is skewed
ggplot(cars_edited, aes(odometer_value)) + geom_histogram()
# The data is left-skewed with what appears to be outliers as around 1,000,000 miles. 

# 6) Histogram Year produced: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(aes(year_produced))
# The graph seems to almost be normally distributed minus what appears to be some outliers on the the older end of the years

# 7) Graph to show what fuel distribution
ggplot(cars_edited, aes(x = engine_fuel)) + geom_bar(stat = "count", aes(fill = engine_fuel)) + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
# There are only 2 major engine fuels(gasoline and diesel)

# 8) Pie graph to show engine type Distribution (Electric, Diesel, Gasoline)
TypeGrouped <- group_by(cars_edited, engine_type)
TypeCounted <- count(TypeGrouped) 
percentType <- paste0(round(100*TypeCounted$n/sum(TypeCounted$n), 2), "%")
pie(TypeCounted$n, labels = percentType, main = "Engine Type Distribution", col = rainbow(nrow(TypeCounted)))
legend("right", c("diesel", "electric", "gasoline"), cex = 0.8,
       fill = rainbow(nrow(TypeCounted)))
# Not surprisingly gasoline and diesel are the 2 most common Engine Type considering the fuel distribution

# 9) Table for Engine capacity
View(cars_edited %>% count(engine_capacity))
# Engine Capacity seems to be left-skewed which may indicate outliers

# 10) Bar graph Body type: count how many cars have the same body type
ggplot(cars_edited, aes(x = body_type), stat = "count") + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
# There is some diversity in body type and the diversity in categories may lend itself to useful data for a future model

# 11) Graph Drivetrain distribution:
drivetrainGrouped <- group_by(cars_edited, drivetrain)
drivetrainCounted <- count(drivetrainGrouped) 
percentdrivetrain <- paste0(round(100*drivetrainCounted$n/sum(drivetrainCounted$n), 2), "%")
pie(drivetrainCounted$n, labels = percentdrivetrain, main = "Drivetrain Distribution", col = rainbow(nrow(drivetrainCounted)))
legend("right", c("all", "front", "rear"), cex = 0.8,
       fill = rainbow(nrow(drivetrainCounted)))
# Although most vehicles are front wheel drive there is enough all and real wheel drive to gather some promising insights 

# 12) Number of cars with same price
ggplot(cars_edited, aes(x = price_usd), stat = "count") + geom_histogram()
# This graph is extremely left-skewed. The sheer usefulness of price in our dataset will make this our main response attribute.

# 13) Pie Graph exchangeability Distribution
exchangeableGrouped <- group_by(cars_edited, is_exchangeable)
exchangeableCounted <- count(exchangeableGrouped) 
percentexchangeable <- paste0(round(100*exchangeableCounted$n/sum(exchangeableCounted$n), 2), "%")
pie(exchangeableCounted$n, labels = percentexchangeable, main = "Exchangeability Distribution", col = rainbow(nrow(exchangeableCounted)))
legend("right", c("False", "True"), cex = 0.8,
       fill = rainbow(nrow(exchangeableCounted)))
# Exchangeability is more common than anticipated. It will be interesting to see if pricier or cheaper cars consent to exchanges

# 14) Pie Graph Location region: Count the number of cars in a region
regionPriceDF <- group_by(cars_edited, location_region)
regionPriceDFCount <- count(regionPriceDF)
percentRegion <- paste0(round(100*regionPriceDFCount$n/sum(regionPriceDFCount$n), 2), "%")
pie(regionPriceDFCount$n, labels = percentRegion, main = "Region Price Distribution", col = rainbow(nrow(regionPriceDFCount)))
legend("right", c("Brest Region", "Gomel Region", "Grodno Region", "Minsk Region", "Mogilev Region", "Vitebsk Region"), cex = 0.8,
       fill = rainbow(nrow(regionPriceDFCount)))
# Minsk accounts for a very large amount of vehicles(makes sense considering the population sizes) with even distributions everywhere else. 
# The usefulness of the attribute may be less since Minsk is such a large portion of the data.

# 15) Histogram Number of photos: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(mapping = aes(number_of_photos))
# Graph is very left-skewed. I suspect photos may increase the value of a vehicle, but tests will need to be done on this

# 16) Box plot Number of photos: Graph to see how the data is skewed
ggplot(cars_edited) + geom_boxplot(mapping = aes(number_of_photos))
# There are many outliers. With extra time we may be able to investigate the impact of these outliers on the data.

# 17) Histogram Up counter: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_histogram(mapping = aes(up_counter))
# Clearly an outliers exists since there is a large scale. 

# 18) Box plot Duration listed: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(duration_listed))
# There is a significant amount of outliers. There is no evidence to conclude these should be eliminated.

# 19) Histogram Duration listed: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(aes(duration_listed))
# There are many new cars that are listed in this catalog.
###################################################################################################
#
# Plotting Graphs to investigate relationships
#
# Scatter Plot:
# 2) Price of a car according to its year produced
# 4) Graph to show the price of a car according to it's millage(odometer)
# 5) Graph to show the price of a car according to it's year produced AND body type
# 9) Price of a car according to it's number of photos incl. engine fuel
#
# Balloon Plot:
# 1) The amount of cars(by manufacturer name) in a region
#
# Bar Graph:
# 3) Show the number of cars in specific colors(10 red cars, 8 blue cars etc.) by region
#
# Box Plot:
# 7) Show the outliers with body type and price
#
# Misc:
# 6) Group by car body type and get it's mean price
# 8) Show the correlation between car body type, price, AND engine fuel
# 10) Group cars by manufacturer, and get it's mean price

# 1) Graph to show the amount of cars(by manufacturer name) in a region BALLOON PLOT
ggplot(cars_edited, aes(location_region, manufacturer_name)) + geom_count()
# Due to the quantity of categories a test will need to be done to gather significant data

# 2) Graph to show the price of a car according to its year produced SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point() + geom_smooth()
# There exists a parabolic relationship between year_produced and price_usd

# 3) Graph to show the number of cars in specific colors(10 red cars, 8 blue cars etc.) by region BAR GRAPH
ggplot(cars_edited, aes(color)) + geom_bar(aes(fill = location_region))
# From looking at the bar graph there does not seem to be any significant differences in color distribution for locations

# 4) Graph to show the price of a car according to it's millage(odometer) SCATTER PLOT
ggplot(cars_edited, aes(odometer_value, price_usd)) + geom_point(aes(color = is_exchangeable)) + geom_smooth()
# This graph is incredible diverse and indicates that there is a need for advanced models to access price relationships.

# 5)Graph to show the price of a car according to it's year produced AND body type SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point(aes(color = body_type)) + geom_smooth()
# There seems to be a parabolic relationship between year_produced and price_usd

# 6) Group by car body type and get it's mean price
group_by(cars_edited, body_type) %>% summarise(price_mean = mean(price_usd))

# 7) Graph to show the outliers with body type and price BOX PLOT
ggplot(cars_edited) + geom_boxplot(mapping = aes(x = reorder(body_type, price_usd), y =
                                                   price_usd))
# 8) Graph to show the correlation between car body type, price, AND engine fuel
ggplot(cars_edited) + geom_point(mapping = aes(x = body_type, y = price_usd, color = engine_fuel))

# 9) Graph to show the price of a car according to it's number of photos incl. engine fuel SCATTER PLOT
ggplot(cars_edited) + geom_point(mapping = aes(x = number_of_photos, y = price_usd, color = engine_fuel))

# 10) Group cars by manufacturer, and get it's mean price
cars_edited %>% group_by(manufacturer_name) %>% summarize(mean(price_usd)) %>% View()

###################################################################################################
#
# Answering Questions
#
###################################################################################################
#
# (Emma Doyle)
#
# 1) What impact does region have on price?
# Region does impact price. However, the extent of this impact would have to be assessed in a model that includes more attributes.
# The best indicator of the relationship will be seen in the total model.
# Regions: Minsk, Gomel, Brest, Vitebsk, Mogilev, Grodno
#

#Aggregating the data of price to region to get the mean of prices
#per region
regionPriceDF <- group_by(cars_edited, location_region)
regionPriceDF_averages <- summarise(regionPriceDF, average_price_usd = mean(price_usd))
View(regionPriceDF_averages)
Average_price_usd <- regionPriceDF_averages$average_price_usd
ggplot(regionPriceDF_averages,aes(x= location_region, y = Average_price_usd)) + geom_bar(stat = "identity") 

#one-way anova
group_by(regionPriceDF, regionPriceDF$location_region) %>%
  summarise(
    count = n(),
    mean = mean(price_usd, na.rm = TRUE),
    sd = sd(price_usd, na.rm = TRUE)
  )
# Compute the analysis of variance
res.aov <- aov(regionPriceDF$price_usd ~ regionPriceDF$location_region,
               data = regionPriceDF)
# Summary of the analysis
summary(res.aov)

###################################################################################################
#
# (Emma Doyle)
# 
# 2) Do manufacturers have a significant impact on the asking price of a vehicle?
# Manufacturers does correlate to the asking price of a vehicle.
# Our final models will shows us exactly how large this correlation is with regards to the other attributes.

# 1)What is the distribution of manufacturers?
ggplot(cars_edited, aes(y = manufacturer_name)) + geom_bar(aes(fill = manufacturer_name)) + geom_text(stat='count', aes(label=..count..), hjust=1)

# Do manufacturers have a significant impact on the asking price of a vehicle?
manuPriceDF_averages <- summarise(manuPriceDF, average_price_usd = mean(price_usd))
View(manuPriceDF_averages)
ggplot(manuPriceDF_averages, aes(x = average_price_usd, y = manufacturer_name)) + geom_bar(aes(fill = manufacturer_name),stat="identity") + geom_text(aes(label =  paste0("$",round(average_price_usd)), hjust = 1))

#one-way anova
manuSumm <- group_by(manuPriceDF, manuPriceDF$manufacturer_name) %>%
  summarise(
    count = n(),
    mean = mean(price_usd, na.rm = TRUE),
    sd = sd(price_usd, na.rm = TRUE)
  )
# Compute the analysis of variance
res.aovTwo <- aov(manuPriceDF$price_usd ~ manuPriceDF$manufacturer_name,
                  data = manuPriceDF)
summary(res.aovTwo)

###################################################################################################
#
# (Reid Hoffmeier)
#
# 3) What is the relationship between odometer and price?
# There is a low negative correlation between price and odometer. 
# We can conclude that although there is an impact many more attributes effect the price of a vehicle.
#

#Scatter plot: Odometer and price
ggplot (cars_edited, aes( x =odometer_value, y=price_usd)) + geom_point() + stat_smooth()

#Getting cor value
cor.test(cars_edited$odometer_value, cars_edited$price_usd)
# Since the p-value is less than 0.05 we can conclude that Price and Odometer have a low negative correlation
# The correlation coefficient is -0.4212043 and p-value of < 2.2e-16.

#Getting the formula for linear regression
odometer_on_price <- lm (price_usd ~ odometer_value, data = cars_edited)
odometer_on_price

#Scatter plot: Odometer and price with linear regression line
ggplot(cars_edited, aes(x=odometer_value, y=price_usd)) + geom_point() + stat_smooth(method=lm)

#Finding how well this line fits our data
summary(odometer_on_price)
# R^2 is very low which affirms that odometer is not a good indicator of price. We can suspect that several more variables are in play.
confint(odometer_on_price)
sigma(odometer_on_price)*100/mean(cars_edited$price_usd)
# Our prediction error rate is extremely high (87.80444%) which explains the low correlation

###################################################################################################
#
# (Reid Hoffmeier)
#
# 4) Scatter Plot, Simple Regression Analysis:
# Does the number of photos a vehicle has impact the selling price?
# A low positive correlation between number of photos a vehicle has and the selling price.
# However, on its own photo amount is not a good predictor of price and for that reason we must use several more attributes when predicting price.
#

#Scatter plot: Number of photos and price
ggplot(cars_edited, aes( x =number_of_photos, y=price_usd)) + geom_point() + stat_smooth()

#getting the cor value
cor.test(cars_edited$number_of_photos, cars_edited$price_usd)
# Since the p-value is less than 0.05 we can conclude that Price and Number of photos have a low positive correlation
# The correlation coefficient is 0.3168586 and p-value of < 2.2e-16.

#Getting the formula for linear regression
number_of_photos_on_price <- lm (price_usd ~ number_of_photos, data = cars_edited)
number_of_photos_on_price

#Scatter plot: Number of photos and price with linear regression line
ggplot (cars_edited, aes(x=number_of_photos, y=price_usd)) + geom_point() + stat_smooth(method=lm)

#Finding how well this line fits the data
summary(number_of_photos_on_price)
# R^2 is very low which affirms that number of photos is not a good indicator of price. 
# We can suspect that several more variables are in play.
confint(number_of_photos_on_price)
sigma(number_of_photos_on_price)*100/mean(cars_edited$price_usd)
# Our prediction error rate is extremely high (91.82279%) which explains the low correlation

###################################################################################################
#
# (Matthew Lane)
#
# 5) Scatter Plot, Simple Regression Analysis:
# Does the number of times a vehicle has been upped in the catalog to raise its position impact the selling price?
# The number of times a vehicle has been upped has a negligible impact on the selling price
#

#Regression analysis
ggplot(cars_edited, aes( x =up_counter, y=price_usd)) + geom_point() + stat_smooth()

#Correlation
cor.test(cars_edited$up_counter, cars_edited$price_usd)
up_counter_on_price <- lm (price_usd ~ up_counter, data = cars_edited)
up_counter_on_price

#Finding how well this line fits the data
summary(up_counter_on_price)
# R^2 is extremely low which affirms that number of photos is not a good indicator of price. 
confint(up_counter_on_price)
sigma(up_counter_on_price)*100/mean(cars_edited$price_usd)
# Our prediction error rate is extremely high (96.65168%) which confirms to us that up_counter is a terrible predictor of price(as we can see by the correlation test)

#Scatter plot: up counter and price with regression line
ggplot (cars_edited, aes(x=up_counter, y=price_usd)) + geom_point() + stat_smooth(method=lm)

###################################################################################################
#
# (Matthew Lane)
# 6) Mosaic Plot/ Chi-Squared Test, Two-Way ANOVA:
#
# Relationship between Engine Type and Body Type?
# Sedan and Gasoline is the most common followed by Gasoline and Hatchback
#
# What is the impact of Engine Type and Body Type on the selling price?
# Limousine and pickup trucks appear to have the only impact.

engine_body.data <- table(cars_edited$body_type, cars_edited$engine_type)
chisq.test(engine_body.data)
# Balloon Plot
ggplot(cars_edited, aes(body_type, engine_type)) + geom_count()

#Aov3
body_engine_type_on_price.aov <- aov(price_usd ~ engine_type * body_type, data = cars_edited)
summary(body_engine_type_on_price.aov)
model.tables(body_engine_type_on_price.aov, type="means", se = TRUE)

#Tukey HSD
TukeyHSD(body_engine_type_on_price.aov3)

###################################################################################################
#
# (Mikhail Mikhaylov)
#
# 7) Dplyr count with group_by, One-Way Anova:
# What is the most popular model?
# Most popular is Passat
# Can we conclude that the popularity of a model has a direct impact on the price of a vehicle?
# The popularity of a vehicle does seem to have an impact on the average_price of a vehicle
# More attributes would be needed to predict price
#

# Finding out model popularity
models_counted <- cars_edited %>% count(model_name) %>% arrange(desc(n))
View(models_counted)
# Most popular is Passat

# Do an anova test to see if model name significantly impacts price of a vehicle
model_price <- aov(price_usd ~ model_name, data = cars_edited)
summary(model_price)
# The popularity of a vehicle does seem to have an impact on the price of a vehicle

#Taking the linear regression
modelPrice <- lm (price_usd ~ model_name, data = cars_edited)

#Making sure the linear regression line matches the model
summary(modelPrice)
# R^2(0.6162) is relatively good which affirms that model name is a good indicator of price. 
confint(modelPrice)
sigma(modelPrice)*100/mean(cars_edited$price_usd)
# Our prediction error rate is lower than for other attributes (60.86683%) which confirms to us that model name is a better predictor of price
# Nevertheless, a prediction error of 60.86683% tells us that for prediction we will need more attributes.

###################################################################################################
#
# (Mikhail Mikhaylov)
# 8) Scatter plot, Two-Way ANOVA/ :
# What is the average age of each vehicle manufacturer?
# Does the manufacturer change how the production year impacts the selling price?

#Group cars by manufacturer name
manufacturer_year <- group_by(cars_edited, manufacturer_name)

#Summarize the manufacturer years average
manufacturer_year_averages <- summarise(manufacturer_year, average = mean(year_produced, na.rm = TRUE))
# 1) Average age of each vehicle manufacturer
View(manufacturer_year_averages)

#Scatter plot: Year produced by price and colored by manufacturer name
ggplot(cars_edited) + geom_point(aes(x = year_produced, y = price_usd, color = manufacturer_name))

# Do an anova test to see if year produced significantly impacts price of a vehicle
manufacturer_price <- aov(price_usd ~ manufacturer_name * year_produced, data = cars_edited)
summary(manufacturer_price)
# The manufacturer does change how the production year affects the selling price

#Taking the linear regression
yearPrice <- lm (price_usd ~ year_produced, data = cars_edited)

#Making sure the linear regression line matches the model
summary(yearPrice)
# R^2(0.4977) is relatively good which affirms that year produced is a good indicator of price. 
confint(yearPrice)
sigma(yearPrice)*100/mean(cars_edited$price_usd)
# Our prediction error rate is lower than for other attributes (68.61004%) which confirms to us that year produced is a decent predictor of price(albeit not at good as model name)
# A prediction error of 68.61004% tells us that for prediction we will need more attributes than just year produced.

###################################################################################################
#
# (Everyone) Goal:
# Gain insights into which variables have the largest impact on selling price of a vehicle.
# Create a predictive model based on these insights to create a predictive model.
#
###################################################################################################

## Multiple Linear Regression Models

# R^2 for test/train dataset
LMCont <- lm(price_usd ~ odometer_value
             + year_produced
             + number_of_photos
             + duration_listed
             + up_counter
             , data = train.data)

vif(LMCont)
#  odometer_value    year_produced number_of_photos 
#1.311552         1.375432         1.088560 
#duration_listed       up_counter 
#1.985559         1.995992 
step.LMConts <- LMCont %>% stepAIC(trace = FALSE)
vif(step.LMConts)
#odometer_value    year_produced number_of_photos 
#1.311552         1.375432         1.088560 
#duration_listed       up_counter 
#1.985559         1.995992 
summary(step.LMConts)
#See summary(step.LMConts).txt
coef(step.LMConts)
#(Intercept)   odometer_value    year_produced 
#-9.916423e+05    -4.419525e-03     4.981272e+02 
#number_of_photos  duration_listed       up_counter 
#1.481601e+02     2.241819e+00     2.252599e+00 
confint(step.LMConts)
#                2.5 %        97.5 %
#(Intercept)      -1.005960e+06 -9.773249e+05
#odometer_value   -4.833614e-03 -4.005436e-03
#year_produced     4.909951e+02  5.052594e+02
#number_of_photos  1.398282e+02  1.564919e+02
#duration_listed   1.625863e+00  2.857775e+00
#up_counter        6.685618e-01  3.836636e+00
#
# Predict using Multiple Linear Regression Model
LMContPrediction <- predict(step.LMConts, test.data)

# Prediction error, rmse
RMSE(LMContPrediction,test.data$price_usd)
#[1] 4573.511
# Compute R-square
R2(LMContPrediction,test.data$price_usd) ## R^2 for test/train is 50.95891%
#[1] 0.5095891
# Log transformation
LogLMConts <- lm(log(price_usd) ~ odometer_value
                 + year_produced
                 + number_of_photos
                 + duration_listed
                 + up_counter
                 , data = train.data)

vif(LogLMConts)
#  odometer_value    year_produced number_of_photos 
#1.311552         1.375432         1.088560 
#duration_listed       up_counter 
#1.985559         1.995992 

step.logConts <- LogLMConts %>% stepAIC(trace = FALSE)
vif(step.logConts)
#  odometer_value    year_produced number_of_photos 
#1.311231         1.375118         1.070669 
#duration_listed 
#1.000910 
summary(step.logConts)
#See summary(step.logConts).txt

coef(step.logConts)
#     (Intercept)   odometer_value    year_produced 
#-1.946299e+02     1.674743e-07     1.012069e-01 
#number_of_photos  duration_listed 
#1.907346e-02     5.426537e-04 
confint(step.logConts)
#                         2.5 %        97.5 %
#(Intercept)      -1.965454e+02 -1.927144e+02
#odometer_value    1.120756e-07  2.228729e-07
#year_produced     1.002527e-01  1.021610e-01
#number_of_photos  1.796785e-02  2.017907e-02
#duration_listed   4.841392e-04  6.011683e-04

# Predict using Multiple Linear Regression Model
LogLMContsPrediction <- step.logConts %>% predict(test.data)

# Prediction error, rmse
RMSE(LogLMContsPrediction,test.data$price_usd)
#[1] 9325.461

# Compute R-square
R2(LogLMContsPrediction,test.data$price_usd)
#[1] 0.4949189
# Log transformation is less accurate
###################################################################################################
## SVR Models
# Create SVR Model using Linear Method
modelSVRLinTrain <- train( price_usd ~ ., data = train.data, method = "svmLinear",
                           trControl = trainControl("cv", number =10),
                           preProcess = c("center", "scale"),
                           tuneLength = 10
)

summary(modelSVRLinTrain)
#Length  Class   Mode 
#1   ksvm     S4 
modelSVRLinTrain$bestTune
#C
#1 1

# Predict using SVR Model with Linear Method
modelSVRLinTrainPrediction <- predict(modelSVRLinTrain, test.data)

# Prediction error, rmse
RMSE(modelSVRLinTrainPrediction,test.data$price_usd)
#[1] 3257.887

# Compute R-square
R2(modelSVRLinTrainPrediction,test.data$price_usd)
#[1] 0.7772176

# Create SVR Model using Polynomial Method

modelSVRPolyTrain <- train(price_usd ~ ., data = train.data, method = "svmPoly",
                           trControl = trainControl("cv", number =10),
                           preProcess = c("center", "scale"),
                           tuneLength = 10
)

summary(modelSVRPolyTrain)
modelSVRPolyTrain$bestTune

# Predict using SVR Model with Linear Method
modelSVRPolyTrainPrediction <- predict(modelSVRPolyTrain, test.data)

# Prediction error, rmse
RMSE(modelSVRPolyTrainPrediction,test.data$price_usd)

# Compute R-square
R2(modelSVRPolyTrainPrediction,test.data$price_usd)

# Create SVR Model using Radial Method
modelSVRRadialTrain <- train(price_usd ~ ., data = train.data, method = "svmRadial",
                             trControl = trainControl("cv", number =10),
                             preProcess = c("center", "scale"),
                             tuneLength = 10
)

summary(modelSVRRadialTrain)
#Length  Class   Mode
#1   ksvm     S4					 
modelSVRRadialTrain$bestTune
#sigma   C
#10 0.001556481 128				   

# Predict using SVR Model with Radial Method
modelSVRRadialTrainPrediction <- predict(modelSVRRadialTrain, test.data)

# Prediction error, rmse
RMSE(modelSVRRadialTrainPrediction,test.data$price_usd)
#[1] 4752.231			 

# Compute R-square
R2(modelSVRRadialTrainPrediction,test.data$price_usd)
#[1] 0.5937837

###################################################################################################
## Decision Tree Regression Model

model_DT_Train <- train(price_usd ~ ., data = train.data, method = "rpart",
                        trControl = trainControl("cv",number = 10),
                        preProcess = c("center","scale"),
                        tuneLength = 10)

summary(model_DT_Train)
#See summary(model_DT_Train)(2nd Run).txt
#For results
model_DT_Train$bestTune
#          cp
#1 0.01032955
plot(model_DT_Train)


# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model_DT_Train$finalModel)
text(model_DT_Train$finalModel, digits = 3)

#Decision rules in the model
model_DT_Train$finalModel
See model_DT_Train$finalModel.txt						  

# Make predictions on the test data
prediction_DT_Train <- model_DT_Train %>% predict(test.data)

# Prediction error, rmse
RMSE(prediction_DT_Train,test.data$price_usd)
#[1] 3245.413	 

# Compute R-square
R2(prediction_DT_Train,test.data$price_usd)
#[1] 0.7529956			  

###################################################################################################
## Random Forest Model

random_forest_ranger <- train(price_usd ~ . ,
                              data = train.data,
                              method = "ranger",
                              trControl = trainControl("cv", number = 10),
                              preProcess = c("center","scale"),
                              tuneLength = 10
)

summary(random_forest_ranger)
random_forest_ranger$bestTune
plot(random_forest_ranger)


# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(random_forest_ranger$finalModel)
text(random_forest_ranger$finalModel, digits = 3)

# Make predictions on the test data
rf_predict_ranger <- predict(random_forest_ranger, test.data , type='response')

# Prediction error, rmse
RMSE(rf_predict_ranger,test.data$price_usd)

# Compute R-square
R2(rf_predict_ranger,test.data$price_usd)

###################################################################################################
## KNN Model

model_knn <- train(
  price_usd ~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 20
)

summary(model_knn$finalModel)
#Length Class      Mode     
#learn          2   -none-     list     
#k              1   -none-     numeric  
#theDots        0   -none-     list     
#xNames      1215   -none-     character
#problemType    1   -none-     character
#tuneValue      1   data.frame list     
#obsLevels      1   -none-     logical  
#param          0   -none-     list	

# Print the best tuning parameter k that maximizes model accuracy
model_knn$bestTune
#k
#1 5

# Plot model accuracy vs different values of k
plot(model_knn)

# Make predictions on the test data
knn_predictions <- model_knn %>% predict(test.data)
head(knn_predictions)
#[1] 9560.000 9000.000 4580.000 5648.494 8575.200 7720.000

# Compute the prediction error RMSE
RMSE(knn_predictions,test.data$price_usd)
#[1] 3693.127

# Compute R-square
R2(knn_predictions,test.data$price_usd)
#[1] 0.6802895

##############################################################################################################
#
# Considering Outliers
#

# Thresholds for Outliers
Outlier_List_Fences <- function(df) {
  Q <- quantile(df, probs = c(.25, .75))
  iqr <- IQR(df)
  Lower_Fence <- Q[1] - iqr * 1.5
  Upper_Fence <- Q[2] + iqr * 1.5
  if (Lower_Fence > min(df) & Upper_Fence < max(df))
    sprintf("Lower Fence: %f: Upper Fence: %f", Lower_Fence, Upper_Fence)
  else if (Lower_Fence < min(df))
    sprintf("Upper Fence: %f", Upper_Fence)
  else if (Upper_Fence > max(df))
    sprintf("Lower Fence: %f", Lower_Fence)
  else
    print("DataFrame has no valid outliers")
}

# Functions for Outlier Index and Values
Outlier_List_Index <- function(df) {
  Q <- quantile(df, probs = c(.25, .75))
  iqr <- IQR(df)
  Lower_Fence <- Q[1] - iqr * 1.5
  Upper_Fence <- Q[2] + iqr * 1.5
  which(df > Upper_Fence | df < Lower_Fence)
}

Outlier_List_Values <- function(df) {
  outliers <- boxplot(df, plot = FALSE)$out
}

#Show the outlier indexes and values of each continuous variable
Cars_continuous <- dplyr::select(cars_edited, 5 | 6 | 12 | 15:17)
map(Cars_continuous, Outlier_List_Fences)
map(Cars_continuous, Outlier_List_Index)
map(Cars_continuous, Outlier_List_Values)

# DataSet with all outliers removed entirely
# List of Outliers for every Continuous Variable
my_list <- map(Cars_continuous, Outlier_List_Values)
View(my_list)

# Remove all outliers from the dataframe
cars_edited_without_outliers <-  cars_edited
cars_edited_without_outliers <-
  cars_edited_without_outliers[!cars_edited_without_outliers$odometer_value %in% my_list[[1]], ]
cars_edited_without_outliers <-
  cars_edited_without_outliers[!cars_edited_without_outliers$year_produced %in% my_list[[2]], ]
cars_edited_without_outliers <-
  cars_edited_without_outliers[!cars_edited_without_outliers$price_usd %in% my_list[[3]], ]
cars_edited_without_outliers <-
  cars_edited_without_outliers[!cars_edited_without_outliers$number_of_photos %in% my_list[[4]], ]
cars_edited_without_outliers <-
  cars_edited_without_outliers[!cars_edited_without_outliers$up_counter %in% my_list[[5]], ]
cars_edited_without_outliers <-
  cars_edited_without_outliers[!cars_edited_without_outliers$duration_listed %in% my_list[[6]], ]
View(cars_edited_without_outliers)

#Summary of Attributes with Outliers
summary(Cars_continuous)

#Summary of Attributes_without Outliers
summary(cars_edited_without_outliers)

# Train No_Outliers
trainingCont.samples <- cars_edited_without_outliers$manufacturer_name %>% createDataPartition(p = 0.8, list = FALSE)
trainCont.data <- cars_edited_without_outliers[training.samples,]
testCont.data <- cars_edited_without_outliers[-training.samples,]



LMOutliers <- lm(price_usd ~ odometer_value
         + year_produced
         + number_of_photos
         + duration_listed
         + up_counter
         , data = cars_edited_without_outliers)
step.LMOutliers <- LM %>% stepAIC(trace = FALSE)
vif(step.LMOutliers)
summary(step.LMOutliers)

# R^2 for test/train dataset
LMContOutliers <- lm(price_usd ~ odometer_value
             + year_produced
             + number_of_photos
             + duration_listed
             + up_counter
             , data = trainCont.data)

vif(LMContOutliers)
step.LMContsOutliers <- LMContOutliers %>% stepAIC(trace = FALSE)
vif(step.LMContsOutliers)
summary(step.LMContsOutliers)
coef(step.LMContsOutliers)
confint(step.LMContsOutliers)
LMContOutliersPrediction <- predict(step.LMContsOutliers, test.data)
rmse(test.data$price_usd, LMContOutliersPrediction)
rmse(test.data$price_usd, LMContOutliersPrediction)/mean(test.data$price_usd)  ##0.7025614
R2(LMContOutliersPrediction,test.data$price_usd) ## R^2 for test/train is 51.10202%

##############################################################################################################
#
# Clean up
#

# Clear plots
dev.off()

# Clear environment
rm(list = ls())

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")
