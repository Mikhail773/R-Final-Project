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
# #Recode foreign language into their English meaning (manufacturer_name)
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
# #Recode foreign language into their English meaning (model_name)
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

# Recode foreign language into their English meaning (engine_fuel)
cars_edited <- cars_edited %>% mutate(engine_fuel = dplyr::recode(engine_fuel,'gas' = "compressed natural gas"))

# Check the na's in the dataset
colSums(is.na(cars))
# NA is in the categorical attribute engine-capacity

# Lets arbitrarily pick 999 to denote NA. (Engine-Capacity is categorical so this can be done)
cars_edited <- cars_edited %>% mutate(engine_capacity = coalesce(engine_capacity, 999))

# Check for Duplicates and remove them
which(duplicated(cars_edited))
cars_edited <- cars_edited %>% distinct()

# View the changes the mutate made
View(cars_edited)

#Change model_name to factor. Useful later on for prediction modeling
str(cars_edited)
cars_edited$model_name <- as.factor(cars_edited$model_name)
cars_edited$engine_fuel <- as.factor(cars_edited$engine_fuel)
###################################################################################################
#
# Split Dataset into Training and Testing for our Models
#
set.seed(123)
training.samples <- cars_edited$manufacturer_name %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- cars_edited[training.samples,]
test.data <- cars_edited[-training.samples,]

View(train.data)
View(test.data)

###################################################################################################
#
# Investigating Variables: Viewing the data with our modifications
#
# TABLE:
#
# 2) Show unique car model names and how many there are
# 7) Show unique years produced for cars, and how many there are
# 12) Show unique engine types and how many there are
# 17) Number of cars with same price
# 22) Posts with number of photos per car
# 25) The count unique up time for each car
# 28) The count of unique Duration listed for each car
#
# BOX PLOTS:
#
# 5) Odometer Value
# 8) year produced
# 13) Engine capacity
# 18) Price USD
# 23) Number of photos
# 26) Up counter
# 29) Duration listed
#
# DATA SKEWED:
#
# 6) Odometer Value
# 9) Year produced
# 14) Engine capacity
# 19) Price USD
# 24) Number of photos
# 27) Up counter
# 30) Duration listed
#
# Other Graphs:
#
# Bar Graphs:
#
# 3) Plotting the number of cars with automatic or mechanical transmissions
# 4) Plotting cars by color and quantity
# 10) Graph to show what cars use what engine type (type of fuel)
# 11) graph to show engine type (Electric, Diesel, Gasoline)
# 15) Bar graph Body type: count how many cars have the same body type
# 16) Bar graph Drive train: How many cars have certain drive trains
# 20) Bar graph Is exchangeable: Counting the number of cars that are exchangeable
# 21) Bar graph Location region: Count the number of cars in a region
# 31) What is the distribution of manufacturers?
#
# Histogram Graph:
# 1) Getting the unique entries for all columns and displaying how often they appear
#

# 1) Getting the unique entries for all columns and displaying how often they appear
ggplot(cars_edited, mapping = aes(y = manufacturer_name)) + geom_histogram(stat ="count") + geom_text(stat = "count", aes(label = after_stat(count)), hjust = -1)

# 2) A table to show unique car model names and how many there are
View(cars_edited %>% count(model_name))

# 3) Plotting the number of cars with automatic or mechanical transmissions
ggplot(cars_edited, mapping = aes(x = transmission)) + geom_bar(stat = "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 4) Plotting cars by color and how many there are
ggplot(cars_edited, mapping = aes(x = color)) + geom_bar(stat = "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 5) Box plot Odometer Value: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(odometer_value))

# 6) Histogram Odometer Value: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(mapping = aes(odometer_value))

# 7) A table to show unique years produced for cars, and how many there are
View(cars_edited %>% count(year_produced))

# 8) Box plot year produced: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(year_produced))

# 9) Histogram Year produced: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(mapping = aes(year_produced))

# 10) Graph to show what cars use what engine type (type of fuel)
ggplot(cars_edited, aes(x = engine_fuel), stat = "count") + geom_bar(mapping = aes(fill = engine_type))  + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 11) graph to show engine type (Electric, Diesel, Gasoline)
ggplot(cars_edited, aes(x = engine_type), stat = "count") + geom_bar()  + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 12) A table to show unique engine types and how many there are
View(cars_edited %>% count(engine_capacity))

# 13) Box plot Engine capacity: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(engine_capacity))

# 14) Bar graph Engine capacity: Graph to see how the data is skewed
ggplot(cars_edited) + geom_bar(mapping = aes(engine_capacity))

# 15) Bar graph Body type: count how many cars have the same body type
ggplot(cars_edited, mapping = aes(x = body_type), stat = "count") + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 16) Bar graph Drive train: How many cars have certain drive trains
ggplot(cars_edited, mapping = aes(x = drivetrain), stat = "count") + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 17) Number of cars with same price
View(cars_edited %>% count(price_usd))

# 18) Box plot Price USD: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(price_usd))

# 19) Histogram Price USD: Check to see how skewed the data is
ggplot(cars_edited) + geom_histogram(mapping = aes(price_usd))

# 20) Bar graph Is exchangeable: Counting the number of cars that are exchangeable
ggplot(cars_edited, mapping = aes(x = is_exchangeable)) + geom_bar(stat = "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 21) Bar graph Location region: Count the number of cars in a region
ggplot(cars_edited, mapping = aes(x = location_region)) + geom_bar(stat = "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

# 22) View: posts with number of photos per car
View(cars_edited %>% count(number_of_photos))

# 23) Box plot Number of photos: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(number_of_photos))

# 24) Histogram Number of photos: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(mapping = aes(number_of_photos))

# 25) View Up counter: The count unique up time for each car
View(cars_edited %>% count(up_counter))

# 26) Box plot Up counter: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(up_counter))

# 27) Histogram Up counter: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(mapping = aes(up_counter))

# 28) View Duration listed: The count of unique Duration listed for each car
View(cars_edited %>% count(duration_listed))

# 29) Box plot Duration listed: investigating how our outliers look with our modifications
ggplot(cars_edited) + geom_boxplot(mapping = aes(duration_listed))

# 30) Histogram Duration listed: Graph to see how the data is skewed
ggplot(cars_edited) + geom_histogram(mapping = aes(duration_listed))

# 31)What is the distribution of manufacturers?
ggplot(cars_edited, aes(y = manufacturer_name)) + geom_bar(aes(fill = manufacturer_name)) + geom_text(stat='count', aes(label=..count..), hjust=1)

###################################################################################################
#
# Plotting Graphs to investigate relationships
#
# Scatter Plot:
# 2) Price of a car according to its year produced
# 6) Price of a car according to it's year produced AND body type
# 7) Price of a car according to it's Odometer AND engine fuel
# 10) Price of a car according to it's number of photos incl. engine fuel
#
# Balloon Plot:
# 1) The amount of cars(by manufacturer name) in a region
#
# Line Graph:
# 3) the amount of cars(density) according to it's price
#
# Bar Graph:
# 4) The number of cars in specific colors(10 red cars, 8 blue cars etc.) by region
#
# Box Plot:
# 8) The outliers with body type and price
#
#
# Misc:
# 9) The correlation between car body type, price, AND engine fuel
# 11) Group cars by manufacturer, and get it's mean price

# 1) Graph to show the amount of cars(by manufacturer name) in a region BALLOON PLOT
ggplot(cars_edited, aes(location_region, manufacturer_name)) + geom_count()

# 2) Graph to show the price of a car according to its year produced SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point() + geom_smooth()

# 3) Graph to show the amount of cars(density) according to it's price LINE GRAPH
ggplot(cars_edited, aes(price_usd, ..density..)) + geom_freqpoly(binwidth = 500)

# 4) Graph to show the number of cars in specific colors(10 red cars, 8 blue cars etc.) by region BAR GRAPH
ggplot(cars_edited, aes(color)) + geom_bar(aes(fill = location_region))

# 5) Graph to show the price of a car according to it's millage(odometer) SCATTER PLOT
ggplot(cars_edited, aes(odometer_value, price_usd)) + geom_point(aes(color = is_exchangeable)) + geom_smooth()

# 6)Graph to show the price of a car according to it's year produced AND body type SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point(aes(color = body_type)) + geom_smooth()

# 7) Graph to show the price of a car according to it's Odometer AND engine fuel SCATTER PLOT
ggplot(cars_edited, mapping = aes(x = odometer_value, y = price_usd)) + geom_point() + geom_smooth()

# Group by car body type and get it's mean price
group_by(cars_edited, body_type) %>% summarise(price_mean = mean(price_usd)) -> mean_cars

# 8) Graph to show the outliers with body type and price BOX PLOT
ggplot(cars_edited) + geom_boxplot(mapping = aes(x = reorder(body_type, price_usd), y =
                                                   price_usd))
# 9) Graph to show the correlation between car body type, price, AND engine fuel
ggplot(cars_edited) + geom_point(mapping = aes(x = body_type, y = price_usd, color = engine_fuel))

# 10 ) Graph to show the price of a car according to it's number of photos incl. engine fuel SCATTER PLOT
ggplot(cars_edited) + geom_point(mapping = aes(x = number_of_photos, y = price_usd, color = engine_fuel))

# 11) Group cars by manufacturer, and get it's mean price
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
# Region does correlate with price. However, a correlation between region and price does not necessary mean that one leads to another. 
# The best indicator of the relationship will be seen in the total model.
# Regions: Minsk, Gomel, Brest, Vitebsk, Mogilev, Grodno
#

#Aggregating the data of price to region to get the mean of prices
#per region
regionPriceDF <- group_by(cars_edited, location_region)
regionPriceDF_averages <- summarise(regionPriceDF, average_price_usd = mean(price_usd))
View(regionPriceDF_averages)
percentRegion <- paste0(round(100*regionPriceDF_averages$average_price_usd/sum(regionPriceDF_averages$average_price_usd), 2), "%")
pie(regionPriceDF_averages$average_price_usd, labels = percentRegion, main = "Region Price Distribution", col = rainbow(length(regionPriceDF_averages$average_price_usd)))
legend("right", c("Brest Region", "Gomel Region", "Grodno Region", "Minsk Region", "Mogilev Region", "Vitebsk Region"), cex = 0.8,
       fill = rainbow(length(regionPriceDF_averages$average_price_usd)))

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
ggplot (cars_edited, aes(x=odometer_value, y=price_usd)) + geom_point() + stat_smooth(method=lm)

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
#

# Finding out model popularity
models_counted <- cars_edited %>% count(model_name) %>% arrange(desc(n))
View(models_counted)
# Most popular is Passat

# Find price average for each model and append count to it
models_sorted <- group_by(cars_edited, model_name)
View(models_sorted)
models_sorted_averages <- summarise(models_sorted, average_price_usd = mean(price_usd))
View(models_sorted_averages)

# Figure out count for each model
models_sorted_avg_with_cnt <- models_sorted_averages %>% mutate(counts = count(cars_edited, model_name) %>% dplyr::select(2))
View(models_sorted_avg_with_cnt)
models_sorted_avg_with_cnt$counts <- as.numeric(unlist(models_sorted_avg_with_cnt$counts))

#Scatter plot: count of models VS price
ggplot (models_sorted_avg_with_cnt, aes( x =counts, y=average_price_usd)) + geom_point() + stat_smooth()

#Taking the linear regression
modelPricePerCount <- lm (price_usd ~ model_name, data = cars_edited)

#Plotting the linear regression
ggplot (models_sorted_avg_with_cnt, aes(x=counts, y=average_price_usd)) + geom_point() + stat_smooth(method=lm)

#Making sure the linear regression line matches the model
summary(modelPricePerCount)
# R^2 is extremely low which affirms that number of photos is not a good indicator of price. 
confint(modelPricePerCount)
sigma(modelPricePerCount)*100/mean(models_sorted_avg_with_cnt$average_price_usd)


model_price <- aov(average_price_usd ~ counts, data = models_sorted_avg_with_cnt)
summary(model_price)

# The popularity of a vehicle does seem to have an impact on the average_price of a vehicle

###################################################################################################
#
# (Mikhail Mikhaylov)
# 8) Scatter plot, Two-Way ANOVA/ :
# What is the average age of each vehicle manufacturer?
#
# 1) A view of the age of each vehicle manufacturer
#
# And whether the manufacturer changes how the production year impacts the selling price?
#
# The manufacturer does change how the production year affects the selling price
#

#Group cars by manufacturer name
manufacturer_year <- group_by(cars_edited, manufacturer_name)

#Summarise the manufacturer years average
manufacturer_year_averages <- summarise(manufacturer_year, average = mean(year_produced, na.rm = TRUE))
# 1) Average age of each vehicle manufacturer
View(manufacturer_year_averages)

# Figure out count for each Manufacturer
manu_sorted_avg_with_cnt <- manufacturer_year_averages %>% mutate(counts = count(cars_edited, manufacturer_name) %>% dplyr::select(2))
View(manu_sorted_avg_with_cnt)

#Scatter plot: Manufacturer name and average year
ggplot(manufacturer_year_averages) + geom_point(aes(x = manufacturer_name, y = average))

#Scatter plot: Year produced by price and colored by manufacturer name
ggplot(cars_edited) + geom_point(aes(x = year_produced, y = price_usd, color = manufacturer_name))

#Summary of manufacturer price
manufacturer_price <- aov(price_usd ~ manufacturer_name * year_produced, data = cars_edited)
summary(manufacturer_price)

# The manufacturer does change how the production year affects the selling price

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
step.LMConts <- LMCont %>% stepAIC(trace = FALSE)
vif(step.LMConts)
summary(step.LMConts)
coef(step.LMConts)
confint(step.LMConts)

# Predict using Multiple Linear Regression Model
LMContPrediction <- predict(step.LMConts, test.data)

# Prediction error, rmse
RMSE(LMContPrediction,test.data$price_usd)

# Compute R-square
R2(LMContPrediction,test.data$price_usd) ## R^2 for test/train is 50.95891%

# Log transformation
LogLMConts <- lm(log(price_usd) ~ odometer_value
                 + year_produced
                 + number_of_photos
                 + duration_listed
                 + up_counter
                 , data = train.data)

vif(LogLMConts)
step.logConts <- LogLMConts %>% stepAIC(trace = FALSE)
vif(step.logConts)
summary(step.logConts)
coef(step.logConts)
confint(step.logConts)

# Predict using Multiple Linear Regression Model
LogLMContsPrediction <- step.logConts %>% predict(test.data)

# Prediction error, rmse
RMSE(LogLMContsPrediction,test.data$price_usd)

# Compute R-square
R2(LogLMContsPrediction,test.data$price_usd)
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
Method
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
modelSVRRadialTrain$bestTune

# Predict using SVR Model with Radial Method
modelSVRRadialTrainPrediction <- predict(modelSVRRadialTrain, test.data)

# Prediction error, rmse
RMSE(modelSVRRadialTrainPrediction,test.data$price_usd)

# Compute R-square
R2(modelSVRRadialTrainPrediction,test.data$price_usd)

###################################################################################################
## Decision Tree Regression Model

model_DT_Train <- train(price_usd ~ ., data = train.data, method = "rpart",
                        trControl = trainControl("cv",number = 10),
                        preProcess = c("center","scale"),
                        tuneLength = 10)

summary(model_DT_Train)
model_DT_Train$bestTune
plot(model_DT_Train)


# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model_DT_Train$finalModel)
text(model_DT_Train$finalModel, digits = 3)

#Decision rules in the model
model_DT_Train$finalModel

# Make predictions on the test data
prediction_DT_Train <- model_DT_Train %>% predict(test.data)

# Prediction error, rmse
RMSE(prediction_DT_Train,test.data$price_usd)

# Compute R-square
R2(prediction_DT_Train,test.data$price_usd)

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

# Print the best tuning parameter k that maximizes model accuracy
model$bestTune

# Plot model accuracy vs different values of k
plot(model_knn)

# Make predictions on the test data
knn_predictions <- model_knn %>% predict(test.data)
head(knn_predictions)

# Compute the prediction error RMSE
RMSE(knn_predictions,test.data$price_usd)

# Compute R-square
R2(knn_predictions,test.data$price_usd)

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

