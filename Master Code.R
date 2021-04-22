###################################################################################################
#
# CSC 335 Project: Used Cars Market in Belarus
#

#Import all the library's we are using
library(tidyverse)
library(e1071) #SVM
library(car) #predict
library(Metrics) #rmse
library(caret) #partiiton
library(MASS) #stepwise
#library(multcomp) #glht
###################################################################################################
#
# Evaluate the data
#

#Read our dataset into the cars variable
cars <- read_csv("cars.csv")

View(cars) #view the data

###################################################################################################
#
# Edit(modify) the data
#

#Removing unnecessary columns from cars. Store that data in cars_edited
# -(19) is a column that shows the number of times a car has been upped. This column not descriptive and has been removed
# -(20:29) are boolean columns for various features. There is no description of what these features are and for that reason they have been omitted.
cars_edited <- dplyr::select(cars, -8 & -(12:13) & -(20:29))
View(cars_edited) #view the data


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
#   cars_edited %>% mutate(
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
cars_edited <-
  cars_edited %>% mutate(engine_capacity = coalesce(engine_capacity, 999))

# Check for Duplicates and remove them
which(duplicated(cars_edited))
cars_edited <- cars_edited %>% distinct()

# View the changes the mutate made
View(cars_edited)

#Change model_name to factor. Useful later on for prediction modeling
str(cars_edited)
cars_edited$model_name <- as.factor(cars_edited$model_name)
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
# VIEWS:
#
# 2) Show unique car model names and how many there are
# 7) Show unique years produced for cars, and how many there are
# 12) Show unique engine types and how many there are
# 17) Number of cars with same price
# 22) Posts with number of photos per car
# 25) The count unique up time for each car
# 28) The count of unique Duration listed for each car
#
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
# 
# Other Graphs:
#
# 1) Getting the unique entries for all columns and displaying how often they appear
# 3) Plotting the number of cars with automatic or mechanical transmissions
# 4) Plotting cars by color and how many there are
# 10) Graph to show what cars use what engine type (type of fuel)
# 11) graph to show engine type (Electric, Diesel, Gasoline)
# 15) Bar graph Body type: count how many cars have the same body type
# 16) Bar graph Drive train: How many cars have certain drive trains
# 20) Bar graph Is exchangeable: Counting the number of cars that are exchangeable
# 21) Bar graph Location region: Count the number of cars in a region
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


###################################################################################################
#
# Plotting Graphs to investigate relationships
#
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
# Line 287 Group cars by manufacturer, and get it's mean price
# Line 302 Group by car body type and get it's mean price
# 9) The correlation between car body type, price, AND engine fuel
#


# 1) Graph to show the amount of cars(by manufacturer name) in a region BALLOON PLOT
ggplot(cars_edited, aes(location_region, manufacturer_name)) + geom_count()


# 2) Graph to show the price of a car according to its year produced SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point() + geom_smooth()

# 3) Graph to show the amount of cars(density) according to it's price LINE GRAPH
ggplot(cars_edited, aes(price_usd, ..density..)) + geom_freqpoly(binwidth = 500)

#Group cars by manufacturer, and get it's mean price
cars_edited %>% group_by(manufacturer_name) %>% summarize(mean(price_usd)) %>% View()

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


###################################################################################################
#
# Answering Questions
#


###################################################################################################
#
# (Emma Doyle) 
# 
# 1) 
#
# Box Plots, Bar Graph, ANOVA?: What impact does a region have on price?
# What impact does region have on price?
# Regions: Minsk, Gomel, Brest, Vitebsk, Mogilev, Grodno
#

#Aggregating the data of price to region to get the mean of prices
#per region
regionPriceDF <- Cars_No_Outliers %>% select(location_region, price_usd)
aggregatedRegions <- aggregate(regionPriceDF, 
                               by = list(regionPriceDF$location_region), FUN = mean)
#Creating pie chart to display data
pie(aggregatedRegions$price_usd,aggregatedRegions$Group.1)

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
# 2)
#
# Pie Graph/Box Plot, One-Way Anova?: 
#
# What is the distribution of manufacturers and whether manufacturers have a significant impact on the asking price of a vehicle?
#
# What is the distribution of manufacturers and whether manufacturers have a significant impact on the asking price of a vehicle?   
#

#price/manufacturers
manuPriceDF <- Cars_No_Outliers %>% select(manufacturer_name, price_usd)
aggregatedManufacturers <- aggregate(manuPriceDF, 
                             by = list(manuPriceDF$manufacturer_name), FUN = mean)
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
#making bar graph
#showing the correlation between particular manufacturers and price
#therefore not all manufacturers are needed to show some brands 
# can charge more than others.
aggManu<-data.frame(aggregatedManufacturers)
aggManu$manufacturer_name <- NULL
aggManuSmall <- aggManu[-c(50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44, 45, 46, 
                           47, 48, 49, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 
                           36, 37, 38, 39, 25, 24, 23, 22, 21, 20, 9 ,10, 19, 2),]
barplot(aggManuSmall$price_usd~aggManuSmall$Group.1, xlab="Manufacturer", ylab="Price")

###################################################################################################
#
# (Reid Hoffmeier) 
#
# 3) Scatter Plot/Box-Plot, Simple Regression Analysis: 
# What is the relationship between odometer and price?
ggplot (cars_edited, aes( x =odometer_value, y=price_usd)) + geom_point() + stat_smooth()
cor(cars_edited$odometer_value, cars_edited$price_usd)
odometer_on_price <- lm (price_usd ~ odometer_value, data = cars_edited)
ggplot (cars_edited, aes(x=odometer_value, y=price_usd)) + geom_point() + stat_smooth(method=lm)
summary(odometer_on_price)
confint(odometer_on_price)
sigma(odometer_on_price)*100/mean(cars_edited$price_usd)

# 4) Scatter Plot, Simple Regression Analysis: 
# Does the number of photos a vehicle has impact the selling price?
#
ggplot (cars_edited, aes( x =number_of_photos, y=price_usd)) + geom_point() + stat_smooth()
cor(cars_edited$number_of_photos, cars_edited$price_usd)
number_of_photos_on_price <- lm (price_usd ~ number_of_photos, data = cars_edited)
ggplot (cars_edited, aes(x=number_of_photos, y=price_usd)) + geom_point() + stat_smooth(method=lm)
summary(number_of_photos_on_price)
confint(number_of_photos_on_price)
sigma(number_of_photos_on_price)*100/mean(cars_edited$price_usd)

# (Matthew Lane) 
# 
# 5) Scatter Plot, Simple Regression Analysis:
# Does the number of times a vehicle has been upped in the catalog to raise its position impact the selling price?
#
# Yes, the number of times a vehicle has been upped raises the selling price (It's linear)
#

#regression analysis
ggplot (cars_edited, aes( x =up_counter, y=price_usd)) + geom_point() + stat_smooth()
#Correlation
cor(cars_edited$up_counter, cars_edited$price_usd)
up_counter_on_price <- lm (price_usd ~ up_counter, data = cars_edited)
#The estimated regression line equation can be written as 
#price_usd = 6493.05 + 8.569*up_counter
#The Intercept (b0) is 6493.05.  It can be interpreted as the predicted price in usd for an up counter of 0.  This means that if there are no up counters used, the average sales price would be around $6493.05.
#The regression beta coefficient for the variable up_counter (b1) is 8.569.  For an up_counter value of 100, Average sales price would be 6493.05+8.569*100, $7349.95, and increase of $857.00.
ggplot (cars_edited, aes(x=up_counter, y=price_usd)) + geom_point() + stat_smooth(method=lm)
summary(up_counter_on_price)
#Confidence Interval
confint(up_counter_on_price)
sigma(up_counter_on_price)*100/mean(cars_edited$price_usd)

###################################################################################################
#
# (Matthew Lane) 
# 6) Mosaic Plot/ Chi-Squared Test, Two-Way ANOVA: 
# Relationship between Engine Type and Body Type? 
# 
# 
#
# What is the impact of Engine Type and Body Type on the selling price?
#

#Mosaic Plot
mosaicplot( table(cars_edited$body_type, cars_edited$engine_type), shade=TRUE, las=2, main="Engine Type vs Body Type")
#Aov3
body_engine_type_on_price.aov3 <- aov(price_usd ~ engine_type + body_type, data = cars_edited)
summary(body_engine_type_on_price.aov3)
model.tables(body_engine_type_on_price.aov3, type="means", se = TRUE)
#Tukey HSD
TukeyHSD(body_engine_type_on_price.aov3)
#General Linear Hypothesis
summary(glht(body_engine_type_on_price.aov3, lincft = mcp))
#Limousine and pickup trucks appear to have the only impact


###################################################################################################
# 
# (Mikhail Mikhaylov)
# 7) Dplyr count with group_by, One-Way Anova:
# What is the most popular model and whether we can conclude that the popularity of a model has a direct impact on the price of a vehicle?
#


# Finding out model popularity
models_counted <- cars_edited %>% count(model_name) %>% arrange(desc(n))
View(models_counted)
# Most popular is Passat

# Find price average for each model and appent count to it
models_sorted <- group_by(cars_edited, model_name)
View(models_sorted)
models_sorted_averages <- summarise(models_sorted, average_price_usd = mean(price_usd))
View(models_sorted_averages)

# Figure out count for each model
models_sorted_avg_with_cnt <- models_sorted_averages %>% mutate(counts = count(cars_edited, model_name) %>% select(2))
View(models_sorted_avg_with_cnt)
count(cars_edited, model_name) %>% select(2)

models_sorted_avg_with_cnt$counts <- as.numeric(unlist(models_sorted_avg_with_cnt$counts))

model_price <- aov(average_price_usd ~ counts, data = models_sorted_avg_with_cnt)
summary(model_price)

# The popularity of a vehicle does seem to have an impact on the average_price of a vehicle

###################################################################################################
#
# (Mikhail Mikhaylov)
# 8) Bar graph, Two-Way ANOVA/ :
# What is the average age of each vehicle manufacturer?
# 
# And whether the manufacturer changes how the production year impacts the selling price?
#

manufacturer_year <- group_by(cars_edited, manufacturer_name)
manufacturer_year_averages <- summarise(manufacturer_year, average = mean(year_produced, na.rm = TRUE))
View(manufacturer_year_averages)

ggplot(manufacturer_year_averages) + geom_point(aes(x = manufacturer_name, y = average))

ggplot(cars_edited) + geom_point(aes(x = year_produced, y = price_usd, color = manufacturer_name))

manufacturer_price <- aov(price_usd ~ manufacturer_name * year_produced, data = cars_edited)
summary(manufacturer_price)

# The manufacturer does change how the production year affects the selling price

###################################################################################################
#
# (Everyone) Goal:
# Gain insights into which variables have the largest impact on selling price of a vehicle.
# Create a predictive model based on these insights to create a predictive model.
#

model <- train(
  price_usd ~ ., data = train.data, method = "lm",family="binomial")

summary(model)
  
step.model <- model %>% stepAIC(trace = FALSE)
coef(step.model)


model1 <- lm(
  price_usd ~ odometer_value
  + year_produced
  + number_of_photos
  + duration_listed
  + up_counter
  + is_exchangeable
  + location_region
  + body_type
  + transmission
  + color
  + engine_type
  + engine_fuel
  + engine_capacity
  + model_name
  + manufacturer_name
  ,
  train.data
)
summary(model1) %>% View()

step.model <- model1 %>% stepAIC(trace = FALSE)
coef(step.model) %>% View()


model2 <- lm(
  log(price_usd) ~ odometer_value
  + year_produced
  + number_of_photos
  + duration_listed
  + up_counter
  + is_exchangeable
  + location_region
  + body_type
  + transmission
  + color
  + engine_type
  + engine_fuel
  + engine_capacity
  + model_name
  + manufacturer_name
  ,
  train.data
)
summary(model2)

vif(model2)

modelSVM <- train( price_usd ~ ., data = train.data, method = "svmPoly",
                   trControl = trainControl("cv", number =10), 
                   preProcess = c("center", "scale"),
                   tuneLength = 4
                   )
summary(modelSVM)


# To prevent errors from the test.data encountering new factors we proceed as following:
#add all levels of 'model_name' in 'test.data' dataset to train.data$xlevels[["y"]] in the fit object
step.model$xlevels[["model_name"]] <- union(step.model$xlevels[["model_name"]], levels(test.data[["model_name"]]))

# cars_edited[training.samples,] %>% select(model_name) %>% View()
# cars_edited[-training.samples,] %>% select(model_name) %>% View()
# 
# (test.data$model_name %in% train.data$model_name) %>% as.tibble() 
# 
# subset(cars_edited, cars_edited[training.samples] %in% cars_edited[-training.samples,])
# 
# levels(train.data$model_name) <- c(levels(test.data$model_name, newFactorLevel))
# 
vif(model1)
prediction <- step.model %>% predict(test.data)
prediction %>% as.tibble()
sigma(prediction)/mean(test.data$price_usd)
rmse(test.data$price_usd, prediction)

colnames(cars_edited)

# [1] "manufacturer_name" "model_name"        "transmission"      "color"             "odometer_value"
# [6] "year_produced"     "engine_fuel"       "engine_type"       "engine_capacity"   "body_type"
# [11] "drivetrain"        "price_usd"         "is_exchangeable"   "location_region"   "number_of_photos"
# [16] "up_counter"        "duration_listed"

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
Cars_continuous <- select(cars_edited, 5 | 6 | 12 | 15:17)
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

