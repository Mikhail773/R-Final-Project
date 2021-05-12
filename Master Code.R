###################################################################################################
#
# CSC 335 Project: Used Cars Market in Belarus
#

#Import all the library's we are using
library(tidyverse)
library(hexbin)
library(e1071) #SVM
library(car) #predict
library(caret) #partiiton
library(MASS) #stepwise
library(kernlab) #SVM
library(rpart) # Decision Tree Regression
library(randomForest) #  Random Forest Tree Regression
library(ranger) # RFT more than 53 factors
library(pROC) # Calculate roc
library(vctrs)
###################################################################################################
#
# Evaluate the data
#

#Read our dataset into the cars variable
cars <- read_csv("cars.csv")

View(cars) #View the data

###################################################################################################
#
# Edit(modify) the data: Recoding was done to the file and the CSV file was saved that way.
# This prevented issues arising from locale settings and made working on the code easier as a group.
#

#Removing unnecessary columns from cars. Store that data in cars_edited
# -(19) is a column that shows the number of times a car has been upped. This column not descriptive and has been removed
# -(20:29) are boolean columns for various features. There is no description of what these features are and for that reason they have been omitted.
cars_edited <- cars %>% dplyr::select(-8 & -(12:13) & -(20:29))
View(cars_edited) #View the data

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

# Factor
cars_edited$is_exchangeable <- as.factor(cars_edited$is_exchangeable)
cars_edited$model_name <- as.factor(cars_edited$model_name)
# Check for Duplicates and remove them
which(duplicated(cars_edited))
cars_edited <- cars_edited %>% distinct()

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
legend("right", c("Automatic", "Mechanical"), cex = 0.8, fill = rainbow(length(transmissionCounted)))
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
# 1) Q: What impact does region have on price?
#
# A: Region does impact price. However, the extent of this impact would have to be assessed in a model that includes more attributes.
# The best indicator of the relationship will be seen in the total model.
# Regions: Minsk, Gomel, Brest, Vitebsk, Mogilev, Grodno
#

# Aggregating the data of price to region to get the mean of prices per region
# Aggregating the data of price to region to get the mean of prices per region
regionPriceDF <- group_by(cars_edited, location_region)
regionPriceDF_averages <- summarise(regionPriceDF, average_price_usd = mean(price_usd))
ggplot(regionPriceDF_averages,aes(x= location_region, y = average_price_usd)) + geom_bar(stat = "identity")

# one-way anova
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

# Tukey Test
TukeyHSD(res.aov)

###################################################################################################
#
# (Emma Doyle)
#
# 2) Q: Do manufacturers have a significant impact on the asking price of a vehicle?
# Manufacturers does correlate to the asking price of a vehicle.
#
# A: Our final models will show us exactly how large this correlation is with regards to the other attributes.
#

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

# Tukey Test
TukeyHSD(res.aovTwo)

###################################################################################################
#
# (Reid Hoffmeier)
#
# 3) A: What is the relationship between odometer and price?
#
# Q: There is a low negative correlation between price and odometer.
# We can conclude that although there is an impact many more attributes effect the price of a vehicle.
#

# Scatter plot: Odometer and price
ggplot (cars_edited, aes( x =odometer_value, y=price_usd)) + geom_point() + stat_smooth()

# Getting cor value
cor.test(cars_edited$odometer_value, cars_edited$price_usd)
# Since the p-value is less than 0.05 we can conclude that Price and Odometer have a low negative correlation
# The correlation coefficient is -0.4212043 and p-value of < 2.2e-16.

# Getting the formula for linear regression
odometer_on_price <- lm (price_usd ~ odometer_value, data = cars_edited)
odometer_on_price

# Scatter plot: Odometer and price with linear regression line
ggplot(cars_edited, aes(x=odometer_value, y=price_usd)) + geom_point() + stat_smooth(method=lm)

# Finding how well this line fits our data
summary(odometer_on_price)

confint(odometer_on_price)
sigma(odometer_on_price)*100/mean(cars_edited$price_usd)

###################################################################################################
#
# (Reid Hoffmeier)
#
# 4) Scatter Plot, Simple Regression Analysis:
# Q: Does the number of photos a vehicle has impact the selling price?
#
# A: A low positive correlation between number of photos a vehicle has and the selling price.
# However, on its own photo amount is not a good predictor of price and for that reason we must use several more attributes when predicting price.
#

# Scatter plot: Number of photos and price
ggplot(cars_edited, aes( x =number_of_photos, y=price_usd)) + geom_hex() + stat_smooth(color = "red")

# Getting the cor value
cor.test(cars_edited$number_of_photos, cars_edited$price_usd)

# Since the p-value is less than 0.05 we can conclude that Price and Number of photos have a low positive correlation
# The correlation coefficient is 0.3168586 and p-value of < 2.2e-16.

# Getting the formula for linear regression
number_of_photos_on_price <- lm (price_usd ~ number_of_photos, data = cars_edited)
number_of_photos_on_price

# Scatter plot: Number of photos and price with linear regression line
ggplot (cars_edited, aes(x=number_of_photos, y=price_usd)) + geom_point() + stat_smooth(method=lm)

# Finding how well this line fits the data
summary(number_of_photos_on_price)
confint(number_of_photos_on_price)
sigma(number_of_photos_on_price)*100/mean(cars_edited$price_usd)

###################################################################################################
#
# (Matthew Lane)
#
# 5) Scatter Plot, Simple Regression Analysis:
# Q: Does the number of times a vehicle has been upped in the catalog to raise its position impact the selling price?
#
# A: The number of times a vehicle has been upped has a negligible impact on the selling price
#

#Regression analysis
ggplot(cars_edited, aes( x =up_counter, y=price_usd)) + geom_hex() + stat_smooth(color = "red")

#Correlation
cor.test(cars_edited$up_counter, cars_edited$price_usd)
up_counter_on_price <- lm (price_usd ~ up_counter, data = cars_edited)
up_counter_on_price

#Finding how well this line fits the data
summary(up_counter_on_price)

confint(up_counter_on_price)
sigma(up_counter_on_price)*100/mean(cars_edited$price_usd)

#Scatter plot: up counter and price with regression line
ggplot (cars_edited, aes(x=up_counter, y=price_usd)) + geom_point() + stat_smooth(method=lm)

###################################################################################################
#
# (Matthew Lane)
# 6) Mosaic Plot/ Chi-Squared Test, Two-Way ANOVA:
#
# Q: Relationship between Engine Type and Body Type?
# A: Sedan and Gasoline is the most common followed by Gasoline and Hatchback
#
# Q: What is the impact of Engine Type and Body Type on the selling price?
# A: Limousine and pickup trucks appear to have the only impact.

# Balloon Plot
ggplot(cars_edited, aes(body_type, engine_type)) + geom_count()

#Chi-Square Test
engine_body.data <- table(cars_edited$body_type, cars_edited$engine_type)
chisq.test(engine_body.data)

# Table
prop.table(engine_body.data)*100

#Aov2
body_engine_type_on_price.aov <- aov(price_usd ~ engine_type * body_type, data = cars_edited)
summary(body_engine_type_on_price.aov)

#Tukey HSD
TukeyHSD(body_engine_type_on_price.aov)

###################################################################################################
#
# (Mikhail Mikhaylov)
#
# 7) Dplyr count with group_by, One-Way Anova:
# Q: What is the most popular model?
# A: Most popular is Passat.
#
# Q: Can we conclude that the popularity of a model has a direct impact on the price of a vehicle?
# A: The popularity of a vehicle does seem to have an impact on the average_price of a vehicle
# More attributes would be needed to predict price.
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

confint(modelPrice)
sigma(modelPrice)*100/mean(cars_edited$price_usd)

###################################################################################################
#
# (Mikhail Mikhaylov)
# 8) Scatter plot, Two-Way ANOVA/ :
#
# Q: What is the average age of each vehicle manufacturer?
# A: Tibble included below
#
# Q: Does the manufacturer change how the production year impacts the selling price? 
# A: The manufacturer does influence how production year changes the price of a vehicle.
#

#Scatter plot: Year produced by price and colored by manufacturer name
ggplot(cars_edited, aes(x = year_produced, y = price_usd)) + geom_hex() + facet_wrap(~ manufacturer_name)

#Group cars by manufacturer name
manufacturer_year <- group_by(cars_edited, manufacturer_name)

#Summarize the manufacturer years average
manufacturer_year_averages <- summarise(manufacturer_year, average = mean(year_produced, na.rm = TRUE))
# 1) Average age of each vehicle manufacturer
View(manufacturer_year_averages)

# Do an anova test to see if year produced significantly impacts price of a vehicle
manufacturer_price <- aov(price_usd ~ manufacturer_name * year_produced, data = cars_edited)
summary(manufacturer_price)
# The manufacturer does change how the production year affects the selling price

#Taking the linear regression
ManufyearPrice <- lm (price_usd ~ manufacturer_name + year_produced, data = cars_edited)

#Making sure the linear regression line matches the model
summary(ManufyearPrice)

confint(ManufyearPrice)
sigma(ManufyearPrice)*100/mean(cars_edited$price_usd)

###################################################################################################
#
# Predicting Exchangeability
# We will see if we can predict exchangeability given all the other attributes
#
###################################################################################################
## Using Decision Tree to create an exploratory model for exchangeability
model_DT_Exchangeable <-  train(is_exchangeable ~ . -model_name, data = train.data, method = "rpart",
                                trControl = trainControl("cv",number = 10),
                                preProcess = c("center","scale"),
                                tuneLength = 10)
# Exploratory
predictionsDTExploratory <- predict(model_DT_Exchangeable, train.data)

# Check accuracy, error, and confusion matrix
accuracy <- mean(train.data$is_exchangeable == predictionsDTExploratory)
accuracy
# [1] 0.6939802
error <- mean(train.data$is_exchangeable != predictionsDTExploratory)
error
# [1] 0.3060198
confusionMatrix(train.data$is_exchangeable, predictionsDTExploratory)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE  TRUE
# FALSE 17998  1948
# TRUE   7482  3387
# 
# Accuracy : 0.694           
# 95% CI : (0.6888, 0.6991)
# No Information Rate : 0.8269          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.242           
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.7064          
#             Specificity : 0.6349          
#          Pos Pred Value : 0.9023          
#          Neg Pred Value : 0.3116          
#              Prevalence : 0.8269          
#          Detection Rate : 0.5841          
#    Detection Prevalence : 0.6473          
#       Balanced Accuracy : 0.6706          
#                                           
#        'Positive' Class : FALSE 
#[1] 0.6939802

# Compute roc
predictionsDTExploratoryProb <- predict(model_DT_Exchangeable, train.data, type = "prob")
res.roc <- roc(train.data$is_exchangeable ~ predictionsDTExploratoryProb[,2])
plot.roc(res.roc, print.auc = TRUE)
as.numeric(res.roc$auc)
# [1] 0.6571428

# Get the probability threshold for specificity = 0.5
rocModelDT.data <- data_frame(
  thresholds = res.roc$thresholds,
  sensitivity = res.roc$sensitivities,
  specificity = res.roc$specificities
)
rocModelDT.data %>% filter(specificity >= 0.5)
# thresholds sensitivity specificity
# 1   0.3269864  0.64302144   0.5724456
# 2   0.3324625  0.46554421   0.7691266
# 3   0.3373490  0.46029994   0.7747919
# 4   0.3405653  0.44143895   0.7947959
# 5   0.3463600  0.42395805   0.8131455
# 6   0.3568692  0.41908179   0.8180588
# 7   0.3869302  0.41567762   0.8213176
# 8   0.4180577  0.31631245   0.8988770
# 9   0.4972896  0.31162020   0.9023363
# 10  0.5722631  0.29542736   0.9090043
# 11  0.5936936  0.17085288   0.9591898
# 12  0.6293493  0.15907627   0.9632508
# 13  0.6481293  0.15033582   0.9658578
# 14  0.6665233  0.12282639   0.9739296
# 15  0.7002933  0.07167173   0.9868645
# 16  0.8158251  0.01370871   0.9992981
# 17        Inf  0.00000000   1.0000000
plot.roc(res.roc, print.auc = TRUE, print.thres = "best")

# Predictive Model
predictionsDT <- predict(model_DT_Exchangeable, test.data)

# Check accuracy, error, and confusion matrix
accuracy <- mean(test.data$is_exchangeable == predictionsDT)
accuracy
# [1] 0.6871661
error <- mean(test.data$is_exchangeable != predictionsDT)
error
# [1] 0.3128339
confusionMatrix(test.data$is_exchangeable, predictionsDT)
#Confusion Matrix and Statistics
#
#Reference
#Prediction FALSE TRUE
#FALSE  4475  519
#TRUE   1882  799
#
#Accuracy : 0.6872          
#95% CI : (0.6767, 0.6975)
#No Information Rate : 0.8283          
#P-Value [Acc > NIR] : 1               
#
#Kappa : 0.22            
#
#Mcnemar's Test P-Value : <2e-16          
#                                          
#            Sensitivity : 0.7039          
#            Specificity : 0.6062          
#         Pos Pred Value : 0.8961          
#         Neg Pred Value : 0.2980          
#             Prevalence : 0.8283          
#         Detection Rate : 0.5831          
#   Detection Prevalence : 0.6507          
#      Balanced Accuracy : 0.6551          
#                                          
#       'Positive' Class : FALSE  	   
#Compute roc
predictionsDTProb <- predict(model_DT_Exchangeable, test.data, type = "prob")
res.roc <- roc(test.data$is_exchangeable ~ predictionsDTProb[,2])
plot.roc(res.roc, print.auc = TRUE)
as.numeric(res.roc$auc)
# [1] 0.6525616

# Get the probability threshold for specificity = 0.5
rocModelDT.data <- data_frame(
  thresholds = res.roc$thresholds,
  sensitivity = res.roc$sensitivities,
  specificity = res.roc$specificities
)
rocModelDT.data %>% filter(specificity >= 0.5)
#   thresholds sensitivity specificity
#1   0.3269864  0.63707572   0.5826992
#2   0.3324625  0.45729206   0.7715258
#3   0.3373490  0.45057814   0.7773328
#4   0.3405653  0.43043640   0.7933520
#5   0.3463600  0.41253264   0.8151782
#6   0.3568692  0.40917568   0.8189828
#7   0.3869302  0.40693771   0.8201842
#8   0.4180577  0.30249907   0.8936724
#9   0.4972896  0.29802313   0.8960753
#10  0.5722631  0.28161134   0.9048859
#11  0.5936936  0.17157777   0.9607529
#12  0.6293493  0.16113391   0.9649579
#13  0.6481293  0.15143603   0.9669604
#14  0.6665233  0.12719135   0.9771726
#15  0.7002933  0.07198806   0.9883861
#16  0.8158251  0.01342783   0.9985983
#17        Inf  0.00000000   1.0000000					  
plot.roc(res.roc, print.auc = TRUE, print.thres = "best")

## Using Logistic Regression to create an exploratory model for exchangeability
model_LR_Exchangeable <-  train( is_exchangeable ~ . -model_name, data = train.data, method = "glm", family = "binomial",
                                 trControl = trainControl("cv", number =10),
                                 preProcess = c("center", "scale"),
                                 tuneLength = 10
)

# Exploratory
predictionsLRExploratory <- predict(model_LR_Exchangeable, train.data)

# Check accuracy, error, and confusion matrix
accuracy <- mean(train.data$is_exchangeable == predictionsLRExploratory)
accuracy
# [1] 0.6821029		  
error <- mean(train.data$is_exchangeable != predictionsLRExploratory)
error
# [1] 0.3178971	  
confusionMatrix(train.data$is_exchangeable, predictionsLRExploratory)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE  TRUE
# FALSE 18402  1544
# TRUE   8252  2617
# 
# Accuracy : 0.6821          
# 95% CI : (0.6769, 0.6873)
# No Information Rate : 0.865           
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.1901          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.6904          
#             Specificity : 0.6289          
#          Pos Pred Value : 0.9226          
#          Neg Pred Value : 0.2408          
#              Prevalence : 0.8650          
#          Detection Rate : 0.5972          
#    Detection Prevalence : 0.6473          
#       Balanced Accuracy : 0.6597          
#                                           
#        'Positive' Class : FALSE         
# Compute roc
predictionsLRExploratoryProb <- predict(model_LR_Exchangeable, train.data, type = "prob")
res.roc <- roc(train.data$is_exchangeable ~ predictionsLRExploratoryProb[,2])
plot.roc(res.roc, print.auc = TRUE)
as.numeric(res.roc$auc)
#[1] 0.6505955

# Get the probability threshold for specificity = 0.5
rocModelLRExploratory.data <- data_frame(
  thresholds = res.roc$thresholds,
  sensitivity = res.roc$sensitivities,
  specificity = res.roc$specificities
)
rocModelLRExploratory.data %>% filter(specificity >= 0.5)
#thresholds sensitivity specificity
#1    0.3214292   0.7111050   0.5000000
#2    0.3214353   0.7111050   0.5000501
#3    0.3214399   0.7110130   0.5000501
#4    0.3214500   0.7110130   0.5001003
#5    0.3214600   0.7109210   0.5001003
#6    0.3214625   0.7108290   0.5001003
#7    0.3214728   0.7107370   0.5001003
#8    0.3214831   0.7107370   0.5001504
#9    0.3214855   0.7107370   0.5002005
#10   0.3214925   0.7106450   0.5002005
#11   0.3214998   0.7105529   0.5002005
#12   0.3215054   0.7105529   0.5002507
#13   0.3215302   0.7105529   0.5003008
#14   0.3215519   0.7105529   0.5003509
#15   0.3215612   0.7105529   0.5004011
#16   0.3215725   0.7105529   0.5005014
#17   0.3215810   0.7105529   0.5005515
# Many More ommitted
plot.roc(res.roc, print.auc = TRUE, print.thres = "best")

# Predictive
predictionsLR <- predict(model_LR_Exchangeable, test.data)
# Check accuracy, error, and confusion matrix
accuracy <- mean(test.data$is_exchangeable == predictionsLR)
accuracy
# [1] 0.6650163
error <- mean(test.data$is_exchangeable != predictionsLR)
error
# [1] 0.3349837
confusionMatrix(test.data$is_exchangeable, predictionsLR)
#Confusion Matrix and Statistics
#
#Reference
#Prediction FALSE TRUE
#FALSE  4707  287
#TRUE   2284  397
#
#Accuracy : 0.665           
#95% CI : (0.6543, 0.6756)
#No Information Rate : 0.9109          
#P-Value [Acc > NIR] : 1               
#
#Kappa : 0.1095          
#
#Mcnemar's Test P-Value : <2e-16          
#                                          
#            Sensitivity : 0.6733          
#            Specificity : 0.5804          
#         Pos Pred Value : 0.9425          
#         Neg Pred Value : 0.1481          
#             Prevalence : 0.9109          
#         Detection Rate : 0.6133          
#   Detection Prevalence : 0.6507          
#      Balanced Accuracy : 0.6269          
#                                          
#       'Positive' Class : FALSE           
#                                     
#Compute roc
predictionsLRProb <- predict(model_LR_Exchangeable, test.data, type = "prob")
res.rocLR <- roc(test.data$is_exchangeable ~ predictionsLRProb[,2])
plot.roc(res.rocLR, print.auc = TRUE)
as.numeric(res.rocLR$auc)
# [1] 0.6480223

# Get the probability threshold for specificity = 0.5
rocModelLR.data <- data_frame(
  thresholds = res.rocLR$thresholds,
  sensitivity = res.rocLR$sensitivities,
  specificity = res.rocLR$specificities
)
rocModelLR.data %>% filter(specificity >= 0.5)
# thresholds sensitivity specificity
# 1    0.3124277   0.6904140   0.5000000
# 2    0.3124336   0.6904140   0.5002002
# 3    0.3124397   0.6900410   0.5002002
# 4    0.3124927   0.6900410   0.5004005
# 5    0.3125795   0.6896680   0.5004005
# 6    0.3127208   0.6892950   0.5004005
# 7    0.3128345   0.6889220   0.5004005
# 8    0.3128493   0.6889220   0.5006007
# 9    0.3128604   0.6889220   0.5008010
# 10   0.3129094   0.6889220   0.5010012
# 11   0.3129564   0.6889220   0.5012014
# 12   0.3129951   0.6885490   0.5012014
# 13   0.3130347   0.6885490   0.5014017
# 14   0.3130436   0.6885490   0.5016019
# 15   0.3130594   0.6881761   0.5016019
# 16   0.3130773   0.6881761   0.5018022
# 17   0.3130941   0.6881761   0.5020024
# More ommitted
plot.roc(res.rocLR, print.auc = TRUE, print.thres = "best")

###################################################################################################
#
# (Everyone) Goal:
# Gain insights into which variables have the largest impact on selling price of a vehicle.
# Create a predictive model based on these insights to create a predictive model.
#
###################################################################################################

## Multiple Linear Regression Models
# R^2 for test/train dataset
LM <- lm(price_usd ~ . -model_name, data = train.data)

step.LM <- LM %>% stepAIC(trace = FALSE)
vif(step.LM)
# GVIF Df GVIF^(1/(2*Df))
# manufacturer_name 15.288862 54        1.025573
# transmission       1.840427  1        1.356623
# color              1.545952 11        1.019999
# odometer_value     1.627875  1        1.275882
# year_produced      2.136749  1        1.461762
# engine_fuel        1.691724  5        1.053981
# engine_capacity    2.224866  1        1.491598
# body_type          8.701242 11        1.103337
# drivetrain         6.095660  2        1.571286
# location_region    1.099706  5        1.009550
# number_of_photos   1.113954  1        1.055440
# duration_listed    1.023732  1        1.011797
summary(step.LM)
coef(step.LM)
confint(step.LM)
LMPrediction <- predict(step.LM, test.data)

# Prediction error, rmse
RMSE(LMPrediction,test.data$price_usd)
#[1] 3542.066

# Compute R-square
R2(LMPrediction,test.data$price_usd)
# [1] 0.7058649

# Log transformation
LogLMConts <- lm(log(price_usd) ~ . -model_name
                 , data = train.data)

step.logConts <- LogLMConts %>% stepAIC(trace = FALSE)
vif(step.logConts)

summary(step.logConts)
coef(step.logConts)
confint(step.logConts)

# Predict using Multiple Linear Regression Model
LogLMPrediction <- step.LogLM %>% predict(test.data)

# Prediction error, rmse
RMSE(LogLMContsPrediction,test.data$price_usd)

# Compute R-square
R2(LogLMContsPrediction,test.data$price_usd)

# Log transformation is less accurate
###################################################################################################
# SVR Models
#
# Create SVR Model using Linear Method
#

modelSVRLinTrain <- train( price_usd ~ . -model_name, data = train.data, method = "svmLinear",
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

modelSVRPolyTrain <- train(price_usd ~ . -model_name, data = train.data, method = "svmPoly",
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
modelSVRRadialTrain <- train(price_usd ~ . -model_name, data = train.data, method = "svmRadial",
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
#
# Decision Tree Regression Model
#

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
# See model_DT_Train$finalModel.txt                          

# Make predictions on the test data
prediction_DT_Train <- model_DT_Train %>% predict(test.data)

# Prediction error, rmse
RMSE(prediction_DT_Train,test.data$price_usd)
#[1] 3245.413    

# Compute R-square
R2(prediction_DT_Train,test.data$price_usd)
#[1] 0.7529956              

###################################################################################################
#
# Random Forest Model
#

random_forest_ranger <- train(price_usd ~ . -model_name,
                              data = train.data,
                              method = "ranger",
                              trControl = trainControl("cv", number = 10),
                              preProcess = c("center","scale"),
                              tuneLength = 10
)

summary(random_forest_ranger)
#
#                          Length Class         Mode     
#predictions               30815  -none-        numeric  
#num.trees                     1  -none-        numeric  
#num.independent.variables     1  -none-        numeric  
#mtry                          1  -none-        numeric  
#min.node.size                 1  -none-        numeric  
#prediction.error              1  -none-        numeric  
#forest                        7  ranger.forest list     
#splitrule                     1  -none-        character
#num.random.splits             1  -none-        numeric  
#treetype                      1  -none-        character
#r.squared                     1  -none-        numeric  
#call                          9  -none-        call     
#importance.mode               1  -none-        character
#num.samples                   1  -none-        numeric  
#replace                       1  -none-        logical  
#xNames                       98  -none-        character
#problemType                   1  -none-        character
#tuneValue                     3  data.frame    list     
#obsLevels                     1  -none-        logical  
#param                         0  -none-        list
#
random_forest_ranger$bestTune
#   mtry  splitrule min.node.size
#20   98 extratrees             5
plot(random_forest_ranger)
print(random_forest_ranger)
#Random Forest
#
#30815 samples
#16 predictor
#
#Pre-processing: centered (98), scaled (98) 
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 27733, 27733, 27732, 27735, 27734, 27734, ...
#Resampling results across tuning parameters:
#  
#  mtry  splitrule   RMSE      Rsquared   MAE     
#2    variance    4243.263  0.7369980  2793.194
#2    extratrees  4819.240  0.6049541  3292.147
#12    variance    2121.529  0.8976088  1256.828
#12    extratrees  2517.810  0.8610071  1525.127
#23    variance    1939.954  0.9100209  1133.567
#23    extratrees  2114.654  0.8965483  1237.008
#34    variance    1899.804  0.9125608  1105.994
#34    extratrees  1978.951  0.9072004  1142.945
#44    variance    1893.489  0.9127624  1099.677
#44    extratrees  1922.414  0.9114298  1104.583
#55    variance    1892.166  0.9126866  1096.447
#55    extratrees  1887.598  0.9139901  1084.031
#66    variance    1896.973  0.9121727  1098.183
#66    extratrees  1871.296  0.9151156  1074.076
#76    variance    1906.764  0.9111840  1102.032
#76    extratrees  1862.667  0.9156676  1069.721
#87    variance    1916.485  0.9102418  1106.084
#87    extratrees  1856.855  0.9160557  1066.920
#98    variance    1933.863  0.9085671  1111.908
#98    extratrees  1856.234  0.9160062  1066.489
#
#Tuning parameter 'min.node.size' was held constant at a value of 5
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were mtry = 98, splitrule = extratrees 
#and min.node.size = 5.
random_forest_ranger$finalModel
#Ranger result
#
#Call:
#  ranger::ranger(dependent.variable.name = ".outcome", data = x,      mtry = min(param$mtry, ncol(x)), min.node.size = param$min.node.size,      splitrule = as.character(param$splitrule), write.forest = TRUE,      probability = classProbs, ...) 
#
#Type:                             Regression 
#Number of trees:                  500 
#Sample size:                      30815 
#Number of independent variables:  98 
#Mtry:                             98 
#Target node size:                 5 
#Variable importance mode:         none 
#Splitrule:                        extratrees 
#Number of random splits:          1 
#OOB prediction error (MSE):       3405480 
#R squared (OOB):                  0.9167914 
#
# Make predictions on the test data
rf_predict_ranger <- predict(random_forest_ranger, test.data)

# Prediction error, rmse
RMSE(rf_predict_ranger,test.data$price_usd)
#[1] 1946.668

# Compute R-square
R2(rf_predict_ranger,test.data$price_usd)
#[1] 0.9117607

###################################################################################################
#
# KNN Model
#

model_knn <- train(
  price_usd ~. -model_name, data = train.data, method = "knn",
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