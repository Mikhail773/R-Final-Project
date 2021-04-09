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

#Read our dataset into the cars variable
cars <- read_csv("cars.csv")

#check the na's in the dataset
colSums(is.na(cars))
View(cars) #view the data

###################################################################################################
#
# Edit(modify) the data
#

#Removing unnecessary columns from cars. Store that data in cars_edited
# -(19) is a column that shows the number of times a car has been upped. This column not descriptive and has been removed
# -(20:29) are boolean columns for various features. There is no description of what these features are and for that reason they have been omitted.
cars_edited <- select(cars, -8 & -(12:13) & -(20:29))
View(cars_edited) #view the data

#Recode foreign language into their English meaning (location_region)
cars_edited <-
  cars_edited %>% mutate(
    location_region = recode(
      cars_edited$location_region,
      'Брестская обл.' = "Brest Region",
      'Витебская обл.' = "Vitebsk Region",
      'Гомельская обл.' = "Gomel Region",
      'Гродненская обл.' = "Grodno Region",
      'Минская обл.'  = "Minsk Region",
      'Могилевская обл.' = "Mogilev Region"
    )
  )

#Recode foreign language into their English meaning (manufacturer_name)
cars_edited <-
  cars_edited %>% mutate(
    manufacturer_name = recode(
      cars_edited$manufacturer_name,
      'ВАЗ' = "AvtoVAZ",
      'ГАЗ' = "GAZ",
      'ЗАЗ' = "ZAZ",
      'Москвич' = "Moskvitch",
      'УАЗ'  = "UAZ"
    )
  )

#Recode foreign language into their English meaning (model_name)
cars_edited <-
  cars_edited %>% mutate(
    model_name = recode(
      cars_edited$model_name,
      'Таврия' = "Tavria",
      '968м' = "968M",
      'Соболь' = "Sobol",
      'Луидор' = "Luidor",
      'ВИС'  = "VIS"
    )
  )

#Recode foreign language into their English meaning (engine_fuel)
cars_edited <-
  cars_edited %>% mutate(engine_fuel = recode(cars_edited$engine_fuel, 'gas' = "gasoline", ))

#Check if there are na's in the cars_edited
colSums(is.na(cars_edited))

# Check for Duplicates and remove them
cars_edited <- cars_edited %>% distinct()

#view the changes the mutate made
View(cars_edited)

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
cars_edited_without_outliers <-  cars_edited_without_outliers[! cars_edited_without_outliers$odometer_value %in% my_list[[1]],]
cars_edited_without_outliers <-  cars_edited_without_outliers[! cars_edited_without_outliers$year_produced %in% my_list[[2]],]
cars_edited_without_outliers <-  cars_edited_without_outliers[! cars_edited_without_outliers$price_usd %in% my_list[[3]],]
cars_edited_without_outliers <-  cars_edited_without_outliers[! cars_edited_without_outliers$number_of_photos %in% my_list[[4]],]
cars_edited_without_outliers <-  cars_edited_without_outliers[! cars_edited_without_outliers$up_counter %in% my_list[[5]],]
cars_edited_without_outliers <-  cars_edited_without_outliers[! cars_edited_without_outliers$duration_listed %in% my_list[[6]],]
View(cars_edited_without_outliers)

#Summary of Attributes with Outliers
summary(Cars_continuous)

#Summary of Attributes_without Outliers
summary(cars_edited_without_outliers)

#Investigating Variables: Getting the unique entries and displaying how often they appear
ggplot(cars_edited, mapping = aes(y = manufacturer_name)) + geom_histogram(stat =
                                                                             "count") + geom_text(stat = "count", aes(label = after_stat(count)), hjust = -1)
View(cars_edited %>% count(model_name))
ggplot(cars_edited, mapping = aes(x = transmission)) + geom_bar(stat =
                                                                  "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
ggplot(cars_edited, mapping = aes(x = color)) + geom_bar(stat = "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
ggplot(cars_edited) + geom_boxplot(mapping = aes(odometer_value))
ggplot(cars_edited) + geom_histogram(mapping = aes(odometer_value))
View(cars_edited %>% count(year_produced))
ggplot(cars_edited) + geom_boxplot(mapping = aes(year_produced))
ggplot(cars_edited) + geom_histogram(mapping = aes(year_produced))
ggplot(cars_edited, aes(x = engine_fuel), stat = "count") + geom_bar(mapping = aes(fill = engine_type))  + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
ggplot(cars_edited, aes(x = engine_type), stat = "count") + geom_bar()  + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
View(cars_edited %>% count(engine_capacity))
ggplot(cars_edited) + geom_boxplot(mapping = aes(engine_capacity))
ggplot(cars_edited) + geom_bar(mapping = aes(engine_capacity))
ggplot(cars_edited,
       mapping = aes(x = body_type),
       stat = "count") + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
ggplot(cars_edited,
       mapping = aes(x = drivetrain),
       stat = "count") + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
View(cars_edited %>% count(price_usd))
ggplot(cars_edited) + geom_boxplot(mapping = aes(price_usd))
ggplot(cars_edited) + geom_histogram(mapping = aes(price_usd))
ggplot(cars_edited, mapping = aes(x = is_exchangeable)) + geom_bar(stat =
                                                                     "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
ggplot(cars_edited, mapping = aes(x = location_region)) + geom_bar(stat =
                                                                     "count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)
View(cars_edited %>% count(number_of_photos))
ggplot(cars_edited) + geom_boxplot(mapping = aes(number_of_photos))
ggplot(cars_edited) + geom_histogram(mapping = aes(number_of_photos))
View(cars_edited %>% count(up_counter))
ggplot(cars_edited) + geom_boxplot(mapping = aes(up_counter))
ggplot(cars_edited) + geom_histogram(mapping = aes(up_counter))
View(cars_edited %>% count(duration_listed))
ggplot(cars_edited) + geom_boxplot(mapping = aes(duration_listed))
ggplot(cars_edited) + geom_histogram(mapping = aes(duration_listed))

###################################################################################################
#
# Plotting the data
#
# (Emma Doyle) 1) Box Plots, Bar Graph, ANOVA?: What impact does a region have on price?
#
# (Emma Doyle) 2) Pie Graph/Box Plot, One-Way Anova?: What is the distribution of manufacturers and whether manufacturers have a significant impact on the asking price of a vehicle?
#
# (Reid Hoffmeier) 3) Scatter Plot/Box-Plot, Simple Regression Analysis: What is the relationship between odometer and price?
# 
# (Reid Hoffmeier) 4) Scatter Plot, Simple Regression Analysis: Does the number of photos a vehicle has impact the selling price?
#
# (Matthew Lane) 5) Scatter Plot, Simple Regression Analysis: Does the number of times a vehicle has been upped in the catalog to raise its position impact the selling price?
#
# (Matthew Lane) 6) Mosaic Plot/ Chi-Squared Test, Two-Way ANOVA: Relationship between Engine Type and Body Type? What is the impact of Engine Type and Body Type on the selling price?
#
# (Mikhail Mikhaylov) 7) Dplyr count with group_by, One-Way Anova: What is the most popular model and whether we can conclude that the popularity of a model has a direct impact on the price of a vehicle?
#
# (Mikhail Mikhaylov) 8) Bar graph, Two-Way ANOVA/ : What is the average age of each vehicle manufacturer and whether the manufacturer changes how the production year impacts the selling price?
#
# (Everyone) Goal:
# Gain insights into which variables have the largest impact on selling price of a vehicle.
# Create a predictive model based on these insights to create a predictive model.
#

#Summarize our dataset
summary(cars_edited)

#Summarize dataset without outliers
summary(cars_edited_without_outliers)

# 1) Graph to show the amount of cars(by manufacturer name) in a region BALLOON PLOT
ggplot(cars_edited, aes(location_region, manufacturer_name)) + geom_count()
ggplot(cars_edited_without_outliers, aes(location_region, manufacturer_name)) + geom_count()

# 2) Graph to show the price of a car according to its year produced SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point() + geom_smooth()
ggplot(cars_edited_without_outliers, aes(year_produced, price_usd)) + geom_point() + geom_smooth()

# 3) Graph to show the amount of cars(density) according to it's price LINE GRAPH
ggplot(cars_edited, aes(price_usd, ..density..)) + geom_freqpoly(binwidth = 500)
ggplot(cars_edited_without_outliers, aes(price_usd, ..density..)) + geom_freqpoly(binwidth = 500)

#Group cars by manufacturer, and get it's mean price
cars_edited %>% group_by(manufacturer_name) %>% summarize(mean(price_usd)) %>% View()


# 4) Graph to show the number of cars in specific colors(10 red cars, 8 blue cars etc.) by region BAR GRAPH
ggplot(cars_edited, aes(color)) + geom_bar(aes(fill = location_region))
ggplot(cars_edited_without_outliers, aes(color)) + geom_bar(aes(fill = location_region))

# 5) Graph to show the price of a car according to it's millage(odometer) SCATTER PLOT
ggplot(cars_edited, aes(odometer_value, price_usd)) + geom_point(aes(color = is_exchangeable)) + geom_smooth()
ggplot(cars_edited_without_outliers, aes(odometer_value, price_usd)) + geom_point(aes(color = is_exchangeable)) + geom_smooth()

# 6)Graph to show the price of a car according to it's year produced AND body type SCATTER PLOT
ggplot(cars_edited, aes(year_produced, price_usd)) + geom_point(aes(color = body_type)) + geom_smooth()
ggplot(cars_edited_without_outliers, aes(year_produced, price_usd)) + geom_point(aes(color = body_type)) + geom_smooth()

# 7) Graph to show the price of a car according to it's Odometer AND engine fuel SCATTER PLOT
ggplot(cars_edited, mapping = aes(x = odometer_value, y = price_usd)) + geom_point() + geom_smooth()
ggplot(cars_edited_without_outliers, mapping = aes(x = odometer_value, y = price_usd)) + geom_point() + geom_smooth()

# Group by car body type and get it's mean price
group_by(cars_edited, body_type) %>% summarise(price_mean = mean(price_usd)) -> mean_cars

# 8) Graph to show the outliers with body type and price BOX PLOT
ggplot(cars_edited) + geom_boxplot(mapping = aes(x = reorder(body_type, price_usd), y =
                                                   price_usd))
ggplot(cars_edited_without_outliers) + geom_boxplot(mapping = aes(x = reorder(body_type, price_usd), y =
                                                   price_usd))
# 9) Graph to show the correlation between car body type, price, AND engine fuel
ggplot(cars_edited) + geom_point(mapping = aes(x = body_type, y = price_usd, color = engine_fuel))
ggplot(cars_edited_without_outliers) + geom_point(mapping = aes(x = body_type, y = price_usd, color = engine_fuel))

# 10 ) Graph to show the price of a car according to it's number of photos incl. engine fuel SCATTER PLOT
ggplot(cars_edited) + geom_point(mapping = aes(x = number_of_photos, y = price_usd, color = engine_fuel))
ggplot(cars_edited_without_outliers) + geom_point(mapping = aes(x = number_of_photos, y = price_usd, color = engine_fuel))

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