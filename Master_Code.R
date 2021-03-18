library(tidyverse)
vehicles <- read_csv("/home/mikhail/Documents/Database/Project/vehicles.csv")
View(vehicles)
vehicles$Date <- as.Date(vehicles$posting_date)
vehicles$Time <- format(vehicles$posting_date,"%H:%M:%S")
# Remove "X1","id","url", "region"_,"url", "VIN", size", "Lat" , "Lon", "posting_date"
vehicles_edited <- select(vehicles, -1:-3 & -5 & -16 & -18 & -21 & -24:-25 & -26) #-3
View(vehicles_edited)
View(vehicles_edited %>% count(posting_date))
View(vehicles_edited %>% count(size))
colSums(is.na(vehicles_edited))
colnames(vehicles)
colnames(vehicles_edited)
View(vehicles_edited %>% count(manufacturer))
vehicles_edited
row_number(vehicles_edited$Date)
ggplot(vehicles_edited) + geom_bar(aes(manufacturer, fill = state))

