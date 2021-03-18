library(tidyverse)
vehicles <- read_csv("/home/mikhail/Documents/Database/Project/vehicles.csv")
View(vehicles)
vehicles$Date <- as.Date(vehicles$posting_date)
vehicles$Time <- format(vehicles$posting_date,"%H:%M:%S")
vehicles_edited <- select(vehicles, -2 & -5 & -16 & -18 & -21 & -24:-25 & -26) #-3
View(vehicles_edited)
View(vehicles_edited %>% count(posting_date))
View(vehicles_edited %>% count(size))
colSums(is.na(vehicles_edited))
colnames(vehicles)
colnames(vehicles_edited)
View(vehicles_edited %>% count(manufacturer))
vehicles_edited

