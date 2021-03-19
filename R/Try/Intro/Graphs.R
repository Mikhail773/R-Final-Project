library(tidyverse)

data(mpg)
View(mpg)

mpg_tibble <- tibble(mpg)
View(mpg_tibble)
ggplot(mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(mpg) + geom_bar(mapping = aes(x = manufacturer))
ggplot(mpg) + geom_bar(mapping = aes(x = fl))
ggplot(mpg) + geom_bar(mapping = aes(x = trans))
ggplot(mpg) + geom_bar(mapping = aes(x = manufacturer, fill=class))
ggplot(mpg) + geom_bar(mapping = aes(x = manufacturer, fill=drv))
ggplot(mpg) + geom_point(mapping = aes(x = manufacturer, y = hwy,fill=class))
