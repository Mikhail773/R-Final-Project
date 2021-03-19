library(tidyverse)
library(nycflights13)
as_tribble(flights)
View(flights)
print(flights, n = 20, width = Inf)
flights[[1]]
flights$year
print(mtcars)
print(flights)

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]
flights$Usless <- flights$hour*60- flights$minute
Names <- c("Bob","Mikhail", "Sarah", "George")
name_hierarchy <- c("Mikhail", "George", "Sarah")
name_list <- factor(Names, levels = name_hierarchy)
name_list
count(flights$origin) #Doesn't work
flights %>% count(origin)
flights$origin <- as.factor( flights$origin)
is.factor(flights$origin)
ggplot(flights) + geom_bar(aes(origin))

variance <- function(x, na.rm = TRUE) {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  sq_err <- (x - m)^2
  sum(sq_err) / (n - 1)
}              
Skew <- function(x){
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  skew_num <- sum((x-m)^3)/(n-2)
  skew_den <- var(x)^(3/2)
  skew_num/skew_den
}
Skew(c(1, 2, 5, 100))

flights %>% count(origin, carrier) %>% ggplot(aes(origin, carrier)) + geom_tile(aes(fill = n))

