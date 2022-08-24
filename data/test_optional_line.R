library(tidyverse)
library(svglite)
library(maps)
library(usmap)
library(rgdal)

test_optional <- as.data.frame(test_optional)

before_covid <- subset(test_optional, year != 2020)

year_count_bc <- before_covid %>%
  group_by(year) %>%
  count(year)

ggplot(data=year_count_bc, aes(x=year,y=n)) +
  geom_line() +
  geom_point()




