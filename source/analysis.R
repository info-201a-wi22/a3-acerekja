library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(tidyverse)

raw_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Total black population incarcerated per state in 2018
blackjail_per_state <- raw_data %>%
  filter(year == '2018') %>%
  group_by(state) %>%
  summarize(black_in_prison = sum(black_jail_pop, na.rm = TRUE))

# Total black population from ages 15-64 per state in 2018
blackpop_per_state <- raw_data %>%
  filter(year == '2018') %>%
  group_by(state) %>%
  summarize(black_population_15to64 = sum(black_pop_15to64, na.rm = TRUE))

# Creating a data set to display the overall percentage of the black population
# from ages 15 to 64 that are incarcerated per state in 2018
black_percentage_per_state <- left_join(blackjail_per_state, blackpop_per_state)
black_percentage_per_state <- mutate(black_percentage_per_state, percentage_imprisoned = (black_in_prison / black_population_15to64) * 100)

# Finding the state with the highest percentage of their black population aged
# 15 to 64 that are incarcerated in 2018
black_highest_percentage <- black_percentage_per_state %>%
  filter(percentage_imprisoned == max(percentage_imprisoned)) %>%
  pull(state)
# Answer: Wyoming (WY)

# Total percentage of black population from ages 15-64 incarcerated in 2018
total_black_in_prison <- sum(black_percentage_per_state$black_in_prison)
total_black_population <- sum(black_percentage_per_state$black_population_15to64)
national_black_incarceration <- (total_black_in_prison / total_black_population) * 100
# Answer: 0.85%

# Percentage black population makes up of total population ages 15-64 in 2018
totalpop_per_state <- raw_data %>%
  filter(year == '2018') %>%
  summarize(total_population_15to64 = sum(total_pop_15to64, na.rm = TRUE))
national_blackpop_percentage <- (total_black_population / totalpop_per_state$total_population_15to64) * 100
# Answer: 13.48%

# Percentage black population makes up of total prison population in 2018
jailpop_per_state <- raw_data %>%
  filter(year == '2018') %>%
  summarize(total_jail_population = sum(total_jail_pop, na.rm = TRUE))
national_blackjail_percentage <- (total_black_in_prison / jailpop_per_state$total_jail_population) * 100
# Answer: 33.58%

# Total Asian and Pacific Islaner(aapi) population incarcerated per state in 2018
aapijail_per_state <- raw_data %>%
  filter(year == '2018') %>%
  group_by(state) %>%
  summarize(aapi_in_prison = sum(aapi_jail_pop, na.rm = TRUE))

# Total aapi population from ages 15-64 per state in 2018
aapipop_per_state <- raw_data %>%
  filter(year == '2018') %>%
  group_by(state) %>%
  summarize(aapi_population_15to64 = sum(aapi_pop_15to64, na.rm = TRUE))

# Creating a data set to display the overall percentage of the aapi population
# from ages 15 to 64 that are incarcerated per state in 2018
aapi_percentage_per_state <- left_join(aapijail_per_state, aapipop_per_state)
aapi_percentage_per_state <- mutate(aapi_percentage_per_state, percentage_imprisoned = (aapi_in_prison / aapi_population_15to64) * 100)

# Finding the state with the highest percentage of their aapi population aged
# 15 to 64 that are incarcerated in 2018
aapi_highest_percentage <- aapi_percentage_per_state %>%
  filter(percentage_imprisoned == max(percentage_imprisoned)) %>%
  pull(state)
# Answer: Utah (UT)

# Total percentage of aapi population from ages 15-64 incarcerated in 2018
total_aapi_in_prison <- sum(aapi_percentage_per_state$aapi_in_prison)
total_aapi_population <- sum(aapi_percentage_per_state$aapi_population_15to64)
national_aapi_incarceration <- (total_aapi_in_prison / total_aapi_population) * 100
# Answer: 0.049%

# Percentage aapi population makes up of total population ages 15-64 in 2018
national_aapipop_percentage <- (total_aapi_population / totalpop_per_state$total_population_15to64) * 100
# Answer: 6.76%

# Percentage aapi population makes up of total prison population in 2018
national_aapijail_percentage <- (total_aapi_in_prison / jailpop_per_state$total_jail_population) * 100
# Answer: 0.96%

# Trends Over Time Chart:
# This chart will show the trend of the number of Black people incarcerated vs. the
# number of AAPI people incarcerated in the United States from 2010-2018
black_incarceration_over_time <- raw_data %>%
  filter(year > 2009) %>%
  group_by(year) %>%
  summarize(black_in_jail = sum(black_jail_pop, na.rm = TRUE))
aapi_incarceration_over_time <- raw_data %>%
  filter(year > 2009) %>%
  group_by(year) %>%
  summarize(aapi_in_jail = sum(aapi_jail_pop, na.rm = TRUE))
black_aapi_over_time <- left_join(black_incarceration_over_time, aapi_incarceration_over_time)

black_aapi_annual <- ggplot(data = black_aapi_over_time) +
  geom_smooth(method = lm, se = FALSE, mapping = aes(year, black_in_jail, color = "Black")) +
  geom_smooth(method = lm, se = FALSE, mapping = aes(year, aapi_in_jail, color = "Asian/Pacific Islander")) +
  labs(title = "Black and AAPI Incarcerated in the United States (2010-2018)",
         x = "Year", y = "People Incarcerated", color = "Race")
# Chart concludes that over the shown time frame, there is a clear downwards trend
# in number of black people incarcerated while the number of aapi stays fairly constant.
  
# Variable Comparison Chart:
# This chart will compare the total number of people incarcerated to the number of
# black people incarcerated in the US from 2000-2018
black_jail_population <- raw_data %>%
  filter(year > 1999) %>%
  group_by(year) %>%
  summarize(black_population = sum(black_jail_pop, na.rm = TRUE))
total_jail_population <- raw_data %>%
  filter(year > 1999) %>%
  group_by(year) %>%
  summarize(total_population = sum(total_jail_pop, na.rm = TRUE))
jail_populations <- left_join(black_jail_population, total_jail_population)

black_vs_total <- ggplot(data = jail_populations, aes(total_population, black_population, label = year)) +
  geom_point() + geom_text() + 
  labs(title = "Black vs Total Incarcerations per year in the US (2000-2018)",
       x = "Total People Incarcerated", y = "Black People Incarcerated")
# Chart concludes that over the shown time frame, it begins with an upwards trend in
# black people being incarcerated, but within the last decade there has been a downwards
# trend

# Map:
# This map displays the mainland United States color-coded by county to show
# the number of black people incarcerated in 2018

black_2018 <- raw_data %>%
  filter(year == "2018") %>%
  select(year, fips, county_name, black_jail_pop) %>%
  mutate(black_per_state = sum(black_jail_pop, na.rm = TRUE))

county_data <- map_data("county") %>%
  mutate(polyname = paste(region, sep = ",", subregion))

placeholder <- left_join(county_data, county.fips)
black_data <- left_join(placeholder, black_2018)

blackpop_county_heatmap <- ggplot(black_data) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = black_jail_pop)) +
  labs(title = "Black Population in Jail by County (2018)", 
       x = "Longitude", y = "Latitude") +
  coord_quickmap()+ theme(legend.title = element_blank())
