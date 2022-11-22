library(tidyverse)
library(dplyr)
library(ggplot2)

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_trends)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

# The total jail population in US
total_jail_population <- sum(incarceration_trends$total_jail_pop, na.rm = TRUE)

# The total black jail population in US
total_black_jail_population <- sum(incarceration_trends$black_jail_pop, na.rm = TRUE)

# The total female jail population in US
total_female_jail_population <- sum(incarceration_trends$female_jail_pop, na.rm = TRUE)

# The total male jail population in US
total_male_jail_population <- sum(incarceration_trends$male_jail_pop, na.rm = TRUE)

# The state with the highest ratio of Black jail population count
# to White jail population count
state_highest_jail_race_ratio <- incarceration_trends %>% 
  select(state, white_jail_pop, black_jail_pop) %>% 
  group_by(state) %>% 
  summarise(white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
            black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
  mutate(rate = black_jail_pop/white_jail_pop) %>% 
  filter(rate == max(rate, na.rm = TRUE)) %>% 
  pull(state)

# The state with the highest total incarceration count of Black, and the
# highest count of Black
state_highest_black_incarceration <- incarceration_trends %>% 
  select(state, black_jail_pop) %>% 
  replace(is.na(.), 0) %>% 
  mutate(sum = black_jail_pop) %>% 
  group_by(state) %>% 
  summarise(total_incarceration = sum(sum, na.rm = TRUE)) %>% 
  filter(total_incarceration == max(total_incarceration, na.rm = TRUE)) %>% 
  pull(state)

# The state with the highest ratio of Female jail population count
# to Male jail population count
state_highest_jail_sex_ratio <- incarceration_trends %>% 
  select(state, female_jail_pop, male_jail_pop) %>% 
  group_by(state) %>% 
  summarise(female_jail_pop = sum(female_jail_pop, na.rm = TRUE),
            male_jail_pop = sum(male_jail_pop, na.rm = TRUE)) %>% 
  mutate(rate = male_jail_pop/female_jail_pop) %>% 
  filter(rate == max(rate, na.rm = TRUE)) %>% 
  pull(state)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

get_year_jail_pop <- function() {
  year_jail_pop <- incarceration_trends %>% 
    group_by(year) %>% 
    summarise(jail_pop_in_US = sum(total_jail_pop, na.rm =  TRUE))
  return(year_jail_pop)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(data)  {
  # TODO: Implement this function 
  # require(ggplot2)
  chart1 <- ggplot(data = get_year_jail_pop(), aes(x = year, y = jail_pop_in_US)) +
    geom_bar(stat = "identity") + 
    xlab("Year") + 
    ylab("Total Jail Population") +
    ylim(0, 800000) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return (chart1)
}

data <- get_year_jail_pop()
plot_jail_pop_for_us(data)

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
states <- c("CA", "WA", "FL", "DC")

get_state_jail_pop <- function() {
  state_jail_pop <- incarceration_trends %>%
    drop_na(total_jail_pop) %>%
    select(year, state, total_jail_pop) %>%
    filter(state %in% c("CA", "WA", "FL", "DC")) %>%
    group_by(year, state) %>% 
    summarise(selected_pop = sum(total_jail_pop, na.rm =  TRUE))
  return(state_jail_pop)   
}

plot_jail_pop_for_us <- function(states)  {
  chart2 <- ggplot(data = get_state_jail_pop(), 
                             aes(x = year, y = selected_pop, group = state)) +
                             geom_line(aes(color = state)) +
                             xlab("Year") +
                             scale_y_continuous("Total Jail Population", labels = scales::comma) +
                             #ylab("Total Jail Population" +
                             ggtitle("Growth of U.S. Jail Population By State (1970-2018)") 
  return (chart2)
}

plot_jail_pop_for_us(states)


# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here

get_female_pop_by_state <- function() {
  female_data <- incarceration_trends %>% 
    group_by(year, state) %>% 
    summarise(female_pop = sum(female_jail_pop, na.rm = TRUE))
  return(female_data)
}

plot_female_pop_by_state <- function() {
  chart3 <- ggplot(data = get_female_pop_by_state(), 
                                aes(x = year, y = female_pop, group = state)) +
    geom_line(aes(color = state)) +
    xlim(1970, 2018) +
    xlab("Year") +
    ylab("Female Incarcerated Population") +
    ggtitle("Female Incarcerated Population by State (1970-2018)")

  return(chart3)
}

plot_female_pop_by_state()

# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here 
library("usmap")
library("openintro")

female_jail_pop_state <- incarceration_trends %>% 
  group_by(state) %>% 
  summarise(female_pop = sum(female_jail_pop, na.rm =  TRUE))

plot_usmap(data = female_jail_pop_state, values = "female_pop", color = "black") + 
           scale_fill_continuous(low = "white", high = "red", 
                                 name = "State Black Jail Population (1970-2018)", 
                                 label = scales::comma) + 
                                 theme(legend.position = "right")

# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


