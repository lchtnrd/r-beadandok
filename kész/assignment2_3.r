knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)

tt <- tt_load('2019-11-05')
commute <- tt$commute

walking_data <- commute %>%
  filter(mode == "Walk") %>%
  group_by(state_abb, state_region) %>%
  summarise(total_walking = sum(n))

biking_data <- commute %>%
filter(mode == "Bike") %>%
  group_by(state_abb, state_region) %>%
  summarise(total_biking = sum(n))

commute_aggregated <- merge(walking_data, biking_data, by = "state_abb")

ggplot(commute_aggregated, aes(x = total_walking, y = total_biking)) +
  geom_point(size=2, aes(color = state_region.x)) +
  geom_text(aes(label=state_abb)) +
  scale_x_log10(labels = comma_format()) +
  scale_y_log10(labels = comma_format()) +
  theme_light() + 
  labs(x = "Number of ppl walking to work (log N)",
       y = "Number of ppl biking to work (log N)",
       title = "Number of people walking vs. biking to work in each USA state",
       color = "State region")
