knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)

tt <- tt_load('2019-02-19')
phd_by_field <- tt$phd_by_field

agg_data <- phd_by_field %>% 
  group_by(broad_field, year) %>%
  summarise(total_phds = sum(n_phds, na.rm = TRUE))

ggplot(agg_data, aes(x = year, y = total_phds, color = broad_field, group = broad_field)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma_format()) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Number of awarded Ph.D.-s in the US by year",
       x = "Year",
       y = "Number of Ph.D.-s",
       color = "Broad field") +
  theme_minimal()