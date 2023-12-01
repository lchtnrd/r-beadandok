library(tidyverse)
library(tidytuesdayR)
library(ggplot2)

phd_by_field <- tidytuesdayR::tt_load("2019-02-19")$phd_by_field
agg_data <- phd_by_field %>%
  group_by(broad_field, year) %>%
  summarise(total_phds = sum(n_phds))
ggplot(agg_data, aes(x = year, y = total_phds, color = broad_field)) +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "PhDs Awarded by Year and Field",
       x = "Year",
       y = "Total PhDs",
       color = "Broad Field") +
  theme_minimal()

library(tidyverse)
library(tidytuesdayR)
library(ggplot2)

phd_by_field <- tidytuesdayR::tt_load("2019-02-19")$phd_by_field
agg_data <- phd_by_field %>%
  group_by(broad_field, year) %>%
  summarise(total_phds = sum(n_phds))

ggplot(agg_data, aes(x = year, y = total_phds, color = broad_field)) +
  geom_line(linewidth = 1.2, na.rm = FALSE) +  # Add na.rm = TRUE /false?? to remove missing values
  geom_point(size = 3, na.rm = TRUE) +  # Show points at data points
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "PhDs Awarded by Year and Field",
       x = "Year",
       y = "Total PhDs",
       color = "Broad Field") +
  theme_minimal()
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)

phd_by_field <- tidytuesdayR::tt_load("2019-02-19")$phd_by_field

# Ensure there are no missing combinations of broad_field and year
agg_data <- phd_by_field %>%
  group_by(broad_field, year) %>%
  summarise(total_phds = sum(n_phds)) %>%
  complete(broad_field, year, fill = list(total_phds = 0)) %>%
  fill(total_phds, .direction = "downup")

ggplot(agg_data, aes(x = year, y = total_phds, color = broad_field)) +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "PhDs Awarded by Year and Field",
       x = "Year",
       y = "Total PhDs",
       color = "Broad Field") +
  theme_minimal()

