knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(tidytuesdayR)
library(forcats)
library(scales)
library(viridis)
library(ggplot2)

tt <- tt_load('2020-09-22')
expeditions <- tt$expeditions

peaks <- expeditions %>%
  mutate(peak_lumped = fct_lump(peak_name, n = 15, other_level = "Other")) %>%
  filter(peak_lumped != "Other") %>%
  group_by(peak_lumped, season) %>%
  summarise(expedition_count = n())

ggplot(peaks, aes(x = expedition_count,  y = fct_reorder(peak_lumped, expedition_count, .fun = sum), fill=season)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_d() +
  labs(title =  "The 15 most popular peaks stacked by season of expedition",
       y = "Peak Name",
       x = "Expedition Count",
       fill = "Season"
       )
