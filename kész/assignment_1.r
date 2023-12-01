knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readr)
library(dplyr)

# 1. Read the data
url <- "https://raw.githubusercontent.com/nthun/cocktail-balance/master/cocktail_data.tsv"
cocktails_original <- read_tsv(url)

## 2. Transform the data table and clean the ingredient variable
cocktails <- cocktails_original %>%
  separate_rows(ingredients, sep = "<br/>") %>%
  separate_rows(ingredients, sep = "<br>") %>%
  separate_rows(ingredients, sep = "<b4/>")
    
# Cleaning the ingredients by removing quantities and alcohol content
cocktails$ingredients <- cocktails$ingredients %>%
  str_remove_all("[\\d\\s\\.]+/?\\d*\\s*(dash(es)?|drop(s)?|oz|bps|bsp)") %>%  # Remove quantities like 2 dashes
  str_remove_all("\\(\\d+\\.?\\d*%\\s*abv\\)") %>%  # Remove alcohol content like 47.3% abv
  str_trim()  # Trim leading and trailing whitespace

## 3. All ingredients in alphabetical order
sorted_ingredients <- cocktails$ingredients %>%
  unique() %>%
  sort()

## 4. Number of unique ingredients
no_ingredients <- length(sorted_ingredients)

## 5. What are the top 10 ingredients?
top_10_ingredients <- cocktails %>%
  count(ingredients) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

## 6. Which cocktail(s) has/have the most ingredients?
ingredient_counts_per_cocktail <- cocktails %>%
  group_by(name) %>%
  summarise(no_of_ingredients = n())

max_ingredients <- max(ingredient_counts_per_cocktail$no_of_ingredients)

cocktails_with_most_ingredients <- ingredient_counts_per_cocktail %>%
  filter(no_of_ingredients == max_ingredients)

## 7. How many ingredients appear in only one cocktail (rare ingredient)?
ingredients_in_cocktails <- cocktails %>%
  group_by(ingredients) %>%
  summarise(no_of_cocktails = n_distinct(name))

rare_ingredients <- ingredients_in_cocktails %>%
  filter(no_of_cocktails == 1)

## 8. Which cocktail has an ingredient that is only used in one cocktail?
cocktails_using_rare_ingredient <- cocktails %>%
  filter(ingredients %in% rare_ingredients$ingredients) %>%
  select(name)

## 9. What are the cocktails without rare ingredients?
cocktails_without_rare_ingredient <- cocktails %>%
  filter(!(name %in% cocktails_using_rare_ingredient$name)) %>%
  select(name) %>%
  distinct()

## 10. Create a cheat sheet for the bartender!
bartender_cheat_sheet <- cocktails %>%
  select(name, ingredients) %>%
  distinct(name, ingredients) %>%
  mutate(presence = "X") %>%
  pivot_wider(names_from = ingredients, values_from = presence, values_fill = list(Value = ""))

