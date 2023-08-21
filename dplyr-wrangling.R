#------------Section 1: Filter------------#

library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

# look for an exact match: ==

penguins_biscoe <- penguins %>% filter(island == "Biscoe")

penguins_2007 <- penguins %>% filter(year == 2007)

adelie_torgersen <- penguins %>% filter(species == "Adelie" & island == "Torgersen")
# alternative: penguins %>% filter(species == "Adelie", island == "Torgersen)

# Create a subset from penguins that only contains Gentoo penguins observed in 2008

gentoo_2008 <- penguins %>% filter(year == 2008, species == "Gentoo")

# Create a subset that contains Gentoos and Adelies

gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

# Create a subset that contains observations where the island is Dream OR the year is 2009

dream_or_2009 <- penguins %>% filter(island == "Dream" | year == 2009)

# make a ggplot chart of water temperature versus crab size:

ggplot(pie_crab, aes(x = water_temp, y = size)) +
  geom_point()

# keep observations for sites NIB, ZI, DB, JC
# we can use the %in% operator to ask: does the value in our column match ANY of the values IN this vector

pie_sites <- pie_crab %>% filter(site %in% c("NIB", "ZI", "DB", "JC"))

# create a subset using the %in% operator that includes sites PIE, ZI, NIB, BB, and CC

sites <- c("PIE", "ZI", "NIB", "BB", "CC")
pie_sites2 <- pie_crab %>% filter(site %in% sites)

# Excluding filter statements
# != (asks is this NOT equal to that value)?

exclude_zi <- pie_crab %>% filter(site != "ZI")

# exclude sites "BB", "CC", "PIE

exclude_bb_cc_pie <- pie_crab %>% filter(!site %in% c("BB", "CC", "PIE"))

# create a subset from pie_crab that only contains observations from NIB, CC, and ZI for crabs with carapace size exceeding 13

crabs_large <- pie_crab %>% filter(site %in% c("NIB", "CC", "ZI") & size > 13)

#------------Section 2: Selecting columns------------#

# select individual columns by name, separate them by a comma
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

# select range of columns using
crabs_subset2 <- pie_crab %>% select(site:air_temp)

# select a range and in individual column
crabs_subset3 <- pie_crab %>% select(date:water_temp, name)

pie_crab %>% select(name, water_temp, size)

#------------Section 3: Mutate------------#

# use dplyr::mutate() to add or update a column, while keeping all existing columns

crabs_cm <- pie_crab %>% mutate(size_cm = size / 10)

# what happens if I use mutate to add a new column containing the mean of the size column

crabs_mean <- pie_crab %>% mutate(mean_mm = mean(size, na.rm = TRUE))

crabs_awesome <- pie_crab %>% mutate(tito = "Tito is awesome")

# reminder: group_by + summarize
mean_size_by_site <- pie_crab %>%
  group_by(site) %>%
  summarize(mean_size = mean(size, na.rm = TRUE),
            sd_size = sd(size, na.rm = TRUE))

# what about group_by then mutate?
group_mutate <- pie_crab %>%
  group_by(site) %>%
  mutate(mean_size = mean(size, na.rm = TRUE))

# What if I want to create a new column in pie_crab that contains "giant" if the size is greater than 35, or "not giant" if the size is less than or equal to 20?

# Use dplyr::case_when() to write if-else statements more easily

crabs_bin <- pie_crab %>%
  mutate(size_binned = case_when(
    size > 20 ~ "giant", size < 20 ~ "not giant"
  ))

sites_bin <- pie_crab %>%
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "High"
    ))






