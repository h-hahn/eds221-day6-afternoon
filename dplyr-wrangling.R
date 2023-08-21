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
