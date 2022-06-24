
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

outdir <- "data-raw/processed"
datadir <- "data"

# Load data 
load(file.path(datadir, "afcd.rda"))
afcd_ref <- readRDS(file.path(outdir, "AFCD_reference_key.Rds"))

afcd_edible = afcd %>%
  mutate(edible_value = value * edible_prop)

# get relevant outliers in groups of nutrients and units
# group by nutrient units as well since some nutrients could have different units
# ideally, they would all be uniform, but we have to convert them still 
nutrient_outliers =  afcd_edible %>%
  group_by(nutrient, nutrient_units) %>%
  rstatix::identify_outliers(edible_value) %>%
  select(study_id, nutrient, value, sciname, genus, family, order, class, phylum, kingdom, common_name) 

# add to this list of outlierss
study_outliers = nutrient_outliers %>%
  count(study_id) %>%
  merge(afcd_ref, by="study_id") %>%
  arrange(desc(n))
