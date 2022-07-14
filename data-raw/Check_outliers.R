
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
  mutate(edible_value = value * edible_prop) %>%
  mutate(ID=row_number()) 
  

# get relevant outliers in groups of nutrients and units
# group by nutrient units as well since some nutrients could have different units
# ideally, they would all be uniform, but we have to convert them still 
nutrient_outliers =  afcd_edible %>%
  group_by(nutrient, nutrient_units) %>%
  rstatix::identify_outliers(edible_value) %>%
  select(ID, study_id, nutrient, nutrient_units, food_prep, prod_catg, food_part, value, sciname, genus, family, order, class, phylum, kingdom, common_name) 

# add to this list of outlierss
study_outliers = nutrient_outliers %>%
  count(study_id) %>%
  merge(afcd_ref, by="study_id") %>%
  arrange(desc(n))

# examine outliers by study ID 
# sample values 
Korea_8thRev_2011_outliers = nutrient_outliers %>%
  filter(study_id=="Korea_8thRev_2011")

FAO_WestAfrica_2019_outliers = nutrient_outliers %>%
  filter(study_id=="FAO_WestAfrica_2019")


# find missing taxa data without nutrients
afcd_taxa = afcd %>%
  select(c(sciname, genus, family, order, class, phylum, kingdom, nutrient))

afcd_taxa_unique = afcd_taxa %>%
  unique()

# number of unique taxa
as.data.frame(table(afcd_taxa_unique$class, afcd_taxa_unique$nutrient)) %>% 
  setNames(c(Var1="CLASS", Var2="NUTRIENT", Freq="count")) %>%
  arrange(count) %>%
  write.csv('CLASS.csv')


afcd_count = as.data.frame(table(afcd$nutrient))

