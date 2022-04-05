
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data-raw/raw"
outdir <- "data-raw/processed"

# Read data
afcd <- readRDS(file=file.path(outdir, "AFCD_data_taxa.Rds"))
afcd_wide <- readRDS(file=file.path(outdir, "AFCD_data_taxa_wide.Rds"))
afcd_sci <- readRDS(file=file.path(outdir, "AFCD_data_sci.Rds"))
afcd_common <- readRDS(file=file.path(outdir, "AFCD_data_comm.Rds"))
afcd_refs <- readRDS(file=file.path(outdir, "AFCD_reference_key.Rds"))
afcd_nutrients <- readRDS(file=file.path(outdir, "AFCD_nutrient_key.Rds"))
taxa_table = readRDS("data-raw/taxa-table/taxa_table.Rds")

##Parts key
afcd_parts = afcd %>% 
  group_by(food_part) %>% 
  count()

##Parts key
afcd_prep = afcd %>% 
  group_by(food_prep) %>% 
  count()

# Export data
usethis::use_data(afcd, overwrite = T)
usethis::use_data(afcd_wide, overwrite = T)
usethis::use_data(afcd_sci, overwrite = T)
usethis::use_data(afcd_common, overwrite = T)
usethis::use_data(afcd_refs, overwrite = T)
usethis::use_data(afcd_nutrients, overwrite = T)
usethis::use_data(afcd_parts, overwrite = T)
usethis::use_data(afcd_prep, overwrite = T)
usethis::use_data(taxa_table, overwrite = T)

##To update functions
#library(roxygen2); # Read in the roxygen2 R package
#roxygenise();      # Builds the help files