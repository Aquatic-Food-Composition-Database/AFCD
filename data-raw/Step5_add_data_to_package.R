
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
afcd1 <- readRDS(file=file.path(outdir, "AFCD_data_sci.Rds"))
afcd2 <- readRDS(file=file.path(outdir, "AFCD_data_comm.Rds"))
afcd_refs <- readRDS(file=file.path(outdir, "AFCD_reference_key.Rds"))
afcd_nutrients <- readRDS(file=file.path(outdir, "AFCD_nutrient_key.Rds"))


# Export data
usethis::use_data(afcd1, overwrite = T)
usethis::use_data(afcd2, overwrite = T)
usethis::use_data(afcd_refs, overwrite = T)
usethis::use_data(afcd_nutrients, overwrite = T)
