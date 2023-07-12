
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)


# Directories
indir <- "data-raw/raw"
outdir <- "data-raw/processed"
plotdir <- "data-raw/figures"

# Resources
# GitHub: https://github.com/zachkoehn/aquatic_foods_nutrient_database
# DataVerse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KI0NYM
# Nature: https://www.nature.com/articles/s41586-021-03917-1?proof=t%2Btarget%3D#data-availability


# Read data
data_orig <- read.csv(file.path(indir, "20230612_AFCD.csv"), na.strings = c("", "NA"))

# Read reference key
ref_fct_orig <- readxl::read_excel(file.path(indir, "afcd_references.xlsx"), sheet="fct_references")
ref_peer_orig <- readxl::read_excel(file.path(indir, "afcd_references.xlsx"), sheet="peer_review_references")

# Read column key
col_key_orig <- readxl::read_excel(file.path(indir, "afcd_variable_codex.xlsx"))


# Build reference key
################################################################################

# Format FCT reference key
ref_fct <- ref_fct_orig %>%
  # Rename
  janitor::clean_names() %>%
  rename(study_id=study_id_number,
         doi=link_to_dataset,
         database=nutrient_database,
         units=nutrients_per) %>%
  # Add study type
  mutate(study_type="Food Composition Table (FCT)") %>%
  # Arrange
  select(study_type, study_id, citation, doi, database, units, everything()) %>%
  # remove old versions of FCTs as new versions are published
  filter(
    study_id != "USA_USDA_2019"
  ) %>%
  # Remove useless columns
  select(-c(notes, added_by, already_included, format))

# Inspect
colnames(ref_fct)
table(ref_fct$units)

# Format peer reviewed reference key
ref_peer <- ref_peer_orig %>%
  # Rename
  janitor::clean_names() %>%
  rename(study_id=study_id_number,
         doi=study_doi,
         country_origin_study=study_region,
         citation=study_apa_citation) %>%
  # Add study type
  mutate(study_type="Peer-reviewed literature") %>%
  # Convert study id
  mutate(study_id=as.character(study_id)) %>%
  # Arrange
  select(study_type, study_id, everything()) %>%
  # Remove useless columns
  select(-x5)

# Inspect
colnames(ref_peer)
table(ref_peer$region)

# Merge reference key & extract year 
ref_key <- bind_rows(ref_peer, ref_fct) %>%
  arrange(study_type, study_id) %>%
  # extract year
  mutate(study_year=str_extract(citation, "\\(\\d{4}\\)"),
         study_year=case_when(is.na(study_year) ~ str_extract(citation, "(2\\d{3}|19\\d{2})"), TRUE ~ study_year),
         study_year=gsub('(\\(|\\))', "", study_year)
  ) %>%
  select(study_type, study_id, citation, everything()) 

# Inspect
# freeR::complete(ref_key)

# Export
saveRDS(ref_key, file.path(outdir, "AFCD_reference_key.Rds"))


# Step 1. Rename columns and go from wide to long
################################################################################

##Combine some columns
dta = data_orig %>% 
  mutate(energy_total_combined = if_else(is.na(Energy_total_metabolizable_calculated_from_the_energy_producing_food_components_original_as_from_source_kcal), 
                                         Energy_total_metabolizable_calculated_from_the_energy_producing_food_components_original_as_from_source_kj/4.184,
                                         Energy_total_metabolizable_calculated_from_the_energy_producing_food_components_original_as_from_source_kcal),
         protein_total_combined = if_else(is.na(Protein_total_calculated_from_total_nitrogen), 
                                          Protein_total_method_of_determination_unknown_or_variable,
                                          Protein_total_calculated_from_protein_nitrogen_est),
         nitrogen_total_combined = if_else(is.na(Nitrogen_total),
                                           Nitrogen_nonprotein,
                                           Nitrogen_total),
         nitrogen_total_combined = if_else(is.na(nitrogen_total_combined), 
                                           Nitrogen_protein, 
                                           nitrogen_total_combined),
         vitamin_a_combined = if_else(is.na(Vitamin_a_retinol_activity_equivalent_rae_calculated_by_summation_of_the_vitamin_a_activities_of_retinol_and_the_active_carotenoids),
                                      Retinol, 
                                      Vitamin_a_retinol_activity_equivalent_rae_calculated_by_summation_of_the_vitamin_a_activities_of_retinol_and_the_active_carotenoids),
         vitamin_a_combined = if_else(is.na(vitamin_a_combined), 
                                      0.3*Vitamin_a_international_units_iu_sum_of_carotenoids_usda_indicates_over_estimates_bioavailability,
                                      vitamin_a_combined),
         DHA = if_else(is.na(Fatty_acid_20_5), Fatty_acid_20_5_n3, Fatty_acid_20_5),
         DHA = if_else(is.na(DHA), Fatty_acid_20_5_cis_n3, DHA),
         EPA = if_else(is.na(Fatty_acid_22_6), Fatty_acid_22_6_n3, Fatty_acid_22_6),
         EPA = if_else(is.na(EPA), Fatty_acid_22_6_cis_n3, EPA),
         ALA = if_else(is.na(Fatty_acid_18_3), Fatty_acid_18_3_n3, Fatty_acid_18_3),
         ALA = if_else(is.na(ALA), Fatty_acid_18_3_cis_n3, ALA),
         DHA_EPA = if_else(is.na(EPA), DHA, EPA+DHA),
         DHA_EPA = if_else(is.na(DHA_EPA), EPA, DHA_EPA),
         Country_origin_sample=ifelse(is.na(Country_origin_sample),Country_iso3,Country_origin_sample)
         ) %>%
select(
  Taxa_name,Kingdom,Class,Order,Family,Genus,Taxa_id,Parts_of_food,Preparation_of_food,
  Production_category,Edible_portion_coefficient,Study_id_number,Country_origin_sample,Country_origin_study,Peer_review,Phylum,
  Taxa_db,Country_iso3,Original_fct_food_code,Food_name_in_english,Food_name_in_original_language,
  everything()
)
  

# Format data
data1 <- dta %>%
  # Rename columns
  janitor::clean_names() %>%
  rename(sciname=taxa_name,
         food_part=parts_of_food,
         food_prep=preparation_of_food,
         prod_catg=production_category,
         edible_prop=edible_portion_coefficient,
         study_id=study_id_number,
         fct_code_orig=original_fct_food_code,
         food_name=food_name_in_english,
         food_name_orig=food_name_in_original_language
         ) %>%
  # Arrange
  select(sciname:food_name_orig,country_origin_sample,country_origin_study, notes, everything()) %>%
  # Gather nutrients (maintain capitalization)
  gather(key="nutrient_orig", value="value", 23:ncol(.)) %>%
  mutate(nutrient_orig=stringr::str_to_sentence(nutrient_orig)) %>%
  # Reduce to rows with no data and remove old versions of FCTs
  filter(
    !is.na(value),
    study_id != "USA_USDA_2019"
    )

# Inspect
# freeR::complete(data1)


# Step 2. Build nutrient key
################################################################################

# Build column key
col_key <- col_key_orig %>%
  # Rename
  janitor::clean_names() %>%
  rename(col_id=x1, col_name=afcd_variable_name, units=unit, fao_code=fao_tagname_if_applicable)

# Build nutrient key
nutr_col_key <- col_key %>%
  # Simplify
  select(-col_id) %>%
  # Reduce to nutrients
  filter(units!="none" | is.na(units)) %>%
  # Rename
  rename(nutrient_orig=col_name) %>%
  # Arrange
  select(nutrient_orig, units, fao_code, description) %>%
  unique()

# Identify nutrients in data
nutr_key_orig <- data1 %>%
  # Identify nutrients in dataset
  select(nutrient_orig) %>%
  unique() %>%
  arrange(nutrient_orig) %>%
  # Add known meta-data from column key
  left_join(nutr_col_key, by="nutrient_orig") %>%
  # Format nutrient name
  mutate(nutrient=nutrient_orig %>% gsub("_", " ", .)) %>%
  # Arrange
  select(nutrient_orig, nutrient, units, description, everything())

# Export for formatting outside R
# write.csv(nutr_key_orig, file.path(indir, "AFCD_nutrient_key_work.csv"), row.names = F)


# Step 3. Format data
################################################################################

# Read formatted key
nutr_key_use <- readxl::read_excel(file.path(indir, "AFCD_nutrient_key_work.xlsx"), na="NA")

# Format data some more
data2 <- data1 %>%
  # Format scientific name
  mutate(sciname=stringr::str_to_sentence(sciname),
         sciname=stringr::str_trim(sciname)) %>%
  mutate(sciname=recode(sciname,
                        "Can"="Cancer spp."),
         sciname=ifelse(sciname=="Etc.", NA, sciname)) %>%
  # Format other taxonomic info
  mutate(across(.cols=kingdom:genus, .fns=stringr::str_to_title),
         across(.cols=kingdom:genus, .fns=stringr::str_trim)) %>%
  # Format ETC in genus
  mutate(genus=ifelse(toupper(genus)=="ETC.", NA, genus)) %>%
  # Format taxa database
  mutate(taxa_db=stringr::str_to_upper(taxa_db)) %>%
  # Format food parts
  mutate(food_part=gsub("_", " ",  food_part)) %>%
  # Format food preparation
  mutate(food_prep=gsub("_", " ",  food_prep)) %>%
  # Format production category
  mutate(prod_catg=gsub("_", " ",  prod_catg)) %>%
  # Add reference type
  mutate(study_id=ifelse(is.na(study_id), "Not provided in unformatted AFCD", study_id)) %>%
  left_join(ref_key %>% select(study_id, study_type), by=c("study_id")) %>%
  mutate(study_type=ifelse(is.na(study_type), "Id not in AFCD reference key", study_type)) %>%
  # Format I30
  mutate(country_origin_sample=stringr::str_trim(country_origin_sample),
         country_origin_sample=ifelse(is.na(country_origin_sample), "Not provided in unformatted AFCD", country_origin_sample),
         country_origin_sample=recode(country_origin_sample,
                     "SAu"="SAU",
                     "BNG"="IND", # West Bengal which is part of India - study 1407
                     "GRB"="GBR", # study 789 mis-recorded
                     "KHG"="ITA", # study 338 mis-recorded
                     "MYL"="MYS", # study 1438 mis-recorded
                     "PNDB"="Pacific Region",
                     "smiling_cambodia"="KHM",
                     "smiling_indonesia"="IDN",
                     "smiling_laos"="LAO",
                     "smiling_thailand"="THA",
                     "smiling_vietnam"="VNM",
                     "unknown (Caspian Sea)"="Caspian Sea",
                     "unknown"="Unknown",
                     "POL/ AUS"="POL, AUS",
                     "FAO.biodiv3"="FAO Biodiv 3",
                     "FAO.infoods.ufish1"="FAO INFOODS Ufish",
                     "FAO.infoods.west.africa"="FAO INFOODS West Africa",
                     "FAO.latinfoods"="FAO Latin Foods")) %>%
  # Add country
  mutate(country=countrycode::countrycode(country_origin_sample, "iso3c", "country.name")) %>%
  mutate(country=ifelse(is.na(country), country_origin_sample, country),
         country=recode(country,
                        "BGD, KHM"="Bangladesh, Cambodia",
                        "CHN, JPN, KOR"="China, Japan, South Korea",
                        "CHN, TWN"="China, Taiwan",
                        "KOR, CHN"="South Korea, China",
                        "FRA, GBR"="France, Great Britain",
                        "NOR, FRA, ISL"="Norway, France, Israel",
                        "POL, AUS"="Poland, Australia")) %>%
  # Add nutrients
  left_join(nutr_key_use, by=c("nutrient_orig")) %>%
  rename(nutrient_units=units, nutrient_desc=description, nutrient_code_fao=fao_code) %>%
   # Format nutrient units/description
  mutate(nutrient_units=ifelse(is.na(nutrient_units), "Not provided in unformatted AFCD", nutrient_units),
         nutrient_desc=ifelse(is.na(nutrient_desc), nutrient, nutrient_desc)) %>%
  # Fix up scientific names with "includes"
  mutate(sciname=recode(sciname,
                        "Includes a mix of species belonging to the astacidae"="Astacidae spp.",
                        "Includes a mix of species belonging to the ommastrephidae family"="Ommastrephidae spp.",
                        "Includes a mix of species belonging to the palaemonidae family"="Palaemonidae spp."),
         sciname=ifelse(grepl("includes", tolower(sciname)), NA, sciname),
         genus=ifelse(genus=="Includes", NA, genus)) %>%
  # Format scientific name
  mutate(sciname_source=ifelse(!is.na(sciname), "Provided",
                               ifelse(!is.na(genus), "Genus",
                                      ifelse(!is.na(family), "Family",
                                             ifelse(!is.na(order), "Order",
                                                    ifelse(!is.na(food_name), "Food name (English)", "Food name (original)")))))) %>%
  mutate(sciname=ifelse(sciname_source=="Provided", sciname,
                               ifelse(sciname_source=="Genus", genus,
                                      ifelse(sciname_source=="Family", family,
                                             ifelse(sciname_source=="Order", order,
                                                    ifelse(sciname_source=="Food name (English)", food_name, food_name_orig)))))) %>%
  # Rename scientific name columns
  rename(taxa_name=sciname, taxa_name_source=sciname_source) %>%
  # Arrange
  select(taxa_name, taxa_name_source, kingdom:taxa_db,
         study_type, study_id, peer_review, country_origin_sample, country,
         prod_catg, food_part, food_prep, food_name, food_name_orig, fct_code_orig, edible_prop, notes,
         nutrient_type, nutrient, nutrient_orig, nutrient_desc, nutrient_code_fao, nutrient_units, value, everything()) %>%
  # Remove unimportant columns
  select(-c(peer_review))

# Inspect scinames with "includes"
data2 %>%
  filter(grepl(pattern="includes|Includes", x=taxa_name)) %>% pull(taxa_name) %>% unique() %>% sort()


###Fix some wrong values
data2 = data2 %>% 
  mutate(value = case_when(study_id == "4" & nutrient == "Vitamin D; unknown/variable methods" ~ value/1000,
                           study_id == "459" & nutrient == "Vitamin D; unknown/variable methods" ~ value/100,
                           study_id == "732" & nutrient == "Vitamin D; unknown/variable methods" ~ 0.025*value/0.01675,
                           TRUE ~ value))

###Exclude studies with unrealistic values
data2 = data2 %>% 
  filter(!study_id == "465")


# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "AFCD_data_pass1.Rds"))


# Step 4. Inspect data
################################################################################

# Inspect
# str(data2)
# freeR::complete(data2)

# Inspect taxa
table(data2$kingdom)
sort(unique(data2$phylum))
sort(unique(data2$order))
sort(unique(data2$family))
sort(unique(data2$genus))
table(data2$taxa_db)

# Inspect food parts
table(data2$food_part)
table(data2$food_prep)
table(data2$prod_catg)

# Inspect edible proportions (should be 0-1)
range(data2$edible_prop, na.rm=T)

# Inspect nutrient units
table(data2$nutrient_units)

# Inspect study characteristics
sort(unique(data2$study_id))

# Check study type vs. peer review
# Add peer-review back in to make this work
# table(data2$peer_review)
# data2 %>%
#   group_by(study_type, peer_review) %>%
#   summarize(n=n())

# Study ids not in key
data2$study_id[!data2$study_id %in% ref_key$study_id] %>% unique() %>% sort()

# Study ids in key not in data
ref_key$study_id[!ref_key$study_id %in% data2$study_id] %>% unique() %>% sort()

# Inspect foods
sort(unique(data2$fct_code_orig))
sort(unique(data2$food_name)) # terrible
sort(unique(data2$food_name_orig)) # terrible

# Inspect countries
sort(unique(data2$country_origin_sample))
sort(unique(data2$country))
cntry_key <- data2 %>%
  group_by(country_origin_sample, country) %>%
  summarize(n=n())

# Nutrient key
################################################################################

# Build nutrient key
nutr_key <- data2 %>%
  # Summarize
  group_by(nutrient_type, nutrient, nutrient_units, nutrient_desc, nutrient_code_fao) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Remover
  filter(nutrient_type!="Non-nutrient")

# Export data
saveRDS(nutr_key, file=file.path(outdir, "AFCD_nutrient_key.Rds"))
write.csv(nutr_key,
          file = file.path(indir,"afcd_nutrient_key.csv"),
          row.names=FALSE
          )
# Inspect
# freeR::complete(nutr_key)
