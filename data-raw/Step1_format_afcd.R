
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
data_orig <- read.csv(file.path(indir, "20210914_AFCD.csv"), na.strings = c("", "NA"))

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
         region=study_region,
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

# Merge reference key
ref_key <- bind_rows(ref_peer, ref_fct) %>%
  arrange(study_type, study_id) %>%
  select(study_type, study_id, citation, everything())

# Inspect
freeR::complete(ref_key)

# Export
saveRDS(ref_key, file.path(outdir, "AFCD_reference_key.Rds"))


# Step 1. Rename columns and go from wide to long
################################################################################

# Format data
data1 <- data_orig %>%
  # Rename columns
  janitor::clean_names() %>%
  rename(sciname=taxa_name,
         food_part=parts_of_food,
         food_prep=preparation_of_food,
         prod_catg=production_category,
         edible_prop=edible_portion_coefficient,
         study_id=study_id_number,
         iso3=country_iso3,
         fao3=fao_3a_code,
         fct_code_orig=original_fct_food_code,
         food_id=food_item_id,
         food_name=food_name_in_english,
         food_name_orig=food_name_in_original_language) %>%
  # Arrange
  select(sciname:food_name_orig, food_id, notes, everything()) %>%
  # Gather nutrients (maintain capitalization)
  gather(key="nutrient_orig", value="value", 23:ncol(.)) %>%
  mutate(nutrient_orig=stringr::str_to_sentence(nutrient_orig)) %>%
  # Reduce to rows with data
  filter(!is.na(value))

# Inspect
freeR::complete(data1)


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
write.csv(nutr_key_orig, file.path(indir, "AFCD_nutrient_key_work.csv"), row.names = F)


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
  mutate(iso3=stringr::str_trim(iso3),
         iso3=ifelse(is.na(iso3), "Not provided in unformatted AFCD", iso3),
         iso3=recode(iso3,
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
  mutate(country=countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  mutate(country=ifelse(is.na(country), iso3, country),
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
         study_type, study_id, peer_review, iso3, country, fao3,
         prod_catg, food_part, food_prep, food_name, food_name_orig, fct_code_orig, food_id, edible_prop, notes,
         nutrient_type, nutrient, nutrient_orig, nutrient_desc, nutrient_code_fao, nutrient_units, value, everything()) %>%
  # Remove unimportant columns
  select(-c(peer_review))

# Inspect scinames with "includes"
data2 %>%
  filter(grepl(pattern="includes|Includes", x=taxa_name)) %>% pull(taxa_name) %>% unique() %>% sort()

# Step 4. Inspect data
################################################################################

# Inspect
# str(data2)
freeR::complete(data2)

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
sort(unique(data2$iso3))
sort(unique(data2$country))
cntry_key <- data2 %>%
  group_by(iso3, country) %>%
  summarize(n=n())

# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "AFCD_data_pass1.Rds"))


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

# Inspect
freeR::complete(nutr_key)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot sample size: fatty acids
g1 <- ggplot(nutr_key %>% filter(nutrient_type=="Fatty acid"), aes(y=reorder(nutrient,n), x=n)) +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of observations", y="") +
  # Theme
  theme_bw() + my_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "AFCD_nutrient_sample_size_fatty_acids.pdf"),
       width=8.5, height=11, units="in", dpi=600)

# Plot sample size: fatty acids
g2 <- ggplot(nutr_key %>% filter(nutrient_type!="Fatty acid"), aes(y=reorder(nutrient,n), x=n)) +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of observations", y="") +
  # Theme
  theme_bw() + my_theme
g2

# Export
ggsave(g2, filename=file.path(plotdir, "AFCD_nutrient_sample_size_non_fatty_acids.pdf"),
       width=8.5, height=11, units="in", dpi=600)




