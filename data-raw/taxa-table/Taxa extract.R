library(rfishbase)
library(tidyverse)
##Load fishbase data to store taxa information for all species
fish_taxa <- read_csv("~/Fisheries Nutrition Modeling/data/fish_taxa.csv")

family_info = fish_taxa %>% 
  dplyr::select(Genus, Family, Order, Class) %>% 
  distinct(Genus, .keep_all = TRUE)

##Seaflife
seaflife_table = sealifebase %>% 
  dplyr::select(Genus, Family, Order, Class) %>% 
  distinct(Genus, .keep_all = TRUE)

taxa_table = rbind(family_info, seaflife_table) %>% 
  distinct(Genus, .keep_all = TRUE) %>% 
  rename(family = Family,
         genus = Genus,
         order = Order,
         class = Class) %>% 
  mutate(family = tolower(family),
         genus = tolower(genus),
         order = tolower(order),
         class = tolower(class),
         class = recode(class, 
                        "elasmobranchii" = "chondrichthyes"))

saveRDS(taxa_table, "data-raw/taxa-table/taxa_table.Rds")

