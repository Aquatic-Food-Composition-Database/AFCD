library(rfishbase)
library(tidyverse)
##Load fishbase data to store taxa information for all species
family_info <- rfishbase::load_taxa(server="https://fishbase.ropensci.org") %>%
  as.data.frame() %>% 
  dplyr::select(Genus, Family, Order, Class) %>% 
  distinct(Genus, .keep_all = TRUE) %>% 
  mutate(Class = recode(Class,
                        "Elasmobranchii" = "Chondrichthyes"))

##clean taxa

##Seaflife
seaflife_table = rfishbase::sealifebase %>% 
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
         class = tolower(class))

saveRDS(taxa_table, "data-raw/taxa-table/taxa_table.Rds")