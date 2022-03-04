library(rfishbase)
library(tidyverse)
##Load fishbase data to store taxa information for all species
fishbase_table <- rfishbase::load_taxa(server="fishbase") %>%
  as.data.frame() %>% 
  dplyr::select(Genus, Family, Order, Class) %>% 
  distinct(Genus, .keep_all = TRUE) %>% 
  mutate(Class = recode(Class,
                        "Elasmobranchii" = "Chondrichthyes",
                        "Actinopteri" = "Actinopterygii"))

##clean taxa

##Sealifebase
sealife_table = rfishbase::load_taxa(server="sealifebase") %>% #recent update on how to load sealifebase in package
  dplyr::select(Genus, Family, Order, Class) %>% 
  distinct(Genus, .keep_all = TRUE)

##add non-animal taxonomic data extracted from AFCD 
non_animal_taxa <- read.csv(
  file.path("data-raw","raw","afcd_taxonomy_plants.csv"),
  header=TRUE
) %>%
  select(genus,family,order,class)

##bind all data together
taxa_table = rbind(fishbase_table, sealife_table) %>% 
  distinct(Genus, .keep_all = TRUE) %>% 
  rename(family = Family,
         genus = Genus,
         order = Order,
         class = Class) %>% 
  mutate(family = tolower(family),
         genus = tolower(genus),
         order = tolower(order),
         class = tolower(class)) %>%
  rbind(non_animal_taxa) %>% #add in correclty formated non-animaltaxa from AFCD
  distinct()

saveRDS(taxa_table, "data-raw/taxa-table/taxa_table.Rds")