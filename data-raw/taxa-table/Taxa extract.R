library(rfishbase)
library(tidyverse)
##Load fishbase data to store taxa information for all species
fishbase_table <- rfishbase::load_taxa(server="fishbase") %>%
  as.data.frame() %>% 
  dplyr::select(Genus, Family, Order, Class) %>% 
  distinct(Genus, .keep_all = TRUE) %>% 
  mutate(Class = recode(Class,
                        "Elasmobranchii" = "Chondrichthyes",
                        "Actinopteri" = "Actinopterygii")) %>% 
  mutate(Phylum = "Chordata",
         Kingdom = "Animalia") %>% 
  drop_na(Family)

##clean taxa
##Sealifebase
sealife_table = rfishbase::load_taxa(server="sealifebase") %>% #recent update on how to load sealifebase in package
  dplyr::select(Genus, Family, Order, Class, Phylum, Kingdom) %>% 
  distinct(Genus, .keep_all = TRUE)

##add non-animal taxonomic data extracted from AFCD 
non_animal_taxa <- read.csv(
  file.path("data-raw","raw","afcd_taxonomy_plants.csv"),
  header=TRUE
) %>%
  rename(kingdom = X) %>% 
  select(genus,family,order,class, phylum, kingdom)

##add additional taxa from WORMS
additional_taxa_WORMS <- read_csv("data-raw/raw/additional_taxa_WORMS.csv") %>% 
  separate(sciname_new, c("genus", "spp"), " ", remove=FALSE) %>%
  rename(family = Family,
         order = Order,
         class = Class,
         phylum = Phylum,
         kingdom = Kingdom) %>% 
  select(genus,family,order,class, phylum, kingdom)

##bind all data together
taxa_table = rbind(fishbase_table, sealife_table) %>% 
  distinct(Genus, .keep_all = TRUE) %>% 
  rename(family = Family,
         genus = Genus,
         order = Order,
         class = Class,
         phylum = Phylum,
         kingdom = Kingdom) %>% 
  rbind(additional_taxa_WORMS) %>% 
  mutate(family = tolower(family),
         genus = tolower(genus),
         order = tolower(order),
         class = tolower(class),
         phylum = tolower(phylum),
         kingdom = tolower(kingdom)) %>%
  rbind(non_animal_taxa) %>% #add in correclty formated non-animaltaxa from AFCD
  distinct() %>% 
  drop_na(genus) %>% 
  mutate(order = recode(order,
                        "perciformes/scorpaenoidei" = "perciformes",
                        "carangaria/misc" = "carangaria",
                        "eupercaria/misc" = "eupercaria",
                        "ovalentaria/misc" = "ovalentaria",
                        "perciformes/cottoidei" = "perciformes",
                        "perciformes/gasterosteoidei" = "perciformes",
                        "perciformes/zoarcoidei" = "perciformes",
                        "perciformes/uranoscopoidei" = "perciformes",
                        "perciformes/serranoidei" = "perciformes",
                        "perciformes/percoidei" = "perciformes",
                        "perciformes/notothenioidei" = "perciformes",
                        "perciformes/bembropoidei" = "perciformes",
                        "perciformes/percophoidei" = "perciformes"),
         genus = recode(genus, 
                        "protomyst/des" = "protomyst")) %>% 
  mutate_all(~na_if(., "not assigned"))

saveRDS(taxa_table, "data-raw/taxa-table/taxa_table.Rds")
