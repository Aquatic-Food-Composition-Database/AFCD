
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
data_orig <- readRDS(file.path(outdir, "AFCD_data_pass2.Rds"))

# Read ref key
ref_key <- readRDS(file.path(outdir, "AFCD_reference_key.Rds"))

# Prepare data: with taxonomy worked out
################################################################################

# Build initial data
data_sci1 <- data_orig %>%
  # Reduce to taxa without taxonomic information
  filter(!taxa_name_source %in% c("Food name (English)", "Food name (original)")) %>%
  # Rename
  rename(sciname=taxa_name, sciname_source=taxa_name_source)

# Identify taxa groups
orders <- sort(unique(data_sci1$order))
families <- sort(unique(data_sci1$family))
genera <- sort(unique(data_sci1$genus))

# Build species key
################################################################################

# Species key 1
spp_key1 <- data_sci1 %>%
  # Unique species
  select(sciname) %>%
  unique() %>%
  # Recode species
  rename(sciname_orig=sciname) %>%
  mutate(sciname=sciname_orig) %>%
  # Delete dangling commas
  mutate(sciname=gsub(",$|_$", "", sciname)) %>%
  # Delete ugly characters
  mutate(sciname=gsub("<c2><a0>|<ca>|<c3><8d>", "", sciname)) %>%
  # Delete synonyms in brackets
  mutate(sciname=gsub("\\s*\\[[^\\)]+\\]", "", sciname)) %>%
  # Replace semicolons with commas
  mutate(sciname=gsub(';', ",", sciname)) %>%
  # Replace underscore with commas
  mutate(sciname=gsub(" _ ", ", ", sciname)) %>%
  # Replace AND with comma
  mutate(sciname=gsub(" and ", ", ", sciname)) %>%
  # Add period to end of all SPPs
  mutate(sciname=gsub("spp.", "spp", sciname),
         sciname=gsub("spp", "spp.", sciname)) %>%
  # Add period to end of all trailing SPs
  mutate(sciname=gsub(" sp$", " sp.", sciname)) %>%
  # Fix a few 1 worders
  mutate(sciname=recode(sciname,
                        "Anadara<be>spp."="Anadara spp.",
                        "Anisotremusvirginicus"="Anisotremus virginicus",
                        "Balistescarolinensis"="Balistes carolinensis",
                        "Callinectessapidus"="Callinectes sapidus",
                        # "Callorhynchus"="",
                        # "Can"="",
                        "Cancerporteri"="Cancer porteri",
                        "Caulerpa<be>spp."="Caulerpa spp.",
                        "Cichlaspp."="Cichla spp.",
                        "Clupeasardina"="Clupea sardina",
                        "Diplodusannularis"="Diplodus annularis",
                        "Donaxvariabilis"="Donax variabilis",
                        "Durvilleaantarctica"="Durvillea antarctica",
                        "Eleutheronematetractylum"="Eleutheronema tetradactylum",
                        "Epinephulussexfasciatus"="Epinephelus fasciatus",
                        "Gelidiumpusillum"="Gelidium pusillum",
                        "Genypterusblancodes"="Genypterus blacodes",
                        "Homarusgammarus"="Homarus gammarus",
                        "Hoplosternumlittorale"="Hoplosternum littorale",
                        "Leporinusobtusidens"="Leporinus obtusidens",
                        "Mugilcephalus"="Mugil cephalus",
                        "Mylossomaspp."="Mylossoma spp.",
                        "Mytiluschilensis"="Mytilus chilensis",
                        "Mytilusedulis"="Mytilus edulis",
                        "Obliadamelanura"="Oblada melanura",
                        "Oncorhynchusmykiss"="Oncorhynchus mykiss",
                        # "Palaemonidae/penaeidae"="",
                        "Palinurusvulgaris"="Palinurus vulgaris",
                        "Pleuronectesflesus"="Pleuronectes flesus",
                        "Pseudoplatystomacoruscans"="Pseudoplatystoma corruscans",
                        "Rasbora,sp"="Rasbora sp.",
                        "Sander<c2><a0>lucioperca"="Sander lucioperca",
                        "Sardasarda"="Sarda sarda",
                        "Selenevomer"="Selene vomer",
                        "Seriolellaspecie"="Seriolella spp.",
                        "Taliepusdentatus"="Taliepus dentatus",
                        "Thunnusvulgaris"="Thunnus vulgaris",
                        "Trachuruspicturatus"="Trachurus picturatus",
                        "Ulvalactuca"="Ulva lactuca")) %>%
  # Add SPP to end of 1 word groups
  mutate(nwords=freeR::nwords(sciname)) %>%
  mutate(sciname=ifelse(nwords==1, paste(sciname, "spp."), sciname)) %>%
  select(-nwords) %>%
  # Remove blank
  filter(sciname!="" & !is.na(sciname)) %>%
  # Remove dangling letters
  mutate(sciname=gsub(" a\\.", "", sciname)) %>%
  mutate(sciname=gsub(" l\\.", "", sciname)) %>%
  mutate(sciname=gsub(" b\\.", "", sciname)) %>%
  mutate(sciname=gsub(" v\\.", "", sciname)) %>%
  mutate(sciname=gsub(" c\\.", "", sciname)) %>%
  mutate(sciname=gsub(" h\\.", "", sciname)) %>%
  # Fix ones with punctuation
  mutate(sciname=recode(sciname,
                        "A. nodosum (r.)"="Ascophyllum  nodosum",
                        "A. nodosum (s.)"="Ascophyllum  nodosum",
                        "Amphioctopus fangsiao_"="Amphioctopus fangsiao",
                        "C. fragile"="Codium fragile", # ,  and
                        "C. mosullensis"="Chalcalburnus mosullensis",
                        "C. capoeta umbla"="Capoeta capoeta umbla",
                        "C. crucian"="Carassius carassius",
                        "Cyprinus carpio var. specularis)"="Cyprinus carpio",
                        "Cystoseira abies-marina"="Treptacantha abies-marina", # hyphen is correct
                        "Engraulis encrasicolus)"="Engraulis encrasicolus",
                        "F. spiralis"="Fucus  spiralis",
                        "F. vesiculosus"="Fucus vesiculosus",
                        "G. chilensis"="Gracilaria chilensis",
                        # "Gracilaria bursa-pastoris"="", # hyphen is correct
                        # "Hydrocharis morsus-ranae"="", # hyphen is correct
                        "L. graellsii"="Luciobarbus graellsii",
                        "L. xanthochilus"="Lethrinus xanthochilus",
                        "L. bohar"="Lutjanus bohar",
                        "M. pyrifera"="Macrocystis pyrifera",
                        "M. cephalus"="Mugil cephalus",
                        "Megaloancistrus aculeatus)"="Megaloancistrus aculeatus",
                        "Melcertus latisculatus (family penaeidae)"="Melicertus latisulcatus",
                        "Neomeris van -bosseae"="Neomeris vanbosseae",
                        "Neomeris van-bosseae"="Neomeris vanbosseae",
                        "O. aureus"="Oreochromis aureus",
                        "Oncorhynchus mykiss)"="Oncorhynchus mykiss",
                        "Oreochromis niloticus (juvenile)"="Oreochromis niloticus",
                        "Pangasianodon hypophthalmus (juvenile)"="Pangasianodon hypophthalmus",
                        "Paralichthys oli<ea>aceus"="Paralichthys olivaceus",
                        "Perca -uviatilis"="Perca fluviatilis",
                        "Pinirampus pinirampu)"="Pinirampus pirinampu",
                        "Pseudoplatystoma corruscans)"="Pseudoplatystoma corruscans",
                        "S. sierra"="Scomberomorus sierra",
                        "Salmo trutta m. lacustris"="Salmo trutta",
                        "Sepia o.cinalis"="Sepia officinalis",
                        "Skeletonema marinoi-dohrnii"="Skeletonema dohrnii",
                        "Spisula (pseudocardium) sachalinensis"="Spisula sachalinensis",
                        "Tenualosa ilisha (juvenile)"="Tenualosa ilisha")) %>%
  # Fix ones with more than two words
  mutate(sciname=recode(sciname,
                        "Spyridia fi lamentosa"="Spyridia filamentosa",
                        "T rachurus mediterraneus"="Trachurus mediterraneus")) %>%
  # Mark ones with punctuation still
  mutate(punct=grepl("[[:punct:]]", sciname)) %>%
  # Fix a bunch of long ones
  mutate(sciname=recode(sciname,
                        "Acanthropagrus australis or butcheri"="Acanthropagrus australis, Acanthropagrus butcheri",
                        "Centropristes striata andteolabrax japonicus"="Centropristes striata, Andteolabrax japonicus",
                        "Includes a mix of species belonging to the astacidae"="Astacidae spp.",
                        "Includes a mix of species belonging to the ommastrephidae family"="Ommastrephidae spp.",
                        "Includes a mix of species belonging to the palaemonidae family"="Palaemonidae spp.",
                        "Navodon modestus lephiomus setigerus"="Navodon modestus, Lephiomus setigerus",
                        "Osmerus mordax dentex steindachner"="Osmerus mordax",
                        "Ostreidae family including crassostrea gigas"="Ostreidae spp.",
                        "Species belonging to the portunidae family"="Portunidae spp.",
                        "Spratelloides robustus or sardinops sagax"="Spratelloides robustus, Sardinops sagax",
                        "Salmo trutta morpha fario"="Salmo trutta",
                        "Notarchus indicus armatus baba"="Notarchus punctatus armatus",
                        "Engraulis anchoita hubbs larini"="Engraulis anchoita")) %>%
  # Fix ones that don't get matched in GNR resolve (below)
  mutate(sciname=recode(sciname,
                        "Acanthoparagus bifasciatus"="Acanthopagrus bifasciatus",
                        "Apoleichthus taprobanensis"="Paraploactis taprobanensis",
                        "Artem longinaris"="Artemesia longinaris",
                        "Aristae omorphafoliacea"="Aristaeomorpha foliacea",
                        "Brakyptorosis serrulata"="Brachypterois serrulata",
                        "Bregmaceros mcclellandi"="Bregmaceros mcclellandi",
                        "Caulpera sertularioides"="Caulerpa sertularioides",
                        "Carpioides meridionalis"="Carpiodes carpio",
                        "Clupisoma pseudeutropius atherinoides"="Pachypterus atherinoides",
                        "Corralina mediterranea"="Corallina mediterranea",
                        "Coregonus artedisueur"="Coregonus artedi",
                        "Euchemia cottonii"="Eucheuma cottonii",
                        "Eriscion nebulosos"="Cynoscion nebulosus",
                        "Fueguine sardine"="Clupea fueguensis",
                        "Gadus mangala"="Cirrhinus mrigala",
                        "Gracilaria pusillum"="Gelidium pusillum",
                        "Gracilaria turuturu"="Grateloupia turuturu",
                        "Haliotidae haliotis"="Haliotis spp.",
                        "Helicolenus dactylopterus labillei"="Helicolenus dactylopterus",
                        "Hilsa hilsa"="Tenualosa ilisha",
                        "Holopragus guntheri"="Hoplopagrus guentherii",
                        "Hyme mulleri"="Hymeniacidon  mulleri",
                        "Hyppoglossus hyppogl"="Hippoglossus hippoglossus",
                        "Isoctysis galbana"="Isochrysis galbana",
                        "Johinus borneersis"="Johnius borneensis",
                        "Laurencia mcdermid"="Laurencia mcdermidiae",
                        "Leiostomus xanthurascepede"="Leiostomus xanthuras",
                        "Lithod antarcticus"="Lithodes antarcticus",
                        "Liza strongy locephalus"="Liza strongylocephalus",
                        "Merluccius species"="Merluccius spp.",
                        "Metapenaeus shrimp"="Metapenaeus spp.",
                        "Monronr americanus"="Morone americana",
                        "Meurex meurex"="Murex trapa",
                        "Mullussur muletus"="Mullus surmuletus",
                        "Mulus barbatus ponticus"="Mullus barbatus",
                        "Mylopharyngodon piceusch"="Mylopharyngodon piceus",
                        "Mylopharyngodon piceuschn"="Mylopharyngodon piceus",
                        "Naticaproble maticareeve"="Neverita didyma",
                        "Noplopoma timbria pallas"="Anoplopoma fimbria",
                        "Ostreobrama cotio cotio"="Osteobrama cotio",
                        "Polyrenus species"="Polyrenus spp.",
                        "Salvelinus naresi"="Salvelinus alpinus",
                        "Salmonidae family"="Salmonidae spp.",
                        "Sardinex saga"="Sardinops sagax",
                        "Sargussum turbinaria"="Sargassum turbinaria",
                        "Sadra sarda"="Sarda sarda",
                        "Scarus ghabon"="Scarus ghobban",
                        "Scopelegadus mizoiepis mizolepis"="Scopelogadus mizolepis",
                        "Silvestre milossoma"="Trachinotus goodei",
                        "Spicara vulgaris"="Spicara smaris",
                        "Sudananautes africanus africanus"="Sudananautes africanus",
                        "Sulculus diversicolor aquatieis"="Haliotis diversicolor",
                        "Octopus vulgarismarck"="Octopus vulgaris",
                        "Order teuthoidea"="Teuthoidea spp.",
                        "Osmerus epelanus mordax"="Osmerus mordax",
                        "Palinurus borealis"="Palinurus elephas",
                        "Parambassis wollf"="Parambassis wolffii",
                        "Paeneus kerathurus"="Penaeus kerathurus",
                        "Parophrys ve tutus"="Parophrys vetulus",
                        "Paralycthis adpersus"="Paralichthys adspersus",
                        "Pectinidae family"="Pectinidae spp.",
                        "Pink perch"="Labeo rohita",
                        "Polinicies aemingiana"="Polinices aemingiana",
                        "Pseudopimelodus fasciatum"="Bagre spp.",
                        "Pimedolus clarias"="Pimelodus clarias",
                        "Puntis carana"="Puntius sarana",
                        "Polynema sextarius"="Polynemus sextarius",
                        "Pseudo pleuronectes"="Pseudopleuronectes spp.",
                        "Rajja specie"="Rajja spp.",
                        "Roughear scad"="Decapterus tabl",
                        "Various species"="Various spp.",
                        "Tilapia oreochromis"="Oreochromis spp.",
                        "Tympanostomus fuscatus radula"="Tympanotonos fuscatus",
                        "Uppenus sulphureus"="Upeneus sulphureus",
                        "Wak cuja"="Macrospinosa cuja")) %>%
  # Fix some ones that bother you
  mutate(sciname=recode(sciname,
                        "Acanthopagrus schlegeli"="Acanthopagrus schlegelii")) %>%
  # Mark species or group specific
  mutate(type=ifelse(grepl("spp\\.|sp\\.|,|/| x ", sciname), "group", "species")) %>% # x=hybrids, commas/slashes is multiple
  # Count number of words
  mutate(nwords_orig=freeR::nwords(sciname_orig),
         nwords=freeR::nwords(sciname)) %>%
  # Trim
  mutate(sciname=stringr::str_trim(sciname)) %>%
  # Identify taxa level
  mutate(group=ifelse(grepl("sp\\.|spp\\.", sciname), gsub(" sp\\.| spp\\.", "", sciname), NA)) %>%
  mutate(taxa_level=ifelse(type=="species", "species",
                           ifelse(group %in% genera, "genus",
                                   ifelse(group %in% families, "family",
                                          ifelse(group %in% orders, "order", "other")))))

# Inspect groups
group_key <- spp_key1 %>%
  filter(type=="group")

# Fix some 1 word groups - these look good
group_key %>% filter(nwords_orig==1 & !(sciname_orig %in% c(families, orders, genera))) %>% pull(sciname)

# Inspect species with punctuation - these 3 are correct
spp_key1 %>% filter(type=="species" & punct==T) %>% pull(sciname) %>% sort()

# Inspect species with more than two words
spp_key1 %>% filter(type=="species" & nwords>2) %>% pull(sciname) %>% sort()


# Check species names
################################################################################

# Species names
spp_names <- spp_key1 %>% filter(type=="species") %>% pull(sciname) %>% unique() %>% sort()

# Get species suggestions
spp_names_chunks <- split(spp_names, ceiling(seq_along(spp_names)/100))
spp_suggestions <- purrr::map_df(1:length(spp_names_chunks), function(x){
  spp_names_do <- spp_names_chunks[[x]]
  spp_suggest_chunk <- taxize::gnr_resolve(sci = spp_names_do, best_match_only=T,  canonical = T, cap_first=T)
})

# Format suggestions
spp_suggestions1 <- spp_suggestions %>%
  # Number of words in suggestion
  mutate(nwords_in_suggestion=freeR::nwords(matched_name2)) %>%
  # Suggestion type
  mutate(suggest_type=ifelse(matched_name2==user_supplied_name, "correct", "updated")) %>%
  # Simplify
  unique()

# Suggestions
table(spp_suggestions1$suggest_type)

# One with one word suggestions
spp_suggestions1 %>% filter(nwords_in_suggestion==1) %>% pull(user_supplied_name) %>% sort()

# Build final key
spp_key2 <- spp_key1 %>%
  # Add suggestions
  left_join(spp_suggestions1, by=c("sciname"="user_supplied_name")) %>%
  # Rename
  rename(taxa_type=type, sciname_matched=matched_name2, match_type=suggest_type, sciname_matched_nwords=nwords_in_suggestion) %>%
  # Simplify
  select(sciname_orig, sciname, sciname_matched, sciname_matched, match_type, sciname_matched_nwords, taxa_type, taxa_level) %>%
  # Determine final name
  mutate(sciname_final=ifelse(match_type=="correct" | sciname_matched_nwords==1 | is.na(sciname_matched_nwords) | taxa_type=="group", sciname, sciname_matched)) %>%
  # Simplify
  select(taxa_type, taxa_level, sciname_final, sciname_orig, sciname_matched_nwords, match_type) %>%
  # Rename and arrange
  rename(sciname=sciname_final) %>%
  arrange(taxa_type, taxa_level, sciname) %>% 
  # Squish
  mutate(sciname=stringr::str_squish(sciname))

# Inspect
freeR::complete(spp_key2)

# Ones without out names
# Cyparica samplomoneta, Sciania hatei, Chichorus virginicus = don't know who these are
spp_key2 %>% filter(taxa_level=="species" & is.na(sciname_matched_nwords)) %>% pull(sciname_orig) %>% sort()

# Inspect remaining species with more than two words
spp_key2 %>% filter(taxa_type=="species" & freeR::nwords(sciname)>2) %>% pull(sciname) %>% sort()

# Inspect remaining species with more than two words
taxa_table = freeR::all_fish() %>% 
  mutate(is_right = 1) %>% 
  select(sciname, is_right) %>% 
  unique()

long_names = spp_key2 %>% filter(taxa_type=="species" & freeR::nwords(sciname)>2) %>% select(sciname, sciname_orig) %>% 
  separate(sciname, c("spp1", "spp2", "spp3"), " ", remove=F) %>% 
  mutate(name1 = paste(spp1, spp2, sep=" "),
         name2 = paste(spp1, spp3, sp = " ")) %>% 
  select(sciname, sciname_orig, name1, name2) %>% 
  reshape2::melt(id.vars = c("sciname", "sciname_orig")) %>% 
  rename(name = value) %>% 
  select(-variable) %>% 
  mutate(name = stringr::str_squish(name)) %>% 
  left_join(taxa_table, by = c("name" = "sciname")) %>% 
  drop_na(is_right) %>% 
  select(-sciname, -is_right) %>% 
  rename(sciname2 = name) %>% 
  distinct(sciname_orig, .keep_all = T)
  
spp_key3 = spp_key2 %>% 
  left_join(long_names) %>% 
  rename(sciname1 = sciname) %>% 
  mutate(sciname = if_else(is.na(sciname2), sciname1, sciname2)) %>% 
  select(taxa_type, taxa_level, sciname, sciname_orig, sciname_matched_nwords, match_type)
  
# Add updated scientific names to data
################################################################################

# Format
data_sci2 <- data_sci1 %>%
  # Rename
  rename(sciname_orig=sciname) %>%
  # Add updated scientific name
  left_join(spp_key3 %>% select(taxa_type:sciname_orig), by=c("sciname_orig")) %>%
  # Simplify
  select(sciname, sciname_orig, taxa_type, taxa_level, everything()) %>%
  # Remove columns
  select(-sciname_source)

# Inspect
freeR::complete(data_sci2)

# Confirm that the datasets are the right size
nrow(data_comm) + nrow(data_sci2) == nrow(data_orig)

# Input scientific name based on common name
################################################################################

##Assign scientific names
sci_common_names <- data_sci2 %>%
  select(common_name, sciname, sciname_orig, taxa_type, taxa_level) %>% 
  distinct(common_name, .keep_all = TRUE) %>% 
  drop_na(common_name)

# Seperate those without scientific name
data_comm <- data_orig %>%
  # Reduce to taxa without taxonomic information
  filter(taxa_name_source %in% c("Food name (English)", "Food name (original)")) %>% 
  left_join(sci_common_names) %>% 
  select(-taxa_name, -taxa_name_source)

data_comm_sci = data_comm %>% 
  filter(!is.na(sciname))

data_sci3 = rbind(data_sci2, data_comm_sci)

##Fill in taxonomic informtion
##Load Taxa_table
taxa_table = readRDS("data-raw/taxa-table/taxa_table.Rds")
  
dta_species = data_sci3 %>% 
  filter(taxa_level=="species") %>% 
  select(-genus) %>%
  separate(sciname, c("genus", "spp"), " ", remove=FALSE) %>% 
  select(-spp)

spp_missing = dta_species %>% 
  filter(is.na(family)) %>% 
  select(-family, -order, -class) %>% 
  left_join(taxa_table)

dta_species2 = dta_species %>% 
  filter(!is.na(family)) %>% 
  rbind(spp_missing)

dta_family = data_sci3 %>% 
  filter(taxa_level=="family")

family_missing = dta_family %>% 
  filter(is.na(family)) %>% 
  select(-order, -family, -class) %>%
  separate(sciname, c("family", "spp"), " ", remove=T) %>% 
  mutate(sciname = NA) %>% 
  left_join(taxa_table %>% select(family, order, class) %>% distinct(family, .keep_all=T)) %>% 
  select(-spp)

dta_family2 = dta_family %>% 
  filter(!is.na(family)) %>% 
  rbind(family_missing)

dta_genus = data_sci3 %>% 
  filter(taxa_level=="genus") %>% 
  separate(sciname, c("genus", "spp"), " ", remove=T) %>%
  mutate(sciname = NA) %>% 
  select(-spp) 

genus_missing = dta_genus %>% 
  filter(is.na(family)) %>% 
  select(-family, -order, -class) %>%
  left_join(taxa_table)

dta_genus2 = dta_genus %>% 
  filter(!is.na(family)) %>% 
  rbind(genus_missing)


dta_other = data_sci3 %>% 
  filter(taxa_level=="other")

data_sci4 = rbind(dta_species2, dta_genus2, dta_family2, dta_other) %>% 
  mutate(class = if_else(order == "Actinopterygii", "Actinopterygii", class),
         order = na_if(order, "Actinopterygii")) %>% 
  select(-kingdom, -phylum, -taxa_id, -taxa_db, -taxa_type, -taxa_level) %>% 
  select(sciname, sciname_orig, genus, family, order, class, common_name, food_name, food_name_orig, everything()) %>% 
  unique()


##Further clean species without taxa information
# data_comm_Nosci = data_comm %>% 
#   filter(is.na(sciname)) %>% 
#   select(-kingdom, -phylum, -taxa_id, -taxa_db, -taxa_type, -taxa_level, -class, -family, -genus, -order, -notes, -sciname, -sciname_orig) %>% 
#   unique()
##Load Taxa_table
taxa_table = readRDS("data-raw/taxa-table/taxa_table.Rds")

data_comm2 = data_comm %>% 
  filter(is.na(sciname)) %>% 
  select(-kingdom, -phylum, -taxa_id, -taxa_db, -taxa_type, -taxa_level, -class, -family, -order) %>% 
  unique() %>% 
  mutate(food_name_orig = if_else(is.na(food_name_orig), food_name, food_name_orig),
         food_name = tolower(food_name),
         ##Class
         class = case_when(
           #Decapoda
           str_detect(food_name, paste(c("fish", "char"), collapse = '|')) ~ "actinopterygii",
           str_detect(food_name, paste(c("shark", "ray"), collapse = '|')) ~ "chondrichthyes",
           str_detect(food_name, paste(c("shellfish", "mollusk"), collapse = '|')) ~ "bivalvia",
           str_detect(food_name, paste(c("snail"), collapse = '|')) ~ "gastropoda"),
         ##Order
         order = case_when(
           #Decapoda
           str_detect(food_name, paste(c("shrimp", "crab", "lobster", "prawn"), collapse = '|')) ~ "decapoda",
           #Squid
           str_detect(food_name, "squid") ~ "teuthida",
           #Herrings
           str_detect(food_name, paste(c("herring", "anchovy", "sardine"), collapse = '|')) ~ "clupeiformes",
           #Cod
           str_detect(food_name, "cod") ~ "gadiformes",
           #Turtle
           str_detect(food_name, "turtle") ~ "testudines",
           #Pike
           str_detect(food_name, "pike") ~ "esociformes",
           #Carp
           str_detect(food_name, "carp") ~ "cypriniformes"),
         ##Family
         family = case_when(
           #Shrimps
           str_detect(food_name, "shrimp") ~ "penaeidae",
           #mussels
           str_detect(food_name, "mussel") ~ "mytilidae",
           #oysters
           str_detect(food_name, "oyster") ~ "ostreidae",
           #octopus
           str_detect(food_name, "octopus") ~ "octopodidae",
           #tuna
           str_detect(food_name, "tuna") ~ "scombridae",
           #salmon, trout
           str_detect(food_name, paste(c("salmon", "trout"), collapse = '|')) ~ "salmonidae",
           #flatfish
           str_detect(food_name, "flat") ~ "scophthalmidae",
           #conch
           str_detect(food_name, "conch") ~ "strombidae",
           #rockfish
           str_detect(food_name, "rockfish") ~ "sebastidae",
           #Milkfish
           str_detect(food_name, "milk fish") ~ "chanidae",
           #Swimming crab
           str_detect(food_name, "swimming crab") ~ "portunidae",
           #Halibut
           str_detect(food_name, "halibut") ~ "pleuronectidae",
           #Mackerel
           str_detect(food_name, "mackerel") ~ "scombridae"))

afcd_common_family = data_comm2 %>%
  filter(!is.na(family)) %>% 
  select(-class, -order) %>% 
  left_join(taxa_table %>% select(-genus) %>% unique())

afcd_common_order = data_comm2 %>%
  filter(is.na(family),
         !is.na(order)) %>% 
  select(-class) %>% 
  left_join(taxa_table %>% select(-genus, -family) %>% unique())

afcd_common_class = data_comm2 %>%
  filter(is.na(family),
         is.na(order),
         !is.na(class))

afcd_missing = data_comm2 %>%
  filter(is.na(family),
         is.na(order),
         is.na(class))

data_comm_taxa = rbind(afcd_common_family, 
                       afcd_common_order, 
                       afcd_common_class)

data_sci5 = rbind(data_sci4, data_comm_taxa)

# Export data with some taxonomic information
saveRDS(data_sci5, file=file.path(outdir, "AFCD_data_taxa.Rds"))

# Export data with complete scientific name
data_sci_only = data_sci5 %>% 
  drop_na(sciname) %>% 
  select(sciname, sciname_orig, genus, family, order, class, common_name, food_name, food_name_orig, everything()) %>% 
  unique()

saveRDS(data_sci_only, file=file.path(outdir, "AFCD_data_sci.Rds"))

##Export data without scientific names
saveRDS(afcd_missing, file=file.path(outdir, "AFCD_data_comm.Rds"))

##Export data in the wide format (data_taxa)
data_taxa_wide = data_sci5 %>% 
  distinct(sciname, sciname_orig, genus, family, order, class, common_name, food_name, food_name_orig, fct_code_orig, common_name_detailed, food_prep, food_prep_detailed, food_part, food_part_detailed, prod_catg, other_ingredients, study_type, study_id, iso3, country, edible_prop, notes, nutrient_type, nutrient, nutrient_orig, nutrient_desc, nutrient_code_fao, nutrient_units, .keep_all = T) %>% 
  spread(nutrient, value)

# Export
saveRDS(data_taxa_wide, file=file.path(outdir, "AFCD_data_taxa_wide.Rds"))
