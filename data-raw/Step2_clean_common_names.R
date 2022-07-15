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
data_orig <- readRDS(file.path(outdir, "AFCD_data_pass1.Rds"))

# Read ref key
ref_key <- readRDS(file.path(outdir, "AFCD_reference_key.Rds"))

# Read taxa table for spanish common names
taxa_table = readRDS("data-raw/taxa-table/taxa_table.Rds")

##Create ID for each row
data_orig = data_orig %>% 
  mutate(ID = 1:nrow(data_orig)) %>% 
  select(ID, everything())


# translate spanish food names and other data
data_orig_t = data_orig %>%
  filter(study_id=="LATINFOODS" & taxa_name_source=="Food name (original)") %>%
  mutate(food_name=case_when(is.na(food_name) ~ food_name_orig, TRUE ~ food_name)) %>% # make food_name = food_name_orig when no food name
  mutate(food_name=gsub("^Surubí", "Spotted sorubim", food_name)) %>%
  mutate(food_name=gsub("^Carpa", "Common carp", food_name)) %>%
  mutate(food_name=gsub("^Chita", "Peruvian grunt", food_name)) %>%
  mutate(food_name=gsub("^Paiche", "Arapaima", food_name)) %>%
  # mutate(food_name=gsub("^Corocoro", "Burro grunt", food_name)) %>% excluded because sciname provided in LATINFOODS
  mutate(food_name=gsub("^Gatuso", "Narrownose smooth-hound", food_name)) %>%
  mutate(food_name=gsub("^Pintado", "Sand tiger shark", food_name)) %>% 
  mutate(food_name=gsub("^Cascudo", "Amazon sailfin catfish", food_name)) %>%
  mutate(food_name=gsub("^Dourado", "Dorado", food_name)) %>%
  mutate(food_name=gsub("^Pacu", "Cachama", food_name)) %>%
  rbind(data_orig[!(data_orig$study_id=="LATINFOODS" & data_orig$taxa_name_source=="Food name (original)"),]) %>% # add back in
  unique()

## Clean common names in english, filters out the values that don't have food_name
com_names = data_orig_t %>% 
  select(ID, food_name) %>% # this is filtering out a lot of fish names 
  unique() %>% 
  drop_na() %>% 
  # Recode column names
  rename(food_name_orig=food_name) %>%
  mutate(food_name=food_name_orig) %>% 
  mutate(food_name = recode(food_name,
                            "Trucha ahumada" = "Trucha, ahumada", 
                            "Croqueta de merluza" = "merluza, croqueta", 
                            "frog legs, raw" = "frog, raw, legs", 
                            "cusk, tusk, raw" = "cusk, raw",
                            "bassa (basa)" = "basa",
                            "bassa" = "basa",
                            "wild blackspot seabream" = "wild, blackspot seabream",
                            "Fish eggs (Carp, Cod, Haddock, Herring, Pike, Shad)" = "Fish eggs",
                            "Fish; cod; walleye pollock*; \"Sukimidara\" (skinned; salted and dried fillet)__*Syn. Alaska pollock_ " = "Fish, walleye pollock, dried, fillet",
                            "Mola, body tissue, anterior (including head, excluding eyes), raw" = "mola, raw, whole",
                            "Fish burger, breaded, fried, with bread, cheese, sauce, fast food restaurant" = "fish, fried, bread, cheese",
                            "Mollusks; short-necked clam*; \"Tsukudani\" (simmered meat in soy sauce and sugar)_*Syn. baby-neck clam; Manila clam; Japanese littleneck_" = "Mollusks; short-necked clam, simmered, soy souce, sugar",
                            "Fish; cod; walleye pollock*; \"Karashi-mentaiko\" (salted roe with red hot pepper powder) _*Syn. Alaska pollock_ " = "Fish; walleye pollock, roe, salted",
                            "Fish; cod; walleye pollock*; \"Tarako\" (salted roe); baked _*Syn. Alaska pollock_ " = "Fish; walleye pollock, roe, baked",
                            "Fish; cod; walleye pollock*; \"Tarako\" (salted roe); raw _*Syn. Alaska pollock_ " = "Fish; walleye pollock, roe, salted",
                            "USDA Commodity, salmon nuggets, breaded, frozen, heated" = "Salmon, breaded, frozen",
                            "Fish; red gurnard*; raw_*Syn. sea robin; gurnard; gurnet_" = "Fish; red gurnard; raw",
                            "Mollusks; short-necked clam*; raw_*Syn. baby-neck clam; Manila clam; Japanese littleneck_" = "Mollusks; short-necked clam; raw",
                            "Mollusks; short-necked clam*; canned products; boiled with seasoning _*Syn. baby-neck clam; Manila clam; Japanese littleneck_" = "Mollusks; short-necked clam; canned products; boiled with seasoning",
                            "Mollusks; short-necked clam*; canned products; boiled in brine_*Syn. baby-neck clam; Manila clam; Japanese littleneck_" = "Mollusks; short-necked clam; canned products; boiled in brine",
                            "Sea-hare; intestines; raw;¾D. auricularia" = "Sea-hare; intestines; D. auricularia",
                            "Lobster; mangrove; raw;¾T. anomala" = "Lobster; mangrove; raw;T. anomala",
                            "Mackerel, Spanish, â€œWaluâ€\u009d, raw" = "Mackerel, Spanish, raw",
                            "Clam; –kaikoso”; raw;Anadarasp." = "Clam; raw;Anadara",
                            "Little tuna, wild, white muscle flesh (cephalal and caudal, ventral and dorsal), raw" = "Little tuna, wild, white muscle flesh, raw",
                            "Fish, caviar, black and red, granular" = "Fish, caviar",
                            "Seaweed; –Nama”; raw;Caulerpasp." = "Seaweed; raw; Caulerpa",
                            "â€˜Shirogaiâ€™ shell" = "shell",
                            "dulse, dillisk, dilisk" = "dulse, dillisk",
                            "Fish;¾L. xanthophilus; baked; earth-oven" = "Fish;L. xanthophilus; baked",
                            "Herring, smoked, BÃ¸kling" = "Herring, smoked",
                            "Saithe, breaded, industrially made, LerÃ¸y" = "Saithe, breaded",
                            "Fish; tuna; young bluefin tuna; raw" = "Fish; tuna; bluefin tuna; raw",
                            "Sprat in tomato sauce, canned" = "Sprat, in tomato sauce, canned",
                            "Sardines in oil; canned (drained solids with bone)" = "Sardines, in oil; canned",
                            "Sprat in oil, drained, canned" = "Sprat, in oil, drained, canned",
                            "Dish with mola, onion and green chili" = "mola, onion and green chili",
                            "Casserole, with cod and tomato" = "Casserole, cod and tomato",
                            "Casserole, with saithe, onion and sweet pepper sauce" = "Casserole, saithe, onion and sweet pepper sauce",
                            "Fish; sardine; \"Mezashi\" (skewered; salted and semi-dried whole); baked" = "Fish; sardine, salted; whole; baked",
                            "Herring, pickled, cured, marinated, drained" = "Herring, cured, marinated, drained",
                            "Fish; sardine; \"Mezashi\" (skewered; salted and semi-dried whole); raw" = "Fish; sardine; salted, whole; raw",
                            "Fish, reef, composite, raw" = "reef fish, composite, raw",
                            "Mola (cultured), wild, raw" = "Mola, wild, raw",
                            "Striped catfish, whole, baked (ASEAN/Bangladesh)" = "Striped catfish, whole, baked (Bangladesh)",
                            "Striped catfish, whole, boiled in recipe (ASEAN/Bangladesh)" = "Striped catfish, whole, boiled in recipe (Bangladesh)",
                            "Striped catfish, whole, boiled_ (ASEAN/Bangladesh)" = "Striped catfish, whole, boiled_ (Bangladesh)",
                            "Striped catfish, whole, raw (ASEAN/Bangladesh)" = "Striped catfish, whole, raw (Bangladesh)",
                            "Atlantic salmon, farmed, fillet w/o skin, grilled_ (Ireland, UK)" = "Atlantic salmon, farmed, fillet w/o skin, grilled_ (Ireland)",
                            "Atlantic salmon, farmed, fillet w/o skin, boiled in recipe (Ireland, UK)" = "Atlantic salmon, farmed, fillet w/o skin, boiled in recipe (Ireland)",
                            "Atlantic salmon, farmed, fillet w/o skin, boiled_ (Ireland, UK)" = "Atlantic salmon, farmed, fillet w/o skin, boiled_ (Ireland)",
                            "Atlantic salmon, farmed, fillet w/o skin, raw (Ireland, UK)" = "Atlantic salmon, farmed, fillet w/o skin, raw (Ireland)",
                            "Fish, halibut, Atlantic and Pacific, raw" = "Fish, halibut, raw",
                            "Fish, halibut, Atlantic and Pacific, cooked, dry heat" = "Fish, halibut, cooked, dry heat",
                            "Atlantic horse mackerel, atlantic, wild, lean, fillet w/o skin, raw" = "Atlantic horse mackerel, wild, lean, fillet w/o skin, raw",
                            "Mackerel, Spanish, “Walu”, raw" = "Mackerel, Spanish, raw")
         ) %>%
  ##Remove "and"
  mutate(food_name = gsub("salmon and trout;", "", food_name)) %>%
  mutate(food_name = gsub("w/o", "without", food_name)) %>%
  mutate(food_name = gsub(" w/ ", " with ", food_name)) %>%
  mutate(food_name = gsub(" c/ ", " with ", food_name)) %>%
  mutate(food_name = gsub("whole/no skin", "whole with no skin", food_name)) %>%
  mutate(food_name = gsub("cod liver", "cod, liver", food_name)) %>%
  mutate(food_name = gsub("and ", ",", food_name)) %>%
  mutate(food_name = gsub("de mar", ", ocean", food_name)) %>%
  mutate(food_name = gsub("de río", ", river", food_name)) %>%
  mutate(food_name = gsub("/30 min", "", food_name)) %>%
  mutate(food_name = gsub("/45 min", "", food_name)) %>%
  mutate(food_name = gsub("/40 min", "", food_name)) %>%
  mutate(food_name = gsub("/12 min", "", food_name)) %>%
  mutate(food_name = gsub("-30Â°C", "", food_name)) %>%
  mutate(food_name = gsub("-18Â°C", "", food_name)) %>%
  mutate(food_name = enc2native(food_name)) %>% #added to deal with ASCII encodings, now to native encoding working on MacOSX, Linux and Windows
  ##Seperate food name from other information
  separate(food_name, 
           into=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L", "M"), 
           sep = "([_;,()%/])",
           remove=F) %>%
  ##Change to long format
  reshape2::melt(id.vars = c("ID", "food_name_orig", "food_name")) %>%
  ##Remove blancks and NAs
  na_if("") %>%
  na_if(" ") %>% 
  drop_na(value) %>% 
  # Trim
  mutate(value=stringr::str_trim(value)) %>% 
  mutate(value = tolower(value)) %>% 
  # TODO: translate spanish prep types and more 
  mutate(value=recode(value,
                      "cocido"="boiled","al natural"="canned natural","hormiga" = "ant", "carne" = "meat", "agua dulce" = "freshwater", "filete" = "fillet",
                      "de agua dulce" = "freshwater","entero" = "whole","seco" = "dried","entera" = "whole" ,"de ispi" = "ispi", "carne sin piel" = "meat without skin",
                      "fresca" = "fresh" ,"de rana"="from frog", "huevera"="eggs","de pescado"="of fish", "con espinas"="with bones",
                      "pulpa"="muscle tissue","grande" = "big","chino"="chinese", "salado" = "salted","pulpa asada"="baked muscle tissue","enlatado"="canned",
                      "crudo"="raw","en conserva"="canned","sardinha"="sardine","enlatada"="canned","al horno"="baked","filé"="muscle tissue","ovas"="eggs",
                      "cocida"="boiled", "atum"="tuna","lomo"="muscle tissue","de camarón blanco y titi"="mixed shrimp species",
                      "de camarón rosado y fidel"="mixed shrimp species","pescaditos fritos"="fried fish","água doce"="freshwater","músculo"="muscle tissue",
                      "blanca"="white","de batracio"="amphibian","assado"="baked","assada"="baked","abacaxi"="","cachorro"="juvenile","cruda"="raw","cruda"="raw",
                      "cocido y frito"="boiled and fried","frita"="fried","con huesos"="with bones","sin piel"="without skin", "frito"="fried","seca"="dried",
                      "asada"="baked","sancochada"="boiled with condiments","deshidratado"="dried","sancochado"="boiled with condiments","en agua"="in water",
                      "congelado"="frozen","crudas"="raw","ralado"="grated","sólido"="solid","crua"="raw","cru"="raw","en aceite"="in oil","dorada"="sea bream",
                      "sardina"="sardine", "sardinha"="sardine", "precocido"="pre-boiled","con sal"="with salt","salada"="salted","sadia"="healthy",
                      "maionese e vegetais"="with mayonnaise and vegetables","conserva"="canned","molho branco"="in white sauce","rehidratado"="re-hydrated",
                      "pimenta"="pepper","molho de tomate temperado"="in tomato sauce","cebola e louro"="onion and bay leaves","coqueiro"="coconut")) %>% 
  mutate(value = gsub('[*"”-]', "", value)) %>% 
  mutate(value = gsub("Syn.", "", value)) %>% 
  mutate(value = gsub("Ã©", "ao", value)) %>% 
  mutate(value = gsub("¾d.", "", value)) %>% 
  mutate(value = gsub("¾l.", "", value)) %>% 
  mutate(value = gsub("¾t.", "", value)) %>% 
  #Preperation types 
  mutate(name_type = case_when(
    ## Preparation types
    str_detect(value, paste(c("boiled", "grilled", "soup", "bake", "microwaved", "kippered", "canned", "grill", "tempura", "moist heat", "smoke", "dried", "gratin", "simmered", "paste", "fried", "cured", "salted", "cooked", "roasted", "battered", "surimi", "pickled", "steam", "steaemed", "poach", "dry heat", "sushi", "sashimi", "breaded", "casserole", "pudding", "balls", "dressed", "cake", "drained", "brine", "fermented", "skewered"),collapse = '|')) ~ "prep",
    value %in% c("raw", "can") ~ "prep", 
    ## Broad groups
    value %in% c("mollusks", "fish", "crustacean", "crustaceans", "lean fish", "reef fish", "reef") ~ "broad_group", 
    ## Wild vs farmed
    str_detect(value, paste(c("wild", "farmed", "cultured", "aquaculture"), collapse = '|')) ~ "catg",
    ## Animal parts
    str_detect(value, paste(c("fillet", "whole", "roe", "flesh", "gutted", "cleaned fish", "incl", "entire", "intestines", "viscera", "meat", "arms", "tentacles", "body", "claw", "mantle", "gonads", "tail part", "caviar", "muscle", "muslce", "esophagus", "eggs", "head included", "eyes included", "boneless", "skinless", "skinned", "eyes excluded", "eyes partly excluded", "without bones", "without skin", "less skin", "skinon", "with bones", "peeled", "scales removed"), collapse = '|')) ~ "part",
    value %in% c("dressed with head", "with ovary", "without head", "with shell", "with skin", "skin", "ventral with skin", "solids with bone", "skin & bones", "foot", "milt", "liver", "tail", "dorsal with skin") ~ "part", 
    ## Scientific Names
    value %in% c("c. demersum", "b. violocea", "p. aemingiana", "l. xanthophilus", "s.cavalla", "d. auricularia", "t. anomala", "s.niphonius", "tridacna maxima", "tripneustes gratilla", "eiocheir japoncus", "ostrea denselamellosa") ~ "sci",
    ## Regions
    value %in% c("chile", "european", "africa", "europe", "greenland", "asean/bangladesh", "euroamerican", "ireland", "china", "denmark", "northeast pacific", "ne atlantic", "north america", "bangladesh", "vietnam", "norway", "uk", "usa", "iceland", "germany", "new zealand", "northeast atlantic", "asean", "mediterranean sea", "northwest atlantic", "norwegian") ~ "region",
    ## Seasons
    str_detect(value, paste(c("summer", "winter", "autumn", "spring"), collapse = '|')) ~ "season",
    ## Other ingredients
    value %in% c("in tomato sauce", "onion", "with cream", "with sugar", "marinated", "sweet pepper sauce", "marinated in vinegar", "with potatoes", "with egg", "with seasoning", "with seaweed", "with mayonnaise", "with mustard sauce", "seasoned with mirin", "split seasoned with mirin", "green chili", "spices", "sour cream", "tomato", "rolled in breadcrumbs", "milk added", "in oil", "in flour", "in jelly", "rolled in flour", "floured", "bread", "cheese", "soy souce", "soy sauce", "sugar", "seasoned", "garlic", "salt", "salt added to water", "filled", "fish paté", "in spicy marinade", "creamed", "crumbed") ~ "other_ingredients",
    ## Sex
    value %in% c("male", "female") ~ "sex",
    ## Genus
    value %in% c("anadara", "caulerpa") ~ "genus",
    ##Other information (to keep)
    value %in% c("fresh", "frozen", "not previously frozen", "may have been previously frozen", "packaged frozen", "previously frozen", "purchased frozen") ~ "other_info",
    ## Other information (to remove)
    value %in% c("n.s.", "with integument", "lox", "commercially processed", "not further specified", "pelagic", "prepared products", "coop xtra fiskegrateng", "findus steketorsk", "enghav fiskegrateng med makaroni", "soaked in water", "southern rock", "fingers", "commercial", "mashed", "natural", "traditionally", "light", "laboratory", "restaurant style", "full grown", "fully grown", "combined species", "solids", "fins", "liquid", "back", "lakestocked", "talley's", "home recipe", "imitation", "flavoured", "unflavoured", "regular", "fat not further defined", "findus", "first price", "bones",  "tempera",
                 "total can contents", "usda commodity", "–lumi", "medium size", "size", "small size", "edible portion", "ready to eat", "from takeaway outlet", "blended frying fat", "new york state", "adult fish", "maki", "nigiri", "brinesoaked", "marine water", "edible part", "large", "large size", "mature",
                 "ajitsukehirakiboshi", "–nama", "mezashi", "shiokara", "namaboshi", "mirinboshi", "kabayaki", "tazukuri", "shioiwashi", "denbu", "ameni", "ikura", "shirasuboshi", "shirayaki", "sujiko", "mefun", "shiozake", "kusaya", "aramaki", "–kaikoso", "hirakiboshi", "niboshi", "maruboshi", "amazuzuke", "kanroni", "tsukudani", "sababushi", "–walu", "–kai", "lerøy saithe", "first price fiskegrateng med makaroni",
                 "middle portion", "virgin olive oil", "veg.oil", "sour", "sea water", "sea", "unheated", "ventral", "first price fiskegrateng med makaron", "findus familiens fiskegrateng", "fields river", "helix", "lobnobs", "young <1yr", "minced", "industrially made", "marine waters", "mayjune", "little spicies", "all type", "plain", "along dorsal line", "eta", "northern", "assorted flavours", "tusk cusk", "basa bassa",
                 "sealord", "treated", "young", "small fish", "fatty", "2y", "2yr", "45y", "4y", "50", "60", "75", "a fish", "with bones", "freshwater", "unspecified", "edible parts", "channel", "composite", "without salt", "solids & liquid", "slices", "70", "ocean", "fish patties", "lean", "eastern", "without salt and fat","etc.", "coop", "refrigerated", "enghav fiskegrateng med makaroni", 
                 "as part of a recipe", "seafoods", "ocean", "river", "with or without added fat", "portion", "julyseptember", "belly flaps removed", "without visible fat", "palmkernel oil", "stabburlaks", "fat not further defined", "caudal end", "takeaway outlet", "may have been previously frozen", "without salt or fat", "no added fat", "mixed species", "fat", "compressed", "") ~ "remove", 
    TRUE ~ "com_name")) %>% # anything that is not matched = common name 
  filter(!name_type == "remove") %>% 
  select(-variable, -food_name)

##Questions
#Treated?
#Salted?
#"fresh", "frozen", "not previously frozen", "may have been previously frozen", "packaged frozen", "previously frozen", "purchased frozen" - remove?
#Marinated is prep type or other ingredient?

##Check common names
#Long list
common_long = com_names %>%
  filter(name_type == "com_name")

#wide format
common_wide = com_names %>%
  filter(name_type == "com_name") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(com_name) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=com_name) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE) %>% 
  rename(common_name_1 = "1",
         common_name_2 = "2",
         common_name_3 = "3") %>% 
  mutate(common_name_3 = if_else(freeR::nwords(common_name_1)==1 & freeR::nwords(common_name_2) == 1, paste(common_name_2, common_name_1), common_name_3),
         common_name_3 = if_else(common_name_2=="atlantic", paste(common_name_2, common_name_1), 
                                 if_else(common_name_1=="salmon", paste(common_name_2, common_name_1), 
                                         if_else(common_name_1 == "tuna", paste(common_name_2, common_name_1), common_name_3)))) %>% 
  mutate(common_name_3 = gsub("tuna tuna", "tuna", common_name_3),
         common_name = if_else(!is.na(common_name_3), common_name_3, 
                               if_else(freeR::nwords(common_name_2)>1, common_name_2, common_name_1)),
         common_name_detailed = paste(common_name_1, common_name_2, common_name_3, sep = ", "),
         common_name_detailed = gsub(", NA, NA", "", common_name_detailed),
         common_name_detailed = gsub(", NA", "", common_name_detailed)) %>% 
  select(-common_name_1, -common_name_2, -common_name_3, -ID)

common_name_key = common_wide %>% 
  select(food_name_orig, common_name)

##Check Preparation types
##Long list
prep_long = com_names %>%
  filter(name_type == "prep")

##wide format
prep_wide = com_names %>%
  filter(name_type == "prep") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(prep) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=prep) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE) %>%
  mutate(preparation_detailed = paste(`1`, `2`, `3`, sep = ","),
         preparation_detailed = gsub(",NA,", "", preparation_detailed),
         preparation_detailed = gsub(",NA", "", preparation_detailed),
         preparation_detailed = gsub("NA", "", preparation_detailed),
         preparation_detailed = gsub(",", ", ", preparation_detailed))

prep_clean = prep_wide %>% 
  mutate(preparation_simple = case_when(
    str_detect(preparation_detailed, paste(c("raw", "sushi", "sashimi"),collapse = '|')) ~ "raw",
    str_detect(preparation_detailed, "canned") ~ "canned",
    preparation_detailed %in% "can" ~ "canned",
    str_detect(preparation_detailed, "boiled") ~ "boiled",
    str_detect(preparation_detailed, "smoke") ~ "smoked",
    str_detect(preparation_detailed, "dried") ~ "dried",
    str_detect(preparation_detailed, "fermented") ~ "fermented",
    str_detect(preparation_detailed, "fried") ~ "fried",
    str_detect(preparation_detailed, "salted") ~ "boiled",
    str_detect(preparation_detailed, "grill") ~ "grilled",
    str_detect(preparation_detailed, paste(c("steam", "steaemed"),collapse = '|')) ~ "steamed",
    str_detect(preparation_detailed, "microwaved") ~ "microwaved",
    str_detect(preparation_detailed, "bake") ~ "baked",
    TRUE ~ "cooked")) %>% 
  select(food_name_orig, preparation_simple, preparation_detailed)

#####################Check parts
##Long list
parts_long = com_names %>%
  filter(name_type == "part")

##wide format
parts_wide = com_names %>%
  filter(name_type == "part") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(part) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=part) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE) %>%
  mutate(part_detailed = paste(`1`, `2`, `3`, sep = ","),
         part_detailed = gsub(",NA,", "", part_detailed),
         part_detailed = gsub(",NA", "", part_detailed),
         part_detailed = gsub("NA", "", part_detailed),
         part_detailed = gsub(",", ", ", part_detailed))

parts_clean = parts_wide %>% 
  mutate(part_simple = case_when(
    str_detect(part_detailed, paste(c("fillet", "flesh", "meat", "muscle", "muslce", "tentacles", "without bones"),collapse = '|')) ~ "muscle tissue",
    str_detect(part_detailed, paste(c("whole", "entire"),collapse = '|')) ~ "whole",
    str_detect(part_detailed, paste(c("gutted", "cleaned fish", "head included", "dressed with head"),collapse = '|')) ~ "whole gutted",
    str_detect(part_detailed, paste(c("roe", "gonads", "eggs", "caviar"),collapse = '|')) ~ "roe",
    str_detect(part_detailed, paste(c("intestines", "viscera", "esophagus", "liver", "milt"),collapse = '|')) ~ "viscera",
    TRUE ~ "other")) %>% 
  select(food_name_orig, part_simple, part_detailed)

#####################Check broad groups types
##Long list
bgroup_long = com_names %>%
  filter(name_type == "broad_group")

##in wide format
bgroup_wide = com_names %>%
  filter(name_type == "broad_group") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(broad_group) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=broad_group) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

bgroup_clean = bgroup_wide %>% 
  rename(broad_group = `1`)

#####################Check wild vs farmed
##Long list
catg_long = com_names %>%
  filter(name_type == "catg")

## wide format
catg_wide = com_names %>%
  filter(name_type == "catg") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(catg) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=catg) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

catg_clean = catg_wide %>% 
  rename(catg = `1`) %>% 
  mutate(catg = if_else(catg %in% c("cultured", "cultured in freshwater", "cultured in the sea", "farmed", "aquacultured", "farmed flesh"), "farmed", 
                        if_else(catg %in% c("wild", "wild edible flesh", "wild caught", "ise wild arame"), "wild capture", catg))) %>% 
  select(-ID)

#####################Check scientific names
##Long list
sci_long = com_names %>%
  filter(name_type == "sci")

## wide format
sci_wide = com_names %>%
  filter(name_type == "sci") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(sci) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=sci) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE) 

sci_clean = sci_wide %>% 
  rename(sciname = `1`) %>% 
  mutate(sciname = recode(sciname,
                          "s.cavalla" = "Scomberomorus cavalla",
                          "s.niphonius" = "Scomberomorus niphonius",
                          "t. anomala" = "Thalassina anomala",
                          "d. auricularia" = "Dolabella auricularia",
                          "c. demersum" = "Ceratophyllum demersum",
                          "l. xanthophilus" = "Leiostomus xanthurus",
                          "b. violocea" = "Batissa violacea",
                          "p. aemingiana" = "Polinices aemingiana",
                          "tripneustes gratilla" = "Tripneustes gratilla",
                          "eiocheir japoncus" = "Eiocheir japoncus",
                          "ostrea denselamellosa" = "Ostrea denselamellosa"))

#####################Check regions
##Long list
region_long = com_names %>%
  filter(name_type == "region")

## wide format
region_wide = com_names %>%
  filter(name_type == "region") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(region) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=region) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

#####################Check season
##Long list
season_long = com_names %>%
  filter(name_type == "season")

## wide format
season_wide = com_names %>%
  filter(name_type == "season") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(season) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=season) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

#####################Check other_ingredients
##Long list
other_ingredients_long = com_names %>%
  filter(name_type == "other_ingredients")

## wide format
other_ingredients_wide = com_names %>%
  filter(name_type == "other_ingredients") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(other_ingredients) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=other_ingredients) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

other_ingredients_clean = other_ingredients_wide %>% 
  mutate(other_ingredients = paste(`1`, `2`, `3`, sep = ", "),
         other_ingredients = gsub(", NA, NA", "", other_ingredients),
         other_ingredients = gsub(", NA", "", other_ingredients)) %>% 
  select(food_name_orig, other_ingredients)

#####################Check sex
##Long list
sex_long = com_names %>%
  filter(name_type == "sex")

## wide format
sex_wide = com_names %>%
  filter(name_type == "sex") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(sex) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=sex) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

#####################Check genus
##Long list
genus_long = com_names %>%
  filter(name_type == "genus")

## wide format
genus_wide = com_names %>%
  filter(name_type == "genus") %>% 
  pivot_wider(names_from = name_type,
              values_from = value,
              values_fill = NA) %>%
  unnest(genus) %>%
  group_by(ID) %>% 
  mutate(col=seq_along(ID)) %>% 
  spread(key=col, value=genus) %>%
  ungroup() %>% 
  distinct(food_name_orig, .keep_all = TRUE)

######Include info in data

#Include common name
data2 = data_orig_t %>%
  left_join(common_wide, by=c("food_name" = "food_name_orig")) 


#Include preparation types
data2 = data2 %>% 
  left_join(prep_clean, by=c("food_name" = "food_name_orig")) %>% 
  mutate(food_prep = na_if(food_prep, "frozen"),
         food_prep = na_if(food_prep, "unknown preparation"),
         food_prep = na_if(food_prep, "small sample")) %>% 
  rename(food_prep_org = food_prep) %>% 
  mutate(food_prep = if_else(is.na(food_prep_org), preparation_simple, food_prep_org)) %>%
  rename(food_prep_detailed = preparation_detailed) %>% 
  select(-food_prep_org, -preparation_simple)

#Include fish parts
data2 = data2 %>% 
  left_join(parts_clean, by=c("food_name" = "food_name_orig")) %>% 
  mutate(food_part = na_if(food_part, "unknown part"),
         food_part = na_if(food_part, "edible"),
         food_part = na_if(food_part, "raw"),
         food_part = na_if(food_part, "small sample"),
         food_part = recode(food_part, "gutted" = "whole gutted")) %>% 
  rename(food_part_org = food_part) %>% 
  mutate(food_part = if_else(is.na(food_part_org), part_simple, food_part_org)) %>%
  rename(food_part_detailed = part_detailed) %>% 
  select(-food_part_org, -part_simple)

#Include wild vs farmed
data2 = data2 %>% 
  left_join(catg_clean, by=c("food_name" = "food_name_orig")) %>% 
  mutate(prod_catg = na_if(prod_catg, "unknown")) %>% 
  rename(prod_catg_org = prod_catg) %>% 
  mutate(prod_catg = if_else(is.na(prod_catg_org), catg, prod_catg_org)) %>%
  select(-prod_catg_org, -catg)

#Include other ingredients
data2 = data2 %>% 
  left_join(other_ingredients_clean, by=c("food_name" = "food_name_orig"))

#Organize columns
data2 = data2 %>% 
  select("food_name", "food_name_orig", "fct_code_orig", 
         "taxa_name", "taxa_name_source", "kingdom", "phylum", "class",
         "order", "family", "genus", "taxa_id", "taxa_db", "common_name", "common_name_detailed",
         "food_prep", "food_prep_detailed", "food_part", "food_part_detailed", "prod_catg",
         "other_ingredients", "study_type", "study_id", "iso3", "country", 
         "edible_prop", "notes", "nutrient_type", "nutrient", "nutrient_orig",
         "nutrient_desc", "nutrient_code_fao", "nutrient_units", "value")         

# Export data
saveRDS(data2, file=file.path(outdir, "AFCD_data_pass2.Rds"))  

