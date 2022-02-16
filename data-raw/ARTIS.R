source("R/species_nutrients.R")

spp = read_csv("C:/Users/use/Documents/Fisheries Nutrition Modeling/data/unique_taxa_list_20220106.csv")

spp_vec = unique(spp$species)

x = species_nutrients(sci_name = spp_vec,
                      prep = c("raw",
                               "frozen",
                               "small sample"),
                      part = c("muscle tissue", 
                               "edible", 
                               "raw", 
                               "small sample"),
                      nut = c("Iron, total", 
                              "Zinc", 
                              "Calcium", 
                              "Total fatty acids, polyunsaturated",
                              "Protein, total; calculated from total nitrogen",
                              "Vitamin A; sum of retinol/carotenoids",
                              "Vitamin B12"))

x = x %>% 
  select(species, nutrient, nutrient_units, value, ssd, count, se, lower_ci, upper_ci, taxa_match)

spp = spp %>% 
  left_join(x)

##Fill missing value by ISCAAP group
##Calculate confidence intervals
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

spp_complete = spp %>% 
  filter(!is.na(value))

spp_ISCAAP = spp_complete %>% 
  group_by(isscaap_group, nutrient, nutrient_units) %>% 
  summarize(smean = mean(value),
            ssd = sd(value),
            count = n()) %>% 
  mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count)) %>% 
  ungroup() %>% 
  rename(value = smean)

missing_nut = spp %>% 
  filter(is.na(value)) %>% 
  select(-value, -ssd, -count, -se, -lower_ci, -upper_ci, -taxa_match) %>% 
  left_join(spp_ISCAAP) %>% 
  mutate(taxa_match = "ISSCAAP group")

final_ARTIS = rbind(spp_complete, missing_nut)

write.csv(final_ARTIS, "C:/Users/use/Documents/unique_taxa_list_20220106_nutrients.csv", row.names = F)

# > unique(AFCD$food_part)
# [1] "whole"               "muscle tissue"       "unknown part"       
# [4] "edible"              "roe"                 "reproductive tissue"
# [7] "bone"                "viscera"             "head"               
# [10] "raw"                 "small sample"        "blubber"            
# [13] "flippers"            "skin"                "liver"              
# [16] "heart"               "blade"               "mantle"             
# [19] "body wall"           "frond"               "gills"              
# [22] "larvae"              "gutted"              "combination"        
# [25] "stipe"               "oil"                 "kidney"             
# [28] "gelatin"             "holdfast"     

# missing_ARTIS = x %>% 
#   filter(is.na(value))

# > unique(AFCD$food_prep)
# [1] "smoked"              "frozen"              "raw"                
# [4] "baked"               "unknown preparation" "boiled steamed"     
# [7] "canned"              "dried"               "small sample"       
# [10] "salted"              "fried"               "curried"            
# [13] "grilled"             "freeze dried"        "cooked"             
# [16] "microwaved"          "aged"                "acid digestion"   

#1634
#1576
#1541

sci_name = spp_vec
prep = "raw"
part = "muscle tissue"
nut = c("Iron, total", 
        "Zinc", 
        "Calcium", 
        "Total fatty acids",
        "Protein, total; calculated from total nitrogen",
        "Vitamin A; sum of retinol/carotenoids",
        "Vitamin B12")