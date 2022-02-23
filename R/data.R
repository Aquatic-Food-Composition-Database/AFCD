#' Aquatic Foods Composition Database (AFCD) - all taxa
#'
#' A cleaned version of the Aquatic Foods Composition Database (AFCD). This dataset includes observations with foods identified by scientific names or broad taxa (Genus, Family, Order or Class).
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{sciname}{Scientific name, harmonized}
#'   \item{sciname_orig}{Scientific name, in the raw AFCD}
#'   \item{genus}{species Genus}
#'   \item{family}{species Family}
#'   \item{order}{species Order}
#'   \item{class}{species Class}
#'   \item{common_name}{species English common name}
#'   \item{food_name}{Name of food, in English}
#'   \item{food_name_orig}{Name of food, in original data}
#'   \item{fct_code_orig}{FCT code, in original data}
#'   \item{food_id}{Food id}
#'   \item{common_name_detailed}{different versions of species English common name}
#'   \item{food_prep}{Preparation of food}
#'   \item{food_prep_detailed}{Preparation of food with more detailed description}
#'   \item{food_part}{Part of food}
#'   \item{food_part_detailed}{Part of food with more detailed description}
#'   \item{prod_catg}{Production category (farmed, wild capture)}
#'   \item{other_ingredients}{added ingredients}
#'   \item{study_type}{Study type (peer-reviewed paper or FCT report)}
#'   \item{study_id}{Study id}
#'   \item{iso3}{ISO3 of source country(s)}
#'   \item{country}{Source country(s)}
#'   \item{fao3}{FAO code of source country(s)}
#'   \item{edible_prop}{Edible proportion}
#'   \item{notes}{Notes}
#'   \item{nutrient_type}{Nutient type}
#'   \item{nutrient}{Nutient name}
#'   \item{nutrient_orig}{Nutient name, in the raw AFCD}
#'   \item{nutrient_desc}{Description of nutrient}
#'   \item{nutrient_code_fao}{FAO nutrient code}
#'   \item{nutrient_units}{Units per 100g of edible food portion}
#'   \item{value}{Value}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd"

#' Aquatic Foods Composition Database (AFCD) - scientific names
#'
#' A cleaned version of the Aquatic Foods Composition Database (AFCD). This dataset includes observations with foods identified by scientific names.
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{sciname}{Scientific name, harmonized}
#'   \item{sciname_orig}{Scientific name, in the raw AFCD}
#'   \item{genus}{species Genus}
#'   \item{family}{species Family}
#'   \item{order}{species Order}
#'   \item{class}{species Class}
#'   \item{common_name}{species English common name}
#'   \item{food_name}{Name of food, in English}
#'   \item{food_name_orig}{Name of food, in original data}
#'   \item{fct_code_orig}{FCT code, in original data}
#'   \item{food_id}{Food id}
#'   \item{common_name_detailed}{different versions of species English common name}
#'   \item{food_prep}{Preparation of food}
#'   \item{food_prep_detailed}{Preparation of food with more detailed description}
#'   \item{food_part}{Part of food}
#'   \item{food_part_detailed}{Part of food with more detailed description}
#'   \item{prod_catg}{Production category (farmed, wild capture)}
#'   \item{other_ingredients}{added ingredients}
#'   \item{study_type}{Study type (peer-reviewed paper or FCT report)}
#'   \item{study_id}{Study id}
#'   \item{iso3}{ISO3 of source country(s)}
#'   \item{country}{Source country(s)}
#'   \item{fao3}{FAO code of source country(s)}
#'   \item{edible_prop}{Edible proportion}
#'   \item{notes}{Notes}
#'   \item{nutrient_type}{Nutient type}
#'   \item{nutrient}{Nutient name}
#'   \item{nutrient_orig}{Nutient name, in the raw AFCD}
#'   \item{nutrient_desc}{Description of nutrient}
#'   \item{nutrient_code_fao}{FAO nutrient code}
#'   \item{nutrient_units}{Units per 100g of edible food portion}
#'   \item{value}{Value}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd_sci"

#' Aquatic Foods Composition Database (AFCD) - w/out taxa information
#'
#' A cleaned version of the Aquatic Foods Composition Database (AFCD). This dataset includes observations with foods identified by general names (not scientific names).
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{common_name}{species English common name}
#'   \item{food_name}{Name of food, in English}
#'   \item{food_name_orig}{Name of food, in original data}
#'   \item{fct_code_orig}{FCT code, in original data}
#'   \item{food_id}{Food id}
#'   \item{common_name_detailed}{different versions of species English common name}
#'   \item{food_prep}{Preparation of food}
#'   \item{food_prep_detailed}{Preparation of food with more detailed description}
#'   \item{food_part}{Part of food}
#'   \item{food_part_detailed}{Part of food with more detailed description}
#'   \item{prod_catg}{Production category (farmed, wild capture)}
#'   \item{other_ingredients}{added ingredients}
#'   \item{study_type}{Study type (peer-reviewed paper or FCT report)}
#'   \item{study_id}{Study id}
#'   \item{iso3}{ISO3 of source country(s)}
#'   \item{country}{Source country(s)}
#'   \item{fao3}{FAO code of source country(s)}
#'   \item{edible_prop}{Edible proportion}
#'   \item{notes}{Notes}
#'   \item{nutrient_type}{Nutient type}
#'   \item{nutrient}{Nutient name}
#'   \item{nutrient_orig}{Nutient name, in the raw AFCD}
#'   \item{nutrient_desc}{Description of nutrient}
#'   \item{nutrient_code_fao}{FAO nutrient code}
#'   \item{nutrient_units}{Units per 100g of edible food portion}
#'   \item{value}{Value}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd_common"

#' Aquatic Foods Composition Database (AFCD) nutrient key
#'
#' Nutrient key for the Aquatic Foods Composition Database (AFCD).
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{nutrient_type}{Nutrient type (e.g., mineral, vitamin, carbohydrate, etc.)}
#'   \item{nutrient}{Nutrient name}
#'   \item{nutrient_units}{Nutrient units}
#'   \item{nutrient_code_fao}{FAO nutrient code}
#'   \item{nutrient_desc}{Description of nutrient and value}
#'   \item{n}{Number of observations}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd_nutrients"

#' Aquatic Foods Composition Database (AFCD) reference key
#'
#' Reference key for the Aquatic Foods Composition Database (AFCD). The AFCD utilizes a mixture of values from Food Composition Tables (FCTs) and from the peer-reviewed literature.
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{study_type}{Study type (Peer-reviewed literature or FCT table)}
#'   \item{study_id}{Study id}
#'   \item{citation}{Citation}
#'   \item{doi}{DOI for peer-reviewed sources and dataset link for FCT sources}
#'   \item{region}{Study region (peer-reviewed only)}
#'   \item{database}{Nuutrient database (FCT sources only)}
#'   \item{units}{Nutrients per unit (FCT sources only)}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd_refs"

#' Aquatic Foods Composition Database (AFCD) food parts key
#'
#' Parts key for the Aquatic Foods Composition Database (AFCD).
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{food_part}{Food part type}
#'   \item{n}{Number of observations}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd_parts"

#' Aquatic Foods Composition Database (AFCD) preparation key
#'
#' Preparation key for the Aquatic Foods Composition Database (AFCD).
#'
#' @format A data frame with the following attributes:
#' \describe{
#'   \item{food_prep}{Preparation type}
#'   \item{n}{Number of observations}
#' }
#' @source Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. Nature 598: 315-320.
"afcd_prep"
