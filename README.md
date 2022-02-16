# Aquatic Foods Composition Database

## Overview

This R package contains a cleaned version of the Aquatic Foods Composition Database (AFCD) developed by [Golden et al. (2016)](https://www.nature.com/articles/s41586-021-03917-1):

* Golden CD**, Koehn JZ**, Shepon A**, Passarelli S**, Free CM**, Viana DF**, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. _Nature_ 598: 315-320. _** denotes shared first authorship_

## Installation

The "AFCD" R package can be installed from GitHub with:

``` r
# Run if you don't already have devtools installed
install.packages("devtools")

# Run once devtools is successfully installed
devtools::install_github("Aquatic-Food-Composition-Database/AFCD", force=T)
library(AFCD)
```
## Functions

1. Match nutritional content to nearest taxa: ?species_nutrients

## Datasets

The package contains the following datasets:

1. Aquatic Foods Composition Database (foods ID'ed by scientific names): `?afcd1`
2. Aquatic Foods Composition Database (foods ID'ed by general food names only): `?afcd2`
3. Aquatic Foods Composition Database nutrient key: `?afcd_nutrients`
4. Aquatic Foods Composition Database reference key: `?afcd_refs`

The data were accessed from this [Harvard Dataverse repository](https://dataverse.harvard.edu/dataverse/afcd) with additional information accessed via this [GitHub repository](https://github.com/zachkoehn/aquatic_foods_nutrient_database). The data were formatted by [Chris Free](https://marine.rutgers.edu/~cfree/). Please contact Chris Free (cfree14@gmail.com) with questions about this repository and the original authors with questions about the data or paper.

## How were the data cleaned?

The data were cleaned by doing the following:

1. Converting from wide to long format to (1) clarify nutrient identity and units and (2) ease plotting and analysis
2. Filling in missing nutrient information (e.g., missing units)
3. Harmonizing nutrient names and descriptions
4. Validating and harmonizing taxonomic information
5. Adding a harmonized common name corresponding to the scientific name
6. Building a species taxonomic reference key to reduce columns and enforce consistency
7. Building a single reference key merging meta-data on FCT and peer-reviewed sources
8. Carefully harmonizing columns across the data and keys

## Citation

Please reference the original paper when using this data:

* Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. _Nature_ 598: 315-320.
