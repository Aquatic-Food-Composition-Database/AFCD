# Aquatic Foods Composition Database

## Overview

This R package contains a cleaned version of the Aquatic Foods Composition Database (AFCD) developed by [Golden et al. (2021)](https://www.nature.com/articles/s41586-021-03917-1):

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

1. Match nutritional content to nearest taxa: `?species_nutrients`

## Datasets

The package contains the following datasets:

1. Aquatic Foods Composition Database (foods ID'ed by scientific names): `?afcd1`
2. Aquatic Foods Composition Database (foods ID'ed by general food names only): `?afcd2`
3. Aquatic Foods Composition Database nutrient key: `?afcd_nutrients`
4. Aquatic Foods Composition Database reference key: `?afcd_refs`

## Citation

Please reference the original paper when using this data:

* Golden CD, Koehn JZ, Shepon A, Passarelli S, Free CM, Viana DF, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) Aquatic foods to nourish nations. _Nature_ 598: 315-320.
