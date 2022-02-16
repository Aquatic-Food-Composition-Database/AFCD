
# Build taxonomic key
################################################################################

# Taxa key 1
taxa_key1 <- spp_key2 %>%
  select(taxa_type, taxa_level, sciname) %>%
  unique()

# Species to look up
spp_do <- taxa_key1 %>% filter(taxa_level=="species") %>% pull(sciname)
worms_ids <- get_wormsid(spp_do, ask=F, messages=F)



# Get SeaLifeBase FishBase ones first
taxa_key_slb <- freeR::taxa(spp_do)
taxa_key_slb1 <- taxa_key_slb %>%
  left_join(taxa_key1, by="sciname") %>%
  select(sciname, everything())

# Species to do next
spp_do1 <- spp_do[!spp_do %in% taxa_key_slb1$sciname]

# Loop through and look up taxonomy
taxa_key_worms <- purrr::map_df(spp_do1, function(x){

  # Get taxonomy
  spp_taxa <- taxize::classification(x, db = "worms")

  # Format data
  spp_taxa2 <- spp_taxa[[1]] %>%
    # Add scienitific name
    mutate(sciname=x)

})




# Format key
taxa_key3 <- taxa_key2 %>%
  # Reduce to ranks of interest
  mutate(rank=tolower(rank)) %>%
  filter(rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species")) %>%
  # Simplify
  select(sciname, rank, name) %>%
  # Spread
  spread(key="rank", value='name')




