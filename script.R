library(rfishbase)
library(dplyr)
library(tidyr)

# =================== TAXONOMY =================== #
### Get taxonomy info
taxa <- rfishbase::load_taxa(server = "fishbase") %>% 
  dplyr::select(SpecCode, Species, Genus, Family) %>% 
  dplyr::mutate(server = "fishbase") %>% 
  dplyr::bind_rows(
    rfishbase::load_taxa(server = "sealifebase") %>% 
      dplyr::select(SpecCode, Species, Genus, Family) %>% 
      dplyr::mutate(server = "sealifebase")
  )
# > taxa
# # A tibble: 131,941 x 5
# SpecCode   Species                Genus       Family        server  
# <int>      <chr>                  <chr>       <chr>         <chr>   
# 1      972 Ablennes hians         Ablennes    Belonidae     fishbase
# 2    46918 Aboma etheostoma       Aboma       Gobiidae      fishbase
# 3    67299 Aborichthys cataracta  Aborichthys Nemacheilidae fishbase
# 4    24516 Aborichthys elongatus  Aborichthys Nemacheilidae fishbase
# 5    24517 Aborichthys garoensis  Aborichthys Nemacheilidae fishbase

# =================== LMAX =================== #
### Get Lmax
suppressWarnings({
  lmax_info <- rfishbase::length_length(server = "fishbase") %>% 
    dplyr::select(Species, LengthMax) %>% 
    dplyr::bind_rows(
      rfishbase::length_length(server = "sealifebase") %>% 
        dplyr::select(Species, LengthMax)
    ) %>% 
    dplyr::mutate(lmax = as.numeric(LengthMax)) %>% 
    dplyr::group_by(Species) %>% 
    dplyr::summarize(Lmax = max(lmax, na.rm = TRUE)) %>%
    dplyr::mutate(Lmax = ifelse(is.infinite(Lmax), NA, Lmax)) %>% 
    dplyr::left_join(taxa, by = "Species")
})
# > lmax_info
# # A tibble: 131,941 x 6
# Species                       Lmax SpecCode Genus             Family          server     
# <chr>                         <dbl>   <int> <chr>             <chr>           <chr>      
# 1 Aapticheilichthys websteri    NA    64588 Aapticheilichthys Procatopodidae  fishbase   
# 2 Aaptolasma americana          NA    32307 Aaptolasma        Bathylasmatidae sealifebase
# 3 Aaptolasma brintoni           NA    32306 Aaptolasma        Bathylasmatidae sealifebase
# 4 Aaptolasma callistoderma      NA    32308 Aaptolasma        Bathylasmatidae sealifebase
# 5 Aaptolasma leptoderma         NA    32304 Aaptolasma        Bathylasmatidae sealifebase

### Take aggregate means and create bio table
genus_Lmax_info <- lmax_info %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(genus_Lmax = mean(Lmax, na.rm = TRUE))

family_Lmax_info <- lmax_info %>% 
  dplyr::group_by(Family) %>% 
  dplyr::summarize(family_Lmax = mean(Lmax, na.rm = TRUE))

lmax_table <- lmax_info %>% 
  dplyr::rename(species_Lmax = Lmax) %>% 
  dplyr::left_join(genus_Lmax_info, by = "Genus") %>% 
  dplyr::left_join(family_Lmax_info, by = "Family")

# Fill in missing lmax's
bio_table <- lmax_table %>% 
  dplyr::mutate(
    lmax = ifelse(is.na(species_Lmax),
      ifelse(is.na(genus_Lmax),
        family_Lmax,
        genus_Lmax
      ),
      species_Lmax
    )
  ) %>% dplyr::select(
    SpecCode, Species, Genus, Family, lmax, server
  )
# > bio_table
# # A tibble: 131,941 x 6
#   SpecCode Species                    Genus             Family            lmax server    
#      <int> <chr>                      <chr>             <chr>            <dbl> <chr>     
# 1    64588 Aapticheilichthys websteri Aapticheilichthys Procatopodidae    3.20 fishbase  
# 2    32307 Aaptolasma americana       Aaptolasma        Bathylasmatidae NaN    sealifeba…
# 3    32306 Aaptolasma brintoni        Aaptolasma        Bathylasmatidae NaN    sealifeba…
# 4    32308 Aaptolasma callistoderma   Aaptolasma        Bathylasmatidae NaN    sealifeba…
# 5    32304 Aaptolasma leptoderma      Aaptolasma        Bathylasmatidae NaN    sealifeba…

# =================== a/b COEFFICIENTS =================== #
### Get a/b coefficients
ab_info <- rfishbase::poplw(server = "fishbase") %>% 
  dplyr::select(Species, a, b) %>% 
  dplyr::bind_rows(
    rfishbase::poplw(server = "sealifebase") %>% 
      dplyr::select(Species, a, b)
  ) %>% 
  # Some species have multiple a/b values so take the mean
  dplyr::group_by(Species) %>% 
  dplyr::summarize(
    a = mean(a, na.rm = TRUE),
    b = mean(b, na.rm = TRUE)
  ) %>% 
  dplyr::left_join(taxa, by = "Species")
# > ab_info
# # A tibble: 131,941 x 7
# Species                       a     b SpecCode Genus           Family        server    
# <chr>                     <dbl> <dbl>    <int> <chr>           <chr>         <chr>     
# 1 Aapticheilichthys webs…   NaN   NaN    64588 Aapticheilicht… Procatopodid… fishbase  
# 2 Aaptolasma americana      NaN   NaN    32307 Aaptolasma      Bathylasmati… sealifeba…
# 3 Aaptolasma brintoni       NaN   NaN    32306 Aaptolasma      Bathylasmati… sealifeba…
# 4 Aaptolasma callistoder…   NaN   NaN    32308 Aaptolasma      Bathylasmati… sealifeba…
# 5 Aaptolasma leptoderma     NaN   NaN    32304 Aaptolasma      Bathylasmati… sealifeba…

### Aggregate a/b coefficients at the genus level only
genus_ab_info <- ab_info %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(
    genus_a = mean(a, na.rm = TRUE),
    genus_b = mean(b, na.rm = TRUE)
  )

### Fill in missing a/b coefficients, add to bio table
ab_table <- ab_info %>%
  dplyr::left_join(genus_ab_info, by = "Genus")

ab_table <- ab_table %>% 
  dplyr::mutate(
    a = ifelse(is.na(a),
      genus_a,
      a
    ),
    b = ifelse(is.na(b),
      genus_b,
      b
    )
  ) %>% 
  dplyr::select(Species, a, b)

bio_table <- dplyr::left_join(bio_table, ab_table, by = "Species") %>%
  dplyr::select(SpecCode, Species, Genus, Family, lmax, a, b, server)
# > bio_table
# # A tibble: 131,941 x 8
# SpecCode Species                Genus         Family         lmax     a     b server   
#    <int>   <chr>                <chr>         <chr>         <dbl> <dbl> <dbl> <chr>    
# 1    64588 Aapticheilichthys w… Aapticheilic… Procatopodi…   3.20   NaN   NaN fishbase 
# 2    32307 Aaptolasma americana Aaptolasma    Bathylasmat… NaN      NaN   NaN sealifeb…
# 3    32306 Aaptolasma brintoni  Aaptolasma    Bathylasmat… NaN      NaN   NaN sealifeb…
# 4    32308 Aaptolasma callisto… Aaptolasma    Bathylasmat… NaN      NaN   NaN sealifeb…
# 5    32304 Aaptolasma leptoder… Aaptolasma    Bathylasmat… NaN      NaN   NaN sealifeb…

# =================== TROPHIC LEVEL =================== #
### Get trophic levels
troph_info <- rfishbase::ecology(server = "fishbase") %>% 
  dplyr::select(Species, DietTroph) %>% 
  dplyr::bind_rows(
    rfishbase::ecology(server = "sealifebase") %>% 
      dplyr::select(Species, DietTroph)
  ) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarize(species_trophic_level = mean(DietTroph, na.rm=TRUE)) %>% 
  dplyr::left_join(taxa, by = 'Species')

### Aggregate at family and genus level
family_troph <- troph_info %>% 
  dplyr::group_by(Family) %>% 
  dplyr::summarize(family_trophic_level = mean(species_trophic_level, na.rm = TRUE))

genus_troph <- troph_info %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(genus_trophic_level = mean(species_trophic_level, na.rm = TRUE))

### Fill in missing trophic levels with genus/family means
trophic_table <- troph_info %>% 
  dplyr::left_join(family_troph, by = "Family") %>% 
  dplyr::left_join(genus_troph, by = "Genus") %>% 
  dplyr::mutate(
    # Try to use the species trophic level.
    # If it's NA, use genus trophic level.
    # If that's NA, use family trophic level, which will then be used regardless if NA
    trophic_level = ifelse(is.na(species_trophic_level),
      ifelse(is.na(genus_trophic_level),
        family_trophic_level,
        genus_trophic_level
      ),
      species_trophic_level
    )
  ) %>% 
  dplyr::select(Species, trophic_level)

### Add trophic level to bio table
bio_table <- dplyr::left_join(bio_table, trophic_table, by = "Species") %>% 
  dplyr::select(SpecCode, Species, Genus, Family, lmax, a, b, trophic_level, server)
# > bio_table
# # A tibble: 131,941 x 9
# SpecCode Species          Genus      Family      lmax     a     b trophic_level server 
# <int>    <chr>            <chr>      <chr>      <dbl> <dbl> <dbl>         <dbl> <chr>  
# 1    64588 Aapticheilich… Aaptichei… Procatop…   3.20   NaN   NaN           NaN fishba…
# 2    32307 Aaptolasma am… Aaptolasma Bathylas… NaN      NaN   NaN           NaN sealif…
# 3    32306 Aaptolasma br… Aaptolasma Bathylas… NaN      NaN   NaN           NaN sealif…
# 4    32308 Aaptolasma ca… Aaptolasma Bathylas… NaN      NaN   NaN           NaN sealif…
# 5    32304 Aaptolasma le… Aaptolasma Bathylas… NaN      NaN   NaN           NaN sealif…