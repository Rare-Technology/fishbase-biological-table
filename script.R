library(rfishbase)
library(dplyr)
library(tidyr)

# Get the fishbase Genus-Species info for all fish
fishbase_gensp <- rfishbase::load_taxa() %>% 
  dplyr::select(Family, Genus, Gensp = Species, speccode = SpecCode)

# Using the filtered fishbase info, get Lmax for all fish
lmax_info <- rfishbase::length_length() %>% 
  dplyr::select(Gensp = Species, LengthMax) %>% 
  dplyr::mutate(lmax = as.numeric(LengthMax)) %>% 
  dplyr::group_by(Gensp) %>% 
  dplyr::summarize(Lmax = max(lmax, na.rm = TRUE)) %>%
  dplyr::mutate(Lmax = ifelse(is.infinite(Lmax), NA, Lmax)) %>% 
  tidyr::separate(Gensp, c('Genus', 'Species'), sep = ' ', remove = FALSE) %>% 
  dplyr::left_join(fishbase_gensp %>% 
    dplyr::select(Family, Gensp),
    by = "Gensp"
  )

# Aggregate means of Lmax across genus
genus_Lmax_info <-  lmax_info %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(genus_Lmax = mean(Lmax, na.rm = TRUE))

family_Lmax_info <- lmax_info %>% 
  dplyr::group_by(Family) %>% 
  dplyr::summarize(family_Lmax = mean(Lmax, na.rm = TRUE))

lmax_info <- lmax_info %>% 
  dplyr::select(Gensp, Lmax)

# Impute lmax data
lmax_table <- dplyr::left_join(fishbase_gensp, lmax_info, by = 'Gensp') %>% 
  dplyr::left_join(genus_Lmax_info, by = "Genus") %>% 
  dplyr::left_join(family_Lmax_info, by = "Family")

lmax_table <- lmax_table %>% 
  dplyr::mutate(
    lmax = ifelse(is.na(Lmax),
      ifelse(is.na(genus_Lmax),
        family_Lmax,
        genus_Lmax
      ),
      Lmax
    )
  )

ab_info <- rfishbase::poplw() %>% 
  dplyr::select(Gensp = Species, a, b) %>% 
  dplyr::group_by(Gensp) %>% 
  dplyr::summarize(
    a = mean(a, na.rm = TRUE),
    b = mean(b, na.rm = TRUE)
  ) %>% 
  tidyr::separate(Gensp, c("Genus", "Species"), sep = " ", remove = FALSE)

genus_ab_info <- ab_info %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(
    genus_a = mean(a, na.rm = TRUE),
    genus_b = mean(b, na.rm = TRUE)
  )

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
  )

lmax_table <- dplyr::left_join(lmax_table, ab_table, by = 'Gensp')

lmax_table <- lmax_table %>% 
  dplyr::select(
    speccode, Gensp, Family, lmax, a, b
  )