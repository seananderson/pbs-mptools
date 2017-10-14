library(GFASTeR) # devtools::install_github("seananderson/GFASTeR") # (private)
library(PBSmptools) # devtools::install_github("seananderson/pbs-mptools")
# source("pbs-mptools-master/R/db_connection.R")
# source("pbs-mptools-master/R/age_length.R")
# library(DBI)

species <- tolower(c("Squalus suckleyi", "sebastes borealis",
  "Sebastes babcocki", "Microstomus pacificus",
  "Glyptocephalus zachirus", "Hydrolagus colliei",
  "Sebastes alutus"))
surv_codes <- c(4, 1, 3, 16, 2, 32, 7, 6, 14, 36, 22, 40, 39, 41, 42, 43)
surv_names <- tolower(c("WCVI", "QCS", "HS_syn", "QCHG", "HS_assembl",
  "GB_Reed", "WCVI_Shrimp", "QCS", "IPHC", "PHMA_S",
  "PHMA_N", "IRF_S", "IRF_N", "Sable_inlet", "Sable_offshore",
  "Sable_strat"))
surv <- data.frame(surv_codes, surv_names, stringsAsFactors = FALSE)
ids <- expand.grid(surv_codes = surv_codes, species = species,
  stringsAsFactors = FALSE)
ids <- dplyr::left_join(ids, surv)

# Survey indices:
dat_indices <- purrr::map_df(seq_len(nrow(ids)), function(i) {
  x <- surveyIndices(get_species_id(ids$species[i]),
    ids$surv_codes[i])
  x$survey_id <- ids$surv_names[i]
  x$species <- ids$species[i]
  x
})

# Age-length from survey samples:
dat_age_length <- purrr::map_df(seq_len(nrow(ids)), function(i) {
  x <- PBSmptools::get_age_length(ids$species[i], ids$surv_codes[i])
  if (nrow(x) > 0) {
    x$survey_id <- ids$surv_names[i]
    x$species <- ids$species[i]
  } else {
    x <- data.frame()
  }
  x
})

# Ages for surveys and commercial:
surv_codes <- c(4, 1, 3, 16, -1, -2)
surv_names <- tolower(c("WCVI", "QCS", "HS", "WCHG", "Com_Trawl", "Com_LL_Trap"))
surv <- data.frame(surv_codes, surv_names, stringsAsFactors = FALSE)
ids <- expand.grid(surv_codes = surv_codes, species = species,
  stringsAsFactors = FALSE)
ids <- dplyr::left_join(ids, surv)

dat_age_sex <- purrr::map_df(seq_len(nrow(ids)), function(i) {
  x <- ageAndSex(get_species_id(ids$species[i]),
    ids$surv_codes[i])
  if (nrow(x) > 0) {
    x$survey_id <- ids$surv_names[i]
    x$species <- ids$species[i]
  } else {
    x <- data.frame()
  }
  x
})

# Catches from 1996 onwards:
dat_catch <- purrr::map_df(seq_along(species), function(i) {
  x <- catchByYear(get_species_id(species[i]))
  if (nrow(x) > 0) {
    x$species <- species[i]
  } else {
    x <- data.frame()
  }
  x
})

save(dat_indices, dat_age_length, dat_age_sex, dat_catch,
  file = "gfast-om-species.rda")

library(ggplot2)
ggplot(dat_catch, aes(year, trawl_landed_kg)) + geom_line() +
  facet_wrap(~species, scales = "free_y")

ggplot(dat_catch, aes(year, trawl_discarded_kg)) + geom_line() +
  facet_wrap(~species, scales = "free_y")

ggplot(dat_catch, aes(year, nontrawl_landed_kg)) + geom_line() +
  facet_wrap(~species, scales = "free_y")

ggplot(dat_catch, aes(year, nontrawl_discarded_kg)) + geom_line() +
  facet_wrap(~species, scales = "free_y")

ggplot(dat_catch, aes(year, nontrawl_landed_pcs)) + geom_line() +
  facet_wrap(~species, scales = "free_y")

ggplot(dat_catch, aes(year, nontrawl_discarded_pcs)) + geom_line() +
  facet_wrap(~species, scales = "free_y")

ggplot(dat_age_sex, aes(year, age)) + geom_point(alpha = 0.1) +
  facet_grid(survey_id~species, scales = "free_y")

ggplot(dat_age_sex, aes(year, age)) + geom_point(alpha = 0.2) +
  facet_grid(survey_id~species, scales = "free_y")

ggplot(dat_age_length, aes(age, length)) + geom_point(alpha = 0.2) +
  facet_grid(survey_id~species, scales = "free")

ggplot(dat_indices, aes(year, biomass)) + geom_line() +
  facet_grid(species~survey_id, scales = "free_y") +
  geom_ribbon(aes(ymin = lowerci, ymax = upperci), fill = "#00000060", col = NA)
