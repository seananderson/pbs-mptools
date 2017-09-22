#' Get a species ID given a species name
#' 
#' @param species_name A species name 
#' @param con A connection object to a database
#'
#' @examples
#' \dontrun{
#' get_species_id("sebastes borealis")
#' get_species_id("Sebastes borealis")
#' get_species_id("SebAstEs BORealis")
#' head(get_species_id())
#' }
#' @export
get_species_id <- function(species_name = NULL, con = db_connection()) {
  lookup <- dbGetQuery(con, "
    SELECT SPECIES_SCIENCE_NAME AS species, 
    SPECIES_CODE AS species_id from SPECIES")
  lookup$species <- tolower(lookup$species)
  if (!is.null(species_name)) {
    out <- as.numeric(unique(lookup$species_id[
      lookup$species == tolower(species_name)]))
    out[!is.na(out)]
  } else {
    lookup
  }
}

#' Get a species name given a species ID
#' 
#' @param species_id A species ID code 
#' @param con A connection object to a database
#'
#' @examples
#' \dontrun{
#' get_species_name(403)
#' head(get_species_name())
#' }
#' @export
get_species_name <- function(species_id = NULL, con = db_connection()) {
  lookup <- dbGetQuery(con, "
    SELECT SPECIES_SCIENCE_NAME AS species, 
    SPECIES_CODE AS species_id from SPECIES")
  lookup$species <- tolower(lookup$species)
  if (!is.null(species_id)) {
    lookup$species[lookup$species_id == species_id]
  } else {
    lookup
  }
}

#' Get a survey ID given a survey name
#' 
#' Uses \code{\link[base]{grep}}
#' 
#' @param survey_name A survey name
#' @param con A connection object to a database
#'
#' @examples
#' \dontrun{
#' get_survey_id("1989 Hecate Strait Multispecies")
#' get_survey_id("Hecate Strait Multispecies")
#' get_survey_id("hecate strait mULTI")
#' get_survey_id("haida")
#' head(get_survey_id())
#' }
#' @export
get_survey_id <- function(survey_name = NULL, con = db_connection()) {
  lookup <- dbGetQuery(con, "
    SELECT SURVEY_DESC AS survey_name, 
    SURVEY_SERIES_ID AS survey_id from SURVEY")
  if (!is.null(survey_name)) {
    out <- lookup$survey_id[
      grep(tolower(survey_name), tolower(lookup$survey_name))]
    as.numeric(unique(out))
  } else {
    lookup
  }
}

#' Get a survey name given a survey ID
#' 
#' @param survey_id A survey ID code 
#' @param con A connection object to a database
#'
#' @examples
#' \dontrun{
#' get_survey_name(2)
#' head(get_survey_name())
#' }
#' @export
get_survey_name <- function(survey_id = NULL, con = db_connection()) {
  lookup <- dbGetQuery(con, "
    SELECT SURVEY_DESC AS survey_name, 
    SURVEY_SERIES_ID AS survey_id from SURVEY")
  if (!is.null(survey_id)) {
    out <- lookup$survey_name[lookup$survey_id == survey_id]
    unique(out)
  } else {
    lookup
  }
}
