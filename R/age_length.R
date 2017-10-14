#' Get age and length data for a given species and survey
#' 
#' @param species_id The species number (numeric) or name (character)
#' @param survey_id The survey number (numeric) or name (character)
#' @param filter_na Remove NAs?
#' @param con A connection object to a database
#' 
#' @importFrom DBI dbGetQuery
#' @export
#' 
#' @examples
#' \dontrun{
#' get_age_length(403, 16)
#' }
#' 
#' @return 
#' A data frame with ages, lengths, sexes, and dates

get_age_length <- function(species_id, survey_id, filter_na = TRUE,
  con = db_connection()) {
  
  if (is.character(species_id)) 
    species_id <- get_species_id(species_id)
  
  if (is.character(survey_id)) 
    survey_id <- get_survey_id(survey_id)
  
  age_length <- dplyr::as.tbl(DBI::dbGetQuery(con, paste0("
    SELECT YEAR(TRIP_START_DATE) AS year,
    SPECIMEN_SEX_CODE AS sex,
    SPECIMEN_AGE AS age,
    CAST(ROUND(Best_Length / 10.0, 1) AS DECIMAL(8,1)) AS length
    FROM GFBioSQL.dbo.SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_GROUPING SG ON
    S.SURVEY_ID = SG.SURVEY_ID
    INNER JOIN GFBioSQL.dbo.FISHING_EVENT_GROUPING FEG ON
    SG.GROUPING_CODE = FEG.GROUPING_CODE
    INNER JOIN GFBioSQL.dbo.B21_Samples SM ON
    FEG.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
    INNER JOIN GFBioSQL.dbo.TRIP_SURVEY TS ON
    S.SURVEY_ID = TS.SURVEY_ID
    INNER JOIN GFBioSQL.dbo.FISHING_EVENT FE ON
    TS.TRIP_ID = FE.TRIP_ID AND
    FE.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
    INNER JOIN GFBioSQL.dbo.B22_Specimens SP ON
    SM.SAMPLE_ID = SP.SAMPLE_ID
    WHERE S.SURVEY_SERIES_ID = '", survey_id, "' AND
    SM.SPECIES_CODE = '", species_id, "' AND
    SP.SPECIMEN_SEX_CODE IN (1, 2)")))
  
  if (filter_na) {
    age_length <- age_length[!is.na(age_length$age) & !is.na(age_length$length), ]
  }
  age_length
}
