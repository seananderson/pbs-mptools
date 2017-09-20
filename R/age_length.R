#' Get age and length data for a given species and survey
#' 
#' @param species_id The species number
#' @param survey_id The survey number
#' @param filter_na Remove NAs?
#' @param con A connection object to a database
#' 
#' @importFrom dplyr inner_join filter select collect tbl rename mutate
#' @importFrom rlang .data
#' @importFrom magrittr %>%
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
  
  age_length <- tbl(con, "SURVEY") %>%
    filter(.data$SURVEY_SERIES_ID == as.character(survey_id)) %>%
    inner_join(tbl(con, "SURVEY_GROUPING"), by = "SURVEY_ID") %>%
    inner_join(tbl(con, "FISHING_EVENT_GROUPING"), by = "GROUPING_CODE") %>%
    inner_join(tbl(con, "B21_Samples") %>%
        filter(.data$SPECIES_CODE == as.character(species_id)),
      by = "FISHING_EVENT_ID") %>%
    inner_join(tbl(con, "TRIP_SURVEY"), by = "SURVEY_ID") %>%
    inner_join(tbl(con, "FISHING_EVENT"), by = c("TRIP_ID", "FISHING_EVENT_ID")) %>%
    inner_join(tbl(con, "B22_Specimens"), by = "SAMPLE_ID") %>%
    filter(.data$SPECIMEN_SEX_CODE %in% c(1, 2)) %>%
    select(
      .data$SPECIMEN_SEX_CODE, 
      .data$SPECIMEN_AGE, 
      .data$Best_Length, 
      .data$TRIP_START_DATE) %>%
    collect(n = Inf)
  
  if (filter_na) {
    age_length <- age_length %>% 
      filter(!is.na(.data$SPECIMEN_AGE), !is.na(.data$Best_Length))
  }
  
  age_length <- rename(age_length,
    sex = .data$SPECIMEN_SEX_CODE, 
    age = .data$SPECIMEN_AGE,
    length = .data$Best_Length,
    date = .data$TRIP_START_DATE) %>%
    mutate(length = round(.data$length / 10, 1))
  
  age_length
}
