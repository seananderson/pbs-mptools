#' Make a database connection
#' 
#' @param server The server name
#' @param database The database name
#' @export

db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}
