###############################################################################
## Title: How to save tables from a SQLite database as individual CSVs`
## Author: Karin Kralicek (karin.kralicek@usda.gov)
## Date: 04/11/2022
##
## About: 
## - Work-around for issue with Datamart downloads for CSVs, which are giving a
##   404 error at present.
###############################################################################
library(DBI)

# Before running the code below download SQLite db for a particular state from
# the Datamart & unzip:
# - https://apps.fs.usda.gov/fia/datamart/datamart_sqlite.html
# - This script uses Colorado as an example.

# Set path to where the SQLite is located
path_db <- "C:/Users/karinkralicek/Downloads/SQLite_FIADB_CO/"

# Set path to where you want the CSVs to appear
path_out <- "C:/Users/karinkralicek/Downloads/"

# Take a reference to the db (again, using CO as the example)
con <- dbConnect(RSQLite::SQLite(), 
                 paste0(path_db, "FIADB_CO.db"))

# grab all the table names
# - alternatively, subset this to only those tables you want
#   e.g. `db_table_names <- c("SURVEY", "PLOT")`
db_table_names <- dbListTables(con)

# iterate through the table names and write out a csv for each table in the db
lapply(db_table_names, function(x) {
  write.csv(dbReadTable(x, conn = con),
            file = paste0(path_db, "CO_", x, ".csv"))
})

# close connection
dbDisconnect(con)