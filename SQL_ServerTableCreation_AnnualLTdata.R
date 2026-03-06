###############################  HEADER  ######################################
#  TITLE: SQL_ServerTableCreation_AnnualLTdata.R
#  DESCRIPTION: Sends SQL Transact commands to SQL Server to make tables
#  AUTHOR(S): Dan Crocker, Max Nyquist
#  DATE LAST UPDATED: 2021-5-7
#  GIT REPO:
#  R version 4.0.0 (2020-04-24)  i386
##############################################################################.

### LOAD PACKAGES ####
  pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "glue")
  sapply(pkg, library, character.only = TRUE)

### Eliminate Scientific Notation
options(scipen = 999)

### Path to project files
sqlserver_dir <- "W:/WatershedJAH/EQStaff/WaterQualityMonitoring/Database/SQL Server"
ltdata_dir <- "W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Lake Trout/Wachusett Lake Trout Tagging Program/Data"
### change this folder each year when new data arrives from MassWildlife 
ltdata_year_dir <- paste0(ltdata_dir, "/2020 Data")


### Steps to take in MS Access:
# For each database - go to Database Tools --> Database Documenter
# Under the Tables tab --> Select All
# Go to options: Include for Table (Nothing checked), Include for Fields (Names, Data Types, and Sizes), Include for Indexes (nothing)
# Click OK to build the report
# Export to Rich Text file
# In word - find and change to "" all info in page headers and the word Columns.
# Copy formatted word doc into xlsx doc. Add columns to the right with formulas to construct the final SQL that will create each table
# Complete the migration for each table by following the script below.

########################################################################.
###                  READ SCHEMA EXCEL FILE AND FORMAT              ####
########################################################################.

### Set the database back end to be migrated (a local copy w/o password protection is recommended) written by DC. Not used anymore? 
#db <- "C:/WQDatabase/AqBioDBWachusett_be.mdb"
### Set the schema where this data will go in SQL Server - This is critical... Quabbin should never use Wachusett schema and vice versa or else we will mess up each others data
schema <- "Wachusett" # Quabbin
### Set the schema excel file that was created from this database
### This schema has the correct SQL in it, if it is needed. MN. 
xl_file <- paste0(sqlserver_dir,"/LT_Database_Schema_updated.xlsx")
### Read the file
xl <- readxl::read_excel(xl_file, na = "-")
### Eliminate blank lines
xl <- xl[!is.na(xl$Final_SQL),]
# Find table breaks using string match for Table: then remove that part
x <- str_detect(xl$Final_SQL, pattern = "Table:")
xl$Final_SQL <- str_replace_all(xl$Final_SQL, pattern = "Table: ", replacement = "")

### Set vector of final SQL lines
final_sql <- xl$Final_SQL
### Get Index of Table Names
y <- which(x)
### Make List of Tables:
tbls <- final_sql[y]
### Add last row to index
y <- c(y, length(x))
### View listing of tables:
view(tbls)

########################################################################.
###       FOR EACH TABLE... BUILD SQL, CREATE TABLE, ADD DATA       ####
########################################################################.
### This is to build SQL and create the table initially, this has already been done. Should not be needed, except to select tbl i. MN.  
### Pick a table as i
i <- 3
### Set the table name
tbl_i <- final_sql[y[i]]
### Find the first column
col_first <- as.numeric(y[i] + 1)
### Find the last column
col_last <- as.numeric(y[i+1] - 1)
### Make a vector of the columns for the table
cols_i <- final_sql[col_first:col_last] #last col for secchi is 278
# Combine all columns into single string
col_def <- str_c(final_sql[col_first:col_last], collapse = ", ")

sql_create <- glue("CREATE TABLE {schema}.{tbl_i}({col_def});")
sql_create ### Check the statement
# ### DATABASE OPERATIONS ####
# 
# ### ONLY RUN ONCE ####
# 
#           ### Connect to the DWSP database in SQL Server
#           # database <- "DCR_DWSP"
#           # con <- dbConnect(odbc::odbc(), database, timezone = 'America/New_York') # this does not work for JTL see next line
#           # con <- DBI::dbConnect(odbc::odbc(),
#           #                         Driver = "ODBC Driver 17 for SQL Server",
#           #                         Server = "eea-sql-prd-002.env.govt.state.ma.us",
#           #                         Database = "DCR_DWSP",
#           #                         Trusted_Connection = "TRUE",
#           #                         uid = "JTrahan-Liptak",
#           #                         pwd = "PASSWORD", ## **** DON'T FORGET PASSWORD!! ****
#           #                         port = 1443)
#           
# Connect to the DWSP database
database <- "DCR_DWSP"
con <- dbConnect(odbc::odbc(), database, timezone = 'America/New_York')
# If you look in the Connections pane, you will see our database DCR_DWSP - We only have access to this database
###
# 
# # See what tables are in this schema
dbListTables(con, schema_name = schema)

### Create the table ###
dbGetQuery(con, sql_create)  # IMPORTANT!

### Confirm it is there - it should now be listed but contain 0 rows
dbListTables(con, schema_name = schema)

### Check the table and make sure it looks right. If not then drop the table and modify and re-import
df_check <- dbGetQuery(con, glue("SELECT * FROM [{schema}].[{tbl_i}]"))
str(df_check)

# ### Drop a table ####
# # dbGetQuery(con, glue("DROP table [{schema}].[{tbl_i}]")) #IMPORTANT -USE WITH CAUTION - This one deletes the entire table
# # dbGetQuery(con, glue("TRUNCATE table [{schema}].[{tbl_i}]")) #IMPORTANT -USE WITH CAUTION - This one keeps the table, but wipes out all rows
# 
### GET THE DATA TO MIGRATE ####
    # db is set on line 30

    ### Important notes
    ### - Must be using 32 bit R (i386) to connect to MS Access
    ### - beware of timezone shifts for date/date-time columns - they may need to be fixed!
    con2 <- dbConnect(odbc::odbc(),
                      .connection_string = paste('driver={Microsoft Access Driver (*.mdb)}',
                                                 paste0('DBQ=', db), 'Uid=;Pwd=;', sep = ';'),
                      # timezone = 'UTC') # Reconnect using this line if importing data in UTC Timezone
                      timezone = 'America/New_York')
    ### See the tables
    tables <- dbListTables(con2) %>% print()
    # tables <- tables[-c(1:13)] # Filter out the Sys tables (Might need to alter depending on the DB)
    tables <- tables[-c(1:16)] # Filter out the Sys tables (for nutrients, tables 1-16 are sys tables (I think))
    tables

### Fetch entire table i
data_i <- dbReadTable(con2, tables[17]) # check number in tables - different from i in AB database
### Make sure the correct table is pulled
### verify data structure to make sure it aligns with new schema
str(data_i)

### TABLE STRUCTURE CHANGES ####

### Sort
data_i <- arrange(data_i, data_i$ID)

### Col Names (MUST MATCH COL NAMES IN SQL SERVER! -  EXACTLY!) This will do the job...
new_col_names <- dbListFields(con, schema_name = schema, name = tbl_i) 
names(data_i) <- new_col_names

### Dates:
date_cols <- 3
date_time_cols <- 3
### Date only
data_i[,date_cols] <- format(data_i[,date_cols], tz ="America/New_York", usetz = TRUE) %>%
  as_date()
### Date with Time
data_i[, date_time_cols] <- format(data_i[, date_time_cols], tz ="America/New_York", usetz = TRUE) %>%
  lubridate::as_datetime()

# # ### For Secchi 
# SampleTime change to character
# character_cols <- 8
# data_i[,character_cols] <- as.character(str_extract_all(data_i[,character_cols],
#                                                         "[0-9]{2}:[0-9]{2}:[0-9]{2}"))
# data_i[799,6] <- "JC, MN, JS UMass" # this field violated 20 char limit, edited from "Josh from UMass"

########################################################################.
###             IMPORT DATA TO SQL SERVER                           ####
########################################################################.

# Appending to a table:
import <- function(catalog, schema, tbl, df){
  time_start <- Sys.time()

  ### Import the table
  # Option 1
  odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{tbl}")), value = df, append = TRUE)

  # Option 2
  # t <- DBI::Id(catalog = database, schema = schema, table = tbl_i)
  # odbc::dbWriteTable(con, t, value = df_import, append = TRUE)

  time_end <- Sys.time()
  return(print(paste0("Elapsed time: ", time_end - time_start)))

}

### Run the function
import(catalog = database, schema = schema, tbl = tbl_i, df = data_i)


### see if the data was written successfully
df_check <- dbReadTable(con, Id(schema = schema, table = tbl_i))
# Should match the number of records in data_i


### MOVE TO NEXT TABLE AND REPEAT STEPS STARTING AT LINE 67

### Always disconnect and rm connection when done with databases
dbDisconnect(con)
rm(con)

dbDisconnect(con2)
rm(con2)

########################################################################.
###                       Other useful DB stuff                     ####
########################################################################.

### Return list of column names
ColumnsOfTable <- dbListFields(con, schema_name = schema, name = "tblNOAA")

### Get data types of columns

### Check if table exists
dbExistsTable(my_table)

### Set a connection to a table object
my_table <- tbl(con, in_schema("Wachusett", "tbl_W_Fish"))
### Then use the object as if it were in R
my_table %>% head()


COL_TYPES <- function(con, schema, table){
  
  con <- con
  schema <- schema
  table <- my_table
  my_table
  send_sql <- glue("SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = '{schema}' and TABLE_NAME = '{table}';")
  dbGetQuery(con, send_sql)
}

COL_TYPES(con, schema = schema, table = "tbl_Phyto")


