################################### HEADER ###################################
#   TITLE: Lake Trout Import 
#   DESCRIPTION: This script is used to add Lake Trout data sent by 
#   MassWildlife into the DCR LT Tagging Database. Raw data are recorded on 
#   paper field sheets (photo documentation by DCR), and then entered by MassWildlife.
#   At the end of each season the data are sent, usually in excel format, to DCR. 
#   Use this script to connect to the database, perform QAQC on the MassWildlife data, 
#   and import data into the database. 
#   AUTHOR(S): Max Nyquist
#   DATE LAST UPDATED: 2/5/2020
#   R VERSION: R-Portable
##############################################################################.




### POSSIBLE REVISIONS ####
# Create sections for major tasks of all dataframes. Removal of unwanted columns. Data/class formats. Time formatting. Rounding. 
# Duplicate testing not until the end, unless duplicate catching is a part of building the dataframe. Reconsider data frame build to eliminate 
# unnecessary code. Build dataframe into the existing (albeit empty) database table. Create data folder structure for raw/processed data? Prepare 
# for future spreadsheets in different formats. 
#
### LOAD PACKAGES ####
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc")
sapply(pkg, library, character.only = TRUE)

### SET OPTIONS/ENV VARIABLES ####

### Eliminate Scientific Notation
options(scipen = 999)

### SOURCE DATA/FUNCTIONS/FILES ####
### Config filepath 
# R_Config <- read.csv("//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WaterQualityMonitoring/R-Shared/Configs/R_Config.csv", header = TRUE)
# config <- as.character(R_Config$CONFIG_VALUE)
### Directory for all Lake Trout Data sent from MassWildlife 
ltdata_dir <- "W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Lake Trout/Wachusett Lake Trout Tagging Program/Data"
### Annual Data 
### change this folder each year when new data arrives from MassWildlife 
### How can we avoid a script update each year?  

ltdata_year_dir <- paste0(ltdata_dir, "/2020 Data")
ltdata_year_dir_2019 <- paste0(ltdata_dir, "/2019 Data")





### FETCH CACHED DATA FROM WAVE RDS FILES ####
### Lake Trout Access database. No longer using this, but was necessary component in creating lt.rds  
# datadir <- config[18]
# ### Make a list of all the .rds files using full path
rds_files <- list.files(ltdata_dir,full.names = TRUE ,pattern = "\\.rds$")
rds_files # Take a look at list of files
# 
# ### Select which rds files needed for this script
# rds_in <- c(3,4,7:9)

### subset rds files (if you want all of them then skip rds_in and the following line)
# rds_files <- rds_files[rds_in]

### create an object that contains all of the rds files
### Only one .rds for LT data  
data <- lapply(rds_files, readRDS)

### Make a list of the df names by eliminating extension from files
 df_names <- gsub(".rds", "", list.files(ltdata_dir, pattern = "\\.rds$"))
# df_names <- df_names[rds_in]
# name each df in the data object appropriately
 names(data) <- df_names

### Extract each element of the data object into the global environment
 list2env(data ,.GlobalEnv)

### Remove data
 rm(data)

### FETCH ANNUAL DATA FOR QAQC ####
### 'df1' and 'df2' are workarounds to avoid updating portions of the script each year. Generic names.  
data <- list.files(ltdata_year_dir, full.names = TRUE, pattern = "\\.csv$")
data2019 <- list.files(ltdata_year_dir_2019, full.names = TRUE, pattern = "\\.csv$")
 
data <- lapply(data, read_csv) 
df1 <- as.data.frame(data[[1]])
df2 <- as.data.frame(data[[2]])
# 
data2019 <- lapply(data2019, read_csv) 
df12019 <- as.data.frame(data2019[[1]])
df22019 <- as.data.frame(data2019[[2]])
# 

# names <- gsub(".csv", "", list.files(ltdata_year_dir, pattern = "\\.csv"))
# names(data) <- names
# 
# list2env(data,.GlobalEnv)
### Make a list of the df names by eliminating extension from files
### useful, but does not allow for the script to be run without annual modifications. 
### could save the files we receive from MassWildlife as "LT_data.csv" and "Sample_LT_data.csv" and put things in a 
### 'Current Year' directory.  
#df_annual_names <- gsub(".csv", "", list.files(ltdata_year_dir, pattern = "\\.csv$"))
# df_names <- df_names[rds_in]
# name each df in the data object appropriately
#names(annual_data) <- df_annual_names

### Extract each element of the data object into the global environment
#list2env(annual_data ,.GlobalEnv)






### CONNECT TO A FRONT-END DATABASE ####
### This portion of the script interacts with the LT Access database. Migrating away from this.  
### Set DB
# db <- config[18]
# ### Connect to Database 
# con <- dbConnect(odbc::odbc(),
#                  .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
#                                             paste0("DBQ=", db), "Uid=Admin;Pwd=;", sep = ";"),
#                  timezone = "America/New_York")
# ### See the tables 
# tables <- dbListTables(con)  
# tables
# ### Fetch an entire table (avoid using SQL queries - just pull entire tables and subset in R)
# df.qwfish <- dbReadTable(con, "Q&W Fish Table")
# df.qwsample <- dbReadTable(con, "Q&W Sample Table")
# df.wfish<- dbReadTable(con, "Wachusett Fish Table")
# df.wsample<- dbReadTable(con, "Wachusett Sample Table")
# query.df <- dbReadTable(con, "Wachusett Fish Info Query")
# df <- dbReadTable(con, "Q&W Table")
# 
# ### create vector of column names from db Table ####
# dbcolnames <- paste(names(df))
# ### Always disconnect and rm connection when done with db
# dbDisconnect(con)
# rm(con)
# rm(R_Config)

### Create vector of column names from current .rds ####
colnames <- paste(names(ltdata))
#"Fish_ID"              "sample_id"            "waterbody"            "sample_date"          "fish_code"           
# "gender"               "length"               "weight"               "run_num"              "tag_num"             
# "Unique_ID"            "tagging_location"     "location_description" "comments"             "saris_palis"

### SCRIPT BEGIN ####
### join length frequency data with sample data by sample_id. 
### Figure out how to name these two data frames differently upon import. Nice function to automatically add name from 
### the directory, but do not want the year (e.g. 2020) in the script. This requires an annual update of the script (not desireable) 
df1 <- df1 %>% 
  rename(Fish_ID = ID) 
df1 <- df1[, (names(df1) %in% colnames)]
df2 <- df2[, (names(df2) %in% colnames)]
df2 <- df2 %>% select(-"weight")


### for 2019 data 
df12019 <- df12019[, (names(df12019) %in% colnames)]
df12019 <- df12019 %>% select(-"weight") 
df12019$sample_date <- mdy(df12019$sample_date)
df22019 <- df22019 %>% rename(Fish_ID = id, location = LOCATION, comments = COMMENTS) 
df22019 <- df22019[, (names(df22019) %in% colnames)]


#df2 <- df2 #%>% 
#  select(., c("sample_id","waterbody","sample_date","location_description","saris_palis")) #%>% 
#  select(-"weight")
#annual_data2019 <- left_join(df1, df2, by = "sample_id")
annual_data <- left_join(df1, df2, by = "sample_id")
annual_data$sample_date <-  mdy(annual_data$sample_date)

#annual_data <- rename(annual_data, Fish_ID = ID)
wach_data <- ltdata %>% 
  filter(waterbody == "Wachusett Reservoir") #%>% 
  filter(year(sample_date) != 2019)

wach_data2019 <- ltdata %>% 
  filter(waterbody == "Wachusett Reservoir") %>% 
  filter(year(sample_date) == 2019) %>% 
  rename(sample_id = Fish_ID, Fish_ID = sample_id)

wach_data2019 <- left_join(wach_data2019, df12019, by = "sample_date") %>% 
  transmute(wach_data2019, sample_id = ifelse(is.na(sample_id.x), sample_id.y, sample_id.x))


annual_data <- bind_rows(annual_data, wach_data2019)
#left_join(df1, df2, by="y") %>% 
#  transmute(y, x1 = ifelse(is.na(x1.y), x1.x, x1.y))
    

    
names(wach_data)
names(annual_data)

df <- bind_rows(wach_data, annual_data)

rm(df1)
rm(df2)
rm(wach_data)
rm(ltdata)
rm(test)
rm(data)
rm(df12019)
rm(df22019)
rm(wach_data2019)
rm(data2019)

### w.19.df created below 
# df <- bind_rows(df, w.19.df)
# 
# ### identify duplicates 
# dupecheck <- which(duplicated(df$Fish_ID))
# dupes <- df$Fish_ID[dupecheck]
# view(dupes)
# duplicate <- duplicated(df.fish$Fish_ID)
# sum(duplicate)
# sum(is.na(test$Fish_ID))
# 
# 
# 
# 
# test <- test %>% distinct()

### bind rows of w.fishtbl to qw.fishtbl
### create vector of col names from df.wfish to apply to df.qwfish. easier to rbind 
# w.fish.colnames <- names(df.wfish) 
# colnames(df.qwfish) <- w.fish.colnames
# ### unique to df.qwfish. Removes NA tagging locations from df. solution to later problem.  
# df.qwfish <- df.qwfish[!c(df.qwfish$tagging_location == "NA"), ]

### Combine Quabbin and Wachusett fish dataframes and remove duplicates ####

### bind qw and w fish tables from database. There will be duplicates. solving the problem of an update to w table only and need to recombine 
df.fish <- rbind(df.qwfish, df.wfish)
### keep only distinct rows  
df.fish <- df.fish %>% distinct()

### identify duplicates 
dupecheck <- which(duplicated(df.fish$Fish_ID))
dupes <- df.fish$Fish_ID[dupecheck]
#view(dupes)
duplicate <- duplicated(df.fish$Fish_ID)
sum(duplicate)
### create copy of data frame to isolate duplicates
dup <- df.fish
### remove all duplicate records  
df.fish <- df.fish[ !df.fish$Fish_ID %in% dupes, ]
### remove if Fish_ID is in dupes AND matches the conditions below  

# ###  group by and filter the remaining duplicates 
# dup <- dup %>%
#   group_by(Fish_ID) %>%
#   filter(n()>1 ) #%>%
# filter(!is.na(weight) | weight == 650 | weight == 1800 | tagging_location == "South Dike")  

### select correct duplicate values and drop duplicate Fish_ID rows that contain incorrect data 
dup <- dup %>% 
  group_by(Fish_ID) %>% 
  filter(n()>1) %>% 
  filter(!is.na(weight) | weight == 650 | weight == 1800 | tagging_location == "South Dike") %>% 
  filter(!tagging_location == "s.carville rock wall" & !weight == 1650 & !weight == 180) %>% 
  as.data.frame()

### bind former duplicate values with data back to dataframe with distinct values only  
df.fish <- rbind(df.fish, dup)

### clean up sample dataframes ####

df.wsample <- df.wsample %>% 
  select(sample_id, saris_palis, waterbody, sample_date, town, location_description, latitude, longitude, method, crew_names, habitat_comment) %>% 
  subset(format(sample_date, "%Y") == 2018)
df.wsample$waterbody <- "Wachusett Reservoir"
sample_names <- names(df.wsample)
# t <- df.wsample %>% 
#   group_by(year(sample_date), waterbody) %>% 
#   summarise(n())


df.sample <- df.qwsample %>% 
  select(Sample_ID, saris_palis, Waterbody, Date, Town, Location.Description, Latitude, Longitude, Method, Crew.Names, Habitat.Comment) %>% 
  set_colnames(sample_names) %>% 
  rbind(df.wsample) %>% 
  rename(comments = habitat_comment  )
r <- df.sample %>% 
  group_by(year(sample_date), waterbody) %>% 
  summarise(n())



duplicates <- which(duplicated(df.sample$sample_id))
dupe <- df.sample$sample_id[duplicates]
sum(duplicates)

### remove unnecessary data frames ####

# rm(df.qwfish)
# rm(df.wfish)
# rm(query.df)
# rm(df.qwsample)
# rm(df.wsample)
# rm(dup)
# rm(test)
# rm(test1)
# rm(r)

### junk code (mostly) ####

# 
# ### remove if Fish_ID is in dupes AND matches the conditions below  
# test <- df.fish
# ###  group by and filter the remaining duplicates 
# test2 <- test %>% 
#   group_by(Fish_ID) %>% 
#   filter(n()>1 ) #%>%
#   # filter(!is.na(weight) | weight == 650 | weight == 1800 | tagging_location == "South Dike")  
# 
# ### select correct duplicate values and drop duplicate Fish_ID rows that contain incorrect data 
# test5 <- test2 %>% 
#   group_by(Fish_ID) %>% 
#   filter(!is.na(weight) | weight == 650 | weight == 1800 | tagging_location == "South Dike") %>% 
#   filter(!tagging_location == "s.carville rock wall" & !weight == 1650 & !weight == 180) %>% 
#   as.data.frame()
# 



# 
# test3 <- test %>% 
#   group_by(Fish_ID) %>% 
#   filter(n()>1 ) %>% 
#   filter(is.na(weight) | weight == 1650 | weight == 180 | tagging_location == "s.carville rock wall")
# 



# 
# 
# ### use this to create a dataframe showing all duplicated records 
# test1 <- df.fish %>% 
#   group_by(Fish_ID) %>% 
#   filter(n()>1)
# ### remove rows that are complete duplicates of other rows  
# test2 <- test1 %>%
#   distinct 
# 


### need to "flag" or create data frame where Fish_ID is a duplicate AND the data in any of the columns differs. If the data does not differ, we can just 
### remove one of the duplicates. If data does differ, we need to keep the record with more data supplied to each column. E.G. tagging_location removal below.  


###remove rows with NA as tagging_location 

 





# ### use this for alternative duplicate testing. Creates too many dataframes 
# dup <- data.frame(as.numeric(duplicated(df.fish$tag_num)))
# colnames(dup) <- c("dup")
# df2 <- cbind(df.fish, dup)
# df3 <- subset(df2, dup == 1)
# 
# 
# dupecheck <- which(duplicated(df.fish$Fish_ID))
# dupes <- df.fish$Fish_ID[dupecheck]
# view(dupes)
### Create dataframes from new annual data ####
w.19.df <- read_csv(paste0(ltdata_dir, "/2019 Data/lt_WACHUSETT_2019.csv"), col_names = TRUE)
#q.19.df <- read_csv(paste0(getwd(), "/Data/2019 Data/quabbin_lt_2019.csv"), col_names = TRUE)


### create vector of column names from data frame. remove unneccessary columns  
### double check exactly what the "id" means in the Wachusett 2019 data set. It is not similar to saris_palis, sample_id, or fish_id, but was set to sample_id 

### remove unnecessary columns  
w.19.df <- w.19.df %>% 
  select(-c("tag1", "tag2", "tag3")) %>% 
  mutate(waterbody = "Wachusett Reservoir")
  
### rename columns  
names(w.19.df) = c("Fish_ID",
                   "sample_date",
                   "fish_code",
                   "length",
                   "weight", 
                   "gender", 
                   "run_num",
                   "tag_num",
                   "tagging_location", 
                   "comments", 
                   "waterbody")

q.19.df <- q.19.df %>% 
  select(-c("Existing AdClip?", "year", "palis")) %>% 
  mutate(waterbody = "Quabbin Reservoir")
### rename columns  
names(q.19.df)= c("sample_date",
                  "sample_id", 
                  "run_num",
                  "tagging_location", 
                  "fish_code",
                  "length",
                  "weight", 
                  "gender", 
                  "tag_num",
                  "comments", 
                  "waterbody")
### reorder columns  

#as.Date(as.character(q.19.df$sample_date), format = "%Y-%M-%D")
q.19.df$sample_date <- mdy(q.19.df$sample_date)
#q.19.df <- q.19.df[, c(2, 1, 5:8,3, 9, 4, 10) ]


### bind rows  
df.19 <- rbind(w.19.df, q.19.df)




### Join 2014-2018 fish and sample tables  ####
###one_of works here
df <- left_join(df.fish, df.sample, by = "sample_id" ) %>% 
  mutate(Unique_ID = "") %>% 
  select(one_of(dbcolnames)) #%>% 
  
df$sample_date <- as.Date(as.POSIXct(df$sample_date))
df$run_num <- as.numeric(df$run_num)
df$gender[is.na(df$gender)] <- "NA"
df$gender[df$gender == "NA"] <- NA          
df$gender[df$gender == "F?"] <- "F"
df$gender[df$gender == ""]
df$tag_num[df$tag_num=="NA"] <- NA

### create dataframe for database table ####
  
#df <- bind_rows(df, df.19)
### temporary, for LT_analysis.R until dataframe is imported  
#dflt <- df

#ltdata <- saveRDS(df, file =  "ltdata_update.rds")

data <- readRDS("ltdata.rds")

ltrds <- saveRDS(df, file = paste0(ltdata_dir, "/ltdata_2020.rds"))

########################################################################.
###                              Final Reformatting                 ####
########################################################################.
### MN: I do not know what this below script does. Written by DC.  
### Deselect Collumns that do not need in Database ####

### Reorder remaing __ columns to match the database table exactly ####

col.order.w.fishtbl <- dbListFields(con, w.fishtbl)
w.19.df <- w.19.df[,col.order.df.w.fish]

### Create a list of the processed datasets ####
dfs <- list()
dfs[[1]] <- w.19.fish.df
dfs[[2]] <- path
dfs[[3]] <- df.flags
dfs[[4]] <- unmatchedtimes 

dbDisconnect(con)
rm(con)
return(dfs)

### from DC WIT ImportMWRA.R ####

########################################################################.
###                              Write data to Database             ####
########################################################################. 
# 
# IMPORT_DATA <- function(df.wq, df.flags = NULL, path, file, filename.db, processedfolder, ImportTable, ImportFlagTable = NULL){
#   # df.flags is an optional argument
#   
#   con <-  odbcConnectAccess(filename.db)
#   
#   # Import the data to the database - Need to use RODBC methods here. Tried odbc and it failed
#   
#   ### WQ Data ####
#   ColumnsOfTable <- sqlColumns(con, ImportTable)
#   varTypes  <- as.character(ColumnsOfTable$TYPE_NAME)
#   sqlSave(con, df.wq, tablename = ImportTable, append = T,
#           rownames = F, colnames = F, addPK = F , fast = F, varTypes = varTypes)
#   
#   ### Flag data ####
#   if (class(df.flags) == "data.frame"){ # Check and make sure there is flag data to import 
#     sqlSave(con, df.flags, tablename = ImportFlagTable, append = T,
#             rownames = F, colnames = F, addPK = F , fast = F, verbose = F)
#   } else {
#     print("There were no flags to import")
#   }
#   
#   # Disconnect from db and remove connection obj
#   odbcCloseAll()
#   rm(con)
#   
#   ### Move the processed raw data file to the processed folder ####
#   processed_subdir <- paste0("/", max(year(df.wq$SampleDateTime))) # Raw data archived by year, subfolders = Year
#   processed_dir <- paste0(processedfolder, processed_subdir)
#   file.rename(path, paste0(processed_dir,"/", file))
#   return("Import Successful")
# }
# ### END ####
# 
# # IMPORT_DATA(df.wq, df.flags, path, file, filename.db, processedfolder)