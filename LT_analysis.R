################################### HEADER ###################################
#   TITLE: LT_analysis.R
#   DESCRIPTION: This script is used for Lake Trout data analysis. Results will
#   become a part of the Creel Survey Report or the Aquatic Biology sections of 
#   the Water Quality Annual Report. 
#   AUTHOR(S): Max Nyquist
#   DATE LAST UPDATED: 2/5/2020
#   R VERSION R-Portable
##############################################################################.

### LOAD PACKAGES ####
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "grDevices", "janitor", "FSA", "Hmisc", "modelr", "glue")
sapply(pkg, library, character.only = TRUE)


# ### SET OPTIONS/ENV VARIABLES ####
 
# ### Eliminate Scientific Notation
 options(scipen = 999)
# 
# ### SOURCE DATA/FUNCTIONS/FILES ####
# 
### This is not a  permanent solution 
ltdata_dir <- "C:/MassWildlifeDatabase/Fisheries Survey and Inventory Database.accdb"

db <- ltdata_dir
# ### Connect to Database 
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                                            paste0("DBQ=", db), "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

### See the tables 
tables <- dbListTables(con)  
tables

### Fetch tables 
sample_data <- dbReadTable(con, "sample_data")
fish_data <- dbReadTable(con, "fish_data")

### Disconnect db  
dbDisconnect(con)
rm(con)

### keep full data set in sample_data df 
sample <- sample_data %>% 
  filter(grepl("wachusett reservoir", waterbody, ignore.case = TRUE) & method == "Gillnet") %>% 
  select(-"weight")




### join Wachusett sample data and fish data  
fish_query <- inner_join(fish_data, sample, by = "sample_id")
 
### filter to only LT  
df <- fish_query %>% 
 filter(fish_code == "LT")

df <- annual_data %>% 
  filter(fish_code == "LT")


### Identify duplicates, updated 2/28/2022 ####
dupecheck <- which(duplicated(df$ID))
dupes <- df$ID[dupecheck]
dupes
# ### TO REPORT IN THE ANNUAL ####
#ltdata_dir <- "W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Lake Trout/Wachusett Lake Trout Tagging Program/Data"
# # for pulling in year by year 
# ltdata_year_dir <- paste0(ltdata_dir, "/2020 Data")
# 
# ### FETCH CACHED DATA FROM WAVE RDS FILES ####
# # datadir <- config[18]
# # ### Make a list of all the .rds files using full path
# rds_files <- list.files(ltdata_dir,full.names = TRUE ,pattern = "\\.rds$")
# rds_files # Take a look at list of files
# # 
# # ### Select which rds files needed for this script
# # rds_in <- c(3,4,7:9)
# 
# ### subset rds files (if you want all of them then skip rds_in and the following line)
# # rds_files <- rds_files[rds_in]
# 
# ### create an object that contains all of the rds files
# ### Only one .rds for LT data  
# data <- lapply(rds_files, readRDS)
# 
# ### Make a list of the df names by eliminating extension from files
# df_names <- gsub(".rds", "", list.files(ltdata_dir, pattern = "\\.rds$"))
# # df_names <- df_names[rds_in]
# # name each df in the data object appropriately
# names(data) <- df_names
# ### Extract each element of the data object into the global environment
# list2env(data ,.GlobalEnv)
# 
# ### Remove data
# rm(data)
#dflt <- readRDS("ltdata_2020.rds")

### SCRIPT BEGIN ####
########################################################################.
###                      Lake Trout Mark & Recapture Recap Analysis ####
########################################################################.
# dflt <- ltdata_2020
# dflt <- ltdata
dflt <- df
#identify recaps and group by


### begin recaps by year
#recaps_count same as recaps
recaps_count <- dflt %>% 
  group_by(tag_num) %>% 
  filter(n()>1) %>% 
  summarise(recap_event_total = n(), capture_years = toString(year(sample_date))) %>% 
  drop_na()  

unique(recaps_count$tag_num)

all_tagged_fish <- dflt %>% 
  select(tag_num, sample_date, gender, weight, length) %>% 
  mutate(year = year(sample_date), .keep = "all", .after = tag_num) %>% 
  filter(!tag_num == "NA") %>% 
  mutate(recap = duplicated(tag_num)) %>% 
  left_join(., recaps_count, by = c("tag_num"))


first_year_tagged <- all_tagged_fish %>% 
  group_by(tag_num) %>% 
  filter(sample_date == min(sample_date) )
dupecheck <- which(duplicated(first_year_tagged$tag_num))
dupes <- first_year_tagged$tag_num[dupecheck]
dupes


duplicate_catch <-count(all_tagged_fish[all_tagged_fish$recap == T, ])

#futher filter down to only the fish that have been recaptured, but include the original capture event 
recaps_data <- all_tagged_fish %>% 
  group_by(tag_num) %>% 
  filter(n() >1)
#the true number of tagged fish in the reservoir according to the data 
unique_tags <- all_tagged_fish %>% 
  distinct(tag_num)



#number of new fish tagged this year 
tagged_annual <- all_tagged_fish %>% 
  filter(year == as.numeric(year(Sys.Date())-1)) %>% 
  filter(recap == F)

tagged_year <- all_tagged_fish %>% 
  filter(recap == F) %>% 
  group_by(year(sample_date))# %>% 
  summarise(tagged = n())




### use this to test recaps  
proof_recaps <- dflt %>% 
  filter(tag_num == "00-069F-025C")




########################################################################.
###         Lake Trout Mark & Recapture Length/Weight Relationships ####
########################################################################.


### QUESTIONS #### 
# what is the influence of LT gender on weight length relationships? 
# #females caught are likely larger (especially in weight) than males 
# #supported by other dataset? 
# How do we correct for totals by year in box plots? More fish caught in certain years...

# How to facet wrap? 

### Create Dataframes ####
### dflt 



dflt <- df
### coerce data for graphing purposes  
dflt$gender[dflt$gender != "F" & dflt$gender != "M"]  <- "U/I"
dflt$gender[is.na(dflt$gender)] <- "U/I"
dflt$sex[dflt$gender == "F"] <- "Female"
dflt$sex[dflt$gender == "M"] <- "Male"
dflt$sex[dflt$gender == "U/I"] <- "Immature"
dflt$sex <- factor(dflt$sex, levels=c("Male", "Female", "Immature"))
### placeholder until sample date imported  
dflt$sample_date[is.na(dflt$sample_date)] <- "2022-11-01 04:00:00"
dflt$year <- as.factor(year(dflt$sample_date))
unique(dflt$gender)
### males only dfltm
dflt %>% 
  filter(gender == "M") %>% 
  group_by(year(sample_date)) %>% 
  summarise(caught = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
  rename(year = "year(sample_date)") %>% 
  ggplot(aes(year, mean_length))+
  geom_point()+
  labs(title = "Male Wachusett Lake Trout Mean Weight by Mean Length",
                   x = "Length (mm)", 
                   y = "Weight (g)")

dflt %>% 
  filter(gender == "M") %>% 
  ggplot(aes(x=year, y=length))+
  geom_boxplot()+
  labs(title = "Male Wachusett Lake Trout Length by Year",
       x = "Year", 
       y = "Length (mm)")


dflt %>% 
  filter(gender == "F") %>% 
  group_by(year(sample_date)) %>% 
  summarise(caught = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
  rename(year = "year(sample_date)") %>% 
  ggplot(aes(year, mean_length))+
  geom_point()
dflt %>% 
  filter(gender == "F") %>% 
  ggplot(aes(x=year, y=length))+
  geom_boxplot()+
  labs(title = "Female Wachusett Lake Trout Length by Year",
       x = "Year", 
       y = "Length (mm)")


dflt %>% 
  filter(gender == "M") %>% 
  group_by(year(sample_date)) %>% 
  summarise(caught = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
  rename(year = "year(sample_date)") %>% 
  ggplot(aes(year, mean_weight))+
  geom_point()
dflt %>% 
  filter(gender == "M") %>% 
  ggplot(aes(x=year, y=weight))+
  geom_boxplot()+
  labs(title = "Male Wachusett Lake Trout Weight by Year",
       x = "Year", 
       y = "Weight (g)")

dflt %>% 
  filter(gender == "F") %>% 
  group_by(year(sample_date)) %>% 
  summarise(caught = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
  rename(year = "year(sample_date)") %>% 
  ggplot(aes(year, mean_weight))+
  geom_point()
dflt %>% 
  filter(gender == "F") %>% 
  ggplot(aes(x=year, y=weight))+
  geom_boxplot()+
  labs(title = "Female Wachusett Lake Trout Weight by Year",
       x = "Year", 
       y = "Length (mm)")


dflt %>% 
  ggplot(aes(year, weight))+
  geom_boxplot()
dflt %>% 
  ggplot(aes(year, length))+
  geom_boxplot()

### tagged fish dfltag  
### remove NAs 
### not sure if this works any longer  

dfltmort <- dflt %>% 
  filter(is.na(tag_num)|tag_num=="NA") 

tally(dfltag)
tally(dfltmort) 


  
###  Possible totals table by year 
e <- dflt %>% 
    group_by(year(sample_date)) %>% 
    summarise(caught = n(), mean_weight = mean(weight, na.rm=T), mean_length = mean(length, na.rm = T),   
              cil = list(mean_cl_normal(length) %>% 
                           rename(meanl=y, lwrl=ymin, uprl=ymax)), 
              ciw = list(mean_cl_normal(weight) %>% 
                           rename(meanw=y, lwrw=ymin, uprw=ymax))) %>%  
    unnest(cols = c(cil, ciw)) %>% 
    mutate(freq = (caught/sum(caught)*100)) %>% 
    rename(year = 'year(sample_date)') 



### Possible totals table by year and gender   
d <- dflt %>% 
  group_by(year(sample_date),sex) %>% 
  summarise(caught = n(), mean_weight = mean(weight, na.rm=T), mean_length = mean(length, na.rm = T),   
            cil = list(mean_cl_normal(length) %>% 
            rename(meanl=y, lwrl=ymin, uprl=ymax)), 
            ciw = list(mean_cl_normal(weight) %>% 
            rename(meanw=y, lwrw=ymin, uprw=ymax))) %>%  
  unnest(cols = c(cil, ciw)) %>% 
  mutate(freq = (caught/sum(caught)*100)) %>% 
  rename(year = 'year(sample_date)') 
view(d)
 
f <- dflt %>% 
  group_by(sample_date, gender, run_num) %>% 
  summarise(caught = n())

view(f)



test <- dflt %>% 
  group_by(sample_date)%>% 
  summarise(sample_date_length = length(sample_date), caught = n())
view(test)  
test <- d  
test$gender[test$gender != "F" | test$gender != "M"]  <- "U/I"

# mortnotes$morts[mortnotes$morts == ""] <- 1 
  
# f <- dfltag %>% 
#     group_by(gender) %>% 
#     summarise(tagged = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
#     mutate(freq = (tagged/sum(tagged)*100)) %>% 
#     filter(gender=="F"|gender=="M")  
### plot  fish summary data by year ####
tag_weight <- ggplot(data = d, aes(year,mean_weight, fill=gender))+
  geom_point(aes(col=gender), size=3)+
  geom_line(aes(col=gender))+
  theme_bw()+
  geom_errorbar(aes(ymin=lwrw, ymax=uprw, col=gender), width=0.1)

tag_length <- ggplot(data = d, aes(year,mean_length, fill=gender))+
    geom_point(aes(col=gender), size=3)+
    geom_line(aes(col=gender))+
    theme_bw()+
    geom_errorbar(aes(ymin=lwrl, ymax=uprl, col=gender), width=0.1)
  
tag_prop <- ggplot(data = d, aes(x=factor(year), y=freq, fill=sex))+
  geom_bar(stat="identity", aes(col=sex), position = position_dodge2(width = 0.5, preserve = "single"), width = 0.6)+
  expand_limits(y=0)+  
  scale_y_continuous(expand = c(0,0), limits = c(0, 100))+
  labs(x="Year", y="Percentage of Total Lake Trout Caught")+
  scale_fill_manual("Sex", values = c("Female"= "#E69F00", "Male"= "#56B4E9", "Immature" = "#009E73" ))+
  scale_color_manual("Sex", values = c("Female"= "#E69F00", "Male"= "#56B4E9", "Immature" = "#009E73" ))+
theme(panel.background = element_rect(fill = 'white'),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_text(color = 'black', size = 12, face = 'bold'),
      axis.text.y=element_text(color = 'black', size = 12, face = 'bold'),
      axis.text.x=element_text(color='black', size=12, face = 'bold'),
      panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      panel.spacing  = unit(1, "lines"),
      strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
      strip.background = element_rect(fill = "#336600", colour = 'black'),
      plot.background = element_rect(fill = "white"),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
      plot.title = element_text(face = 'bold', size = 12), 
      legend.direction = "horizontal",
      legend.position  ="bottom",
      legend.text = element_text(color = 'black', size = 12),
      legend.title = element_blank())
      


tag_prop
ggsave("lt_proportions.jpg", plot = tag_prop, path = paste0(getwd(), "/", "plots"), width = 11.5, height = 5.7, units = "in", dpi = "print")






tag_caught <- ggplot(data = d, aes(year, caught, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")


tag_prop <- tag_prop+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(), 
        axis.text.x = element_text(size = 12, face = "bold"), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        axis.title = element_text(face = "bold")) 

tag_prop

ggsave("LTcaughtproportions2022.png", width = 10, height = 6, dpi = 300)



test <- dflt %>% 
  dplyr::filter(year(sample_date)==year_sys) %>% 
  group_by() %>% 
  summarise(mean_weight = mean(weight, na.rm=T), mean_ength = mean(length, na.rm = T)) 
  
test <- dflt %>% 
  group_by() %>% 
  summarise(mean_weight = mean(weight, na.rm=T), mean_ength = mean(length, na.rm = T)) 




tag_weight
tag_length
tag_caught


### plot tagged fish summary data by gender ####
tag_weight <- ggplot(data = d, aes(gender,mean_weight, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")

tag_length <- ggplot(data = d, aes(gender,mean_length, fill=gender))+
  geom_bar(aes(col=gender), size=3)

tag_prop <- ggplot(data = d, aes(gender, freq, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")

tag_caught <- ggplot(data = d, aes(gender, caught, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")


tag_prop
tag_weight
tag_length
tag_caught


### Log-transformed linear regression  ####
### log transform weight and length and put them into new columns  
dflt$logw <- log(dflt$weight)
dflt$logl <- log(dflt$length)

### create year column for factoring and plotting  
dflt$year <- year(dflt$sample_date)
### remove -Inf values  
dflt <- dflt %>% 
  filter(!logw=="-Inf")
### filter out single year to df  
dflt21 <- dflt %>% 
  filter(year(sample_date) == 2021)
dflt22 <- dflt %>% 
  filter(year(sample_date) == 2022) #%>% 
  filter(gender == 'F' | gender == 'M')

### checking distribution of LT length 
ggplot(data = dflt)+
  geom_histogram(mapping = aes(x = length), binwidth = 50)+
  labs(title = "Lake Trout Length", 
       subtitle = "",
       x = "Lake Trout Sex", 
       y = "Length (mm)")+
  facet_wrap( ~year)+
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()
### 2022  
ggplot(data = dflt22)+
  geom_histogram(mapping = aes(x = length), binwidth = 25)+
  labs(title = "Lake Trout Length", 
       subtitle = "",
       x = "Lake Trout Sex", 
       y = "Length (mm)")+
    theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()







dflt %>% count(cut_width(length, 5))
ggplot(data = dflt, mapping = aes(x = length, colour = as.factor(year)))+
  geom_freqpoly(binwidth = 25)


ggplot(data = dflt, mapping = aes(x=length))+
  geom_freqpoly(mapping = aes(colour = gender), binwidth= 25)
ggplot(dflt)+
  geom_bar(mapping = aes(x = gender))

ggplot(data=dflt, mapping = aes(x = length, y = ..density..))+
  geom_freqpoly(mapping = aes(colour = gender), binwidth = 25)

ggplot(data=dflt, mapping = aes(x = gender, y = length))+
  geom_boxplot()
dflt_exclude <- dflt %>% 
  filter(year!=2022)

start_yr <- min(unique(dflt$year), na.rm = T) %>% as.numeric()
reportYear <- 2022



ltlength_box_sep22 <-  
ggplot(data=dflt22, mapping = aes(x = sex, y = length))+
  geom_boxplot(data = dflt_exclude, mapping = aes(x = sex, y = length), outlier.shape = 1, fill = 'lightgray', outlier.size = 3)+
geom_point(shape = 21, colour = 'black', fill = 'red',size = 3)+
labs(title = "Lake Trout Length", 
     subtitle = "",
     x = "Lake Trout Sex", 
     y = "Length (mm)", 
     caption = glue("Boxplot statistics from years {start_yr} - {reportYear - 1}\nHollow points are outliers from years {start_yr} - {reportYear - 1}\n{reportYear} data shown by red points\n"))+
    theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 12 ),
        axis.text.x=element_text(color='black', size=12 ),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")

ltlength_box_sep22

ltweight_box_sep22 <-  
  ggplot(data=dflt22, mapping = aes(x = sex, y = weight))+
  geom_boxplot(data = dflt_exclude, mapping = aes(x = sex, y = weight), fill = 'lightgray', outlier.shape = 1,   outlier.size = 3)+
  geom_point(shape = 21, colour = 'black', fill = 'red',size = 3)+
  labs(title = "Lake Trout Weight", 
       subtitle = "",
       x = "Lake Trout Sex", 
       y = "Weight (g)", 
       caption = glue("Boxplot statistics from years {start_yr} - {reportYear - 1}\nHollow points are outliers from years {start_yr} - {reportYear - 1}\n{reportYear} data shown by red points\n"))+
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 12 ),
        axis.text.x=element_text(color='black', size=12),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        plot.title = element_text(face = 'bold', size = 12))

ltweight_box_sep22

ggsave("LTlength_boxplot_sep22.jpg", plot = ltlength_box_sep22, path = paste0(getwd(), "/", "plots"), width = 5.5, height = 5.7, units = "in", dpi = "print")
ggsave("LTweight_boxplot_sep22.jpg", plot = ltweight_box_sep22, path = paste0(getwd(), "/", "plots"), width = 5.5, height = 5.7, units = "in", dpi = "print")


ltlength_box <- ggplot(data=dflt, mapping = aes(x = gender, y = length))+
  geom_boxplot(aes(fill = 'lightgray', alpha = 0.5))+
labs(title = "Lake Trout Length", 
     subtitle = "",
     x = "Lake Trout Sex", 
     y = "Length (mm)")+
  facet_wrap( ~year)+
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),

        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()
ggsave("LTlength_boxplot.jpg", plot = ltlength_box, path = paste0(getwd(), "/", "plots"), width = 11.5, height = 5.7, units = "in", dpi = "print")

ltlength_box22 <- ggplot(data=dflt22, mapping = aes(x = gender, y = length))+
  geom_boxplot(aes(fill = 'lightgray', alpha = 0.5))+
  labs(title = "Lake Trout Length", 
       subtitle = "",
       x = "Lake Trout Sex", 
       y = "Length (mm)")+
   theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()
ltweight_box22 <- ggplot(data=dflt22, mapping = aes(x = gender, y = weight))+
  geom_boxplot(aes(fill = 'lightgray', alpha = 0.5))+
  labs(title = "Lake Trout Weight", 
       subtitle = "",
       x = "Lake Trout Sex", 
       y = "Weight (g)")+
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()



### histogram 
ggplot(data = dflt)+
  geom_histogram(mapping = aes(x = weight), binwidth = 100)+
labs(title = "Lake Trout Weight", 
     subtitle = "",
     x = "Lake Trout Sex", 
     y = "Length (mm)")+
  facet_wrap( ~year)+
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()




ggplot(data = dflt, mapping = aes(x = weight, colour = as.factor(year)))+
  geom_freqpoly(binwidth = 100)


### covariation between continuous variable (weight) and categorical variable (gender) 
ggplot(data = dflt, mapping = aes(x=weight))+
  geom_freqpoly(mapping = aes(colour = gender), binwidth= 50)

ggplot(data=dflt, mapping = aes(x = weight, y = ..density..))+
  geom_freqpoly(mapping = aes(colour = gender), binwidth =100)

ltweight_box <- ggplot(data=dflt, mapping = aes(x = gender, y = weight))+
  geom_boxplot(aes(fill = 'lightgray', alpha = 0.5))+
  labs(title = "Lake Trout Weight", 
       subtitle = "",
       x = "Lake Trout Sex", 
       y = "Weight (g)")+
  facet_wrap( ~year)+
  theme(axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_line(color = 'lightgray'),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.minor.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(color = 'black', size = 12, face = 'bold'),
        axis.text.y=element_text(color = 'black', size = 10),
        axis.text.x=element_text(color='black', size=10, angle=90),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.spacing  = unit(1, "lines"),
        strip.text = element_text(size = 12, face = 'bold', colour = 'white'),
        strip.background = element_rect(fill = "#336600", colour = 'black'),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        
        plot.title = element_text(face = 'bold', size = 12), 
        legend.position = "none")+
  scale_fill_grey()

ggsave("LTweight_boxplot.jpg", plot = ltweight_box, path = paste0(getwd(), "/", "plots"), width = 11.5, height = 5.7, units = "in", dpi = "print")




### covariation between continuous variables  
ggplot(data = dflt)+
  geom_point(mapping = aes(x = length, y = weight), alpha = 1/10)

ggplot(data = dflt)+
  geom_bin2d(mapping = aes(x = length, y = weight))

ggplot(data = dflt, mapping = aes(x = length, y = weight))+
 geom_boxplot(mapping = aes(group = cut_width(length, 100)))

ggplot(data = dflt, mapping = aes(x = length, y = weight))+
  geom_boxplot(mapping = aes(group = cut_number(length, 10)))



### does not really make sense to remove outliers as these fish are real and not data entry errors. Just rare fish. 

mod <- lm(logw~logl, data = dflt)
### Not fully understanding use of residuals
### eliminating relationship between continuous variables?  
dflt2 <- dflt %>% 
  add_residuals(mod) %>% 
  mutate(resid=exp(resid))


mod2 <- lm(log(weight)~log(length), data = dflt)

dflt2 <- dflt %>% 
  add_residuals(mod2) %>% 
  mutate(resid=exp(resid))

ggplot(data = dflt2)+
  geom_point(mapping = aes(x = length, y = resid))

ggplot(dflt2, aes(logl, resid))+
  geom_point()

ggplot(data = dflt2)+
  geom_boxplot(mapping = aes(x = gender, y = resid))

ggplot(dflt2, aes(resid))+
  geom_freqpoly(binwidth = 0.5)

ggplot(data = dflt2)+
  geom_point(mapping = aes(x = logl, y = logw))


### create linear model using log transformed weight and length 
model <- stats::lm(logw~logl, data = dflt)
#model2 <- stats::lm(logw~logl, data = dflt21)
#model <- stats::lm(logw~logl, data = dflt20)
### if log columns not created, use this code 
#model1 <- lm(log(dflt$weight)~log(dflt$length))
### display coefficients of model
coef(model)
coef(mod)
coef(mod2)
### summary of model 
summary(model)
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(dflt2, aes(logl, logw)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 


### Proofing the results of lm tutorial  
model1 <- function(a, data){
  a[1]+data$logl*a[2]
}
model1(c(6.25, 7), dflt2)


measure_distance <- function(mod, data){
  diff <- data$logw - model1(mod,data)
  sqrt(mean(diff^2))
}
measure_distance(c(6.25, 5), dflt2)

dflt2_dist <-function(a1, a2) {
  measure_distance(c(a1, a2), dflt2)
  
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, dflt2_dist))
models

ggplot(dflt2, aes(logl, logw)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

best<-optim(c(0,0), measure_distance, data = dflt2)
best$par

### coefficients as numeric values  
a <- (round(as.numeric(exp(model$coefficients[1])), 7))
b <- (round(as.numeric(model$coefficients[2]),5))




year_colors <- c("2022" = "red")

myformula <- logw~logl
plot_model <- ggplot(data = dflt, mapping = aes(logl, logw))+
  geom_point(aes(color = factor(year)), alpha = 0.5, size=2)+
  geom_smooth(method = "lm", formula = "y~x", se = F, col = "#0072B2")+
  labs(title = "Wachusett Lake Trout log-transformed Length-Weight",
  x = "log[Length (mm)]", 
  y = "log[Weight (g)]")+
  scale_color_manual(values = year_colors, aesthetics = c("color", "fill"), breaks = "2022", limits = "2022") 

plot_model

# scale_color_manual(values = c('#999999', '#999999','#999999','#999999','#999999','#999999','#999999', 'red'))
  
### testing with two datasets
plot_model <- ggplot(data = dflt, mapping = aes(logl, logw))+
  geom_point(data = dflt, alpha = 0.5, shape = 1, size=3)+
  geom_point(data = dflt22, shape = 21, colour = "black", fill = 'red', size=3)+ 
  geom_smooth(method = "lm", formula = "y~x", se = T, col = "black")+
  labs(title = "Wachusett Lake Trout log-transformed Length-Weight",
       x = "log[Length (mm)]", 
       y = "log[Weight (g)]")
  

  #scale_color_manual(values = year_colors, aesthetics = c("color", "fill"), breaks = "2022", limits = "2022")

plot_model

### Grid tutorial 
grid <- dflt2 %>% 
  data_grid(logl) 
grid
grid <- grid %>% 
  add_predictions(model)







ggplot(dflt2, aes(logl))+
  geom_point(aes(y=logw))+
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)





### print the equation 
lm_eqn <- function(dflt){
  m <- lm(logw~logl, dflt);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
      list(a = format(unname(coef(m)[1]), digits = 2),
           b = format(unname(coef(m)[2]), digits = 2),
           r2 = format(summary(m)$r.squared, digits = 3)))
  
}
dftext <- data.frame(eq=as.character(as.expression(eq)))
dftext


# p <- ggplot(data = dflt, aes(x= logl, y= logw))+
#   geom_smooth(method = "lm", formula = myformula, se = F, col = "#999999")+
#   stat_poly_eq(formula=myformula,
#                aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
#                parse=TRUE)+
#   geom_point()
# p
# 

### formatting and pasting the equation to the output 
p1 <- plot_model+ 
  geom_text(x = 6, y = 9, size = 5, aes(label =eq),data = dftext, parse = TRUE)+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size=1), 
        axis.title = element_text(color = 'black', size = 12, face = 'bold'), 
        axis.text = element_text(color = 'black', size = 12, face = 'bold'), 
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        legend.position = "none",
        axis.ticks = element_line(color = "black", size = 1))
 
p1


ggsave("laketroutlinearregression_2022red.jpg", plot = p1, path = paste0(getwd(), "/", "plots"), width = 11.5, height = 5.7, units = "in", dpi = "print")



### Create and then view color palette 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
show_col(cbPalette)



plot_model
p1
    
ggplotRegression <- function(fit){
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point(color = "gray") +
    geom_abline(xintercept = )+
    labs(title = "Wachusett Lake Trout log-transformed Length-Weight",
        x = "log[Length (mm)]", 
        y = "log[Weight (g)]")  
 
  
  
  
  
}
 geom_smooth(method = "lm", col = "black", se=F) +
 subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]],5), 
                       " P =",signif(summary(fit)$coef[2,4],5))


log_plot <- ggplotRegression(model)


### Length-Weight with allometric growth Curve ####


my.label <- bquote(Weight==~.(format(a,digits = 8))~Length^.(format(b, digits = 5)))  

plot <- ggplot(data= dflt, aes(x = length, y= weight)) +
  geom_point()+
  geom_smooth(data = dflt, aes(x = length, y = weight), se=T, method = "loess", color = "black", formula = y ~x, level=0.95)+
  annotate('text', x = 600, y = 10000, label = my.label)+
  xlab("Total Length (mm)")+
  ylab("Total Weight (g)")+
  ggtitle("Wachusett Lake Trout Length-Weight Relationship")#+
  #theme_bw()

plot
### if you want to display data by year, use this chunk of code below in geom_point()
#aes(colour = factor(year(sample_date)))



####Because logl (b) not equal to 3, considered allometric growth according to FishR vignette above 
####extract coefficients and log transform them back for use in allometric growth equation 
####Researcher interested in predicting mean weight on original scale. back-transforming mean underestimates mean value on original scale. 
###predicted values on log scale can be back-tranformed to original scale with exp()

###derive correction factor for back-transformation from Ogle 2013
# syx <- summary(model)$sigma
# ( cf <- exp((syx^2)/2) )
# ( pred.log <- predict(model, data.frame(logl=log(500)), interval = "c") )
# ( bias.pred.orig <- exp(pred.log) )
# ( pred.orig <- cf*bias.pred.orig )

#coefflog1 <- enframe(coef(model))
#predicted weight in grams with back-transformation 
#model[[1]]
#model$coefficients[2]

a <- exp(model$coefficients[1])
b <- model$coefficients[2]
lm_eqn <- function(dflt){
  model <- lm(weight~length, dflt);
  eq <- expression(paste(italic(y),"=", a, italic(x)^b),
                 list(a = a,
                      b = b))
  as.character(as.expression(eq));


}
lm_eqn(dflt)


dflty <- dflt %>% 
  group_by(Fish_ID,length) %>% 
  mutate(model_weight=a*((length)^b))



# mod <- lm(weight.x~length, data = df)
# my.formula <- df$weight.x ~ df$length
####Attempt to paste the formula together 
# eq = function(x){x*x}
# ggplot(data.frame(x=c(1, 50)), aes(x=x)) + stat_function(fun=eq, geom="line") + xlab("x") + ylab("y")
# eq = function(x){x*x}
# ggplot(data.frame(x=c(1, 50)), aes(x=x)) + stat_function(fun=eq, geom="line") + xlab("x") + ylab("y")
# 
# eq <- paste0(round(a, 7), "x","^(", round(b,5), ")", sep="")
# 
# eq <- paste(a, "x^(", b, ")", sep = "")      
# 
# eq <- substitute(grDevices::italic(y)==a*grDevices::italic(x)^b,
#                  list( a = format(coeffa, digits = 7),
#                        b = format(coeffb, digits = 7)))
# dftext <- data.frame(x=500, y=9000, eq=as.character(as.expression(eq)))


# #####Create length-weight ratio scatter plot with allometric growth curve equation plotted 


my.label <- bquote(Weight==~.(format(a,digits = 8))~Length^.(format(b, digits = 5)))  

plot <- ggplot(data= dflt, aes(x = length, y= weight)) +
  geom_point()+
  geom_smooth(data = dflt, aes(x = length, y = weight), se=T, method = "loess", color = "red", formula = y ~x, level=0.95)+
  annotate('text', x = 500, y = 9000, label = my.label)+
  xlab("Total Length (mm)")+
  ylab("Total Weight (g)")+
  ggtitle("Wachusett Lake Trout Length-Weight Relationship")

plot


