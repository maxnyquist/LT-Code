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
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "grDevices", "janitor", "FSA")
sapply(pkg, library, character.only = TRUE)


# ### SET OPTIONS/ENV VARIABLES ####
 
# ### Eliminate Scientific Notation
# options(scipen = 999)
# 
# ### SOURCE DATA/FUNCTIONS/FILES ####
# 
# R_Config <- read.csv("//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WQDatabase/R-Shared/Code/R_Config.csv", header = TRUE)
# config <- as.character(R_Config$CONFIG_VALUE)
# 
# # ### FETCH CACHED DATA FROM WAVE RDS FILES ####
#  datadir <- config[18]
# ### Make a list of all the .rds files using full path
# rds_files <- list.files(datadir,full.names = TRUE ,pattern = "\\.rds$")
# rds_files # Take a look at list of files
# ### Select which rds files needed for this script
# rds_in <- c(3,4,7:9)
# ### subset rds files (if you want all of them then skip rds_in and the following line)
# rds_files <- rds_files[rds_in]
# ### create an object that contains all of the rds files
# data <- lapply(rds_files, readRDS)
# ### Make a list of the df names by eliminating extension from files
# df_names <- gsub(".rds", "", list.files(datadir, pattern = "\\.rds$"))
# df_names <- df_names[rds_in]
# # name each df in the data object appropriately
# names(data) <- df_names
# ### Extract each element of the data object into the global environment
# list2env(data ,.GlobalEnv)
# ### Remove data
# rm(data)

### CONNECT TO A FRONT-END DATABASE ####
# 
# ### Set DB
# db <- config[18]
# ### Connect to Database 
# con <- dbConnect(odbc::odbc(),
#                  .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
#                                             paste0("DBQ=", db), "Uid=Admin;Pwd=;", sep = ";"),
#                  timezone = "America/New_York")
# ### See the tables 
# tables <- dbListTables(con)  
# ### Fetch an entire table (avoid using SQL queries - just pull entire tables and subset in R)
# tbl <- dbReadTable(con, "Q&W Table")
# ### Always disconnect and rm connection when done with db
# dbDisconnect(con)
# rm(con)
# 
# 
# ### TO REPORT IN THE ANNUAL ####
# Between 2014-2019
# Number captured? Number tagged and released? Number of fish recapped? Number released with no tags, harvested, collected for otoliths, morts? 
# % males? % females? % immature or unknown? 
# 2019 only, same questions above. 
# New questions 
# Recap growth
# Plot Lake Trout growth curve (reference other scripts)
# 
setwd()
dflt <- readRDS("ltdata.rds")

### SCRIPT BEGIN ####
########################################################################.
###                      Lake Trout Mark & Recapture Recap Analysis ####
########################################################################.
recaps <- dflt %>% 
  filter(waterbody == "Wachusett Reservoir" & fish_code == "LT") %>% 
  group_by(tag_num) %>% 
  filter(n()>1) %>% 
  summarise(recaps = n()) %>% 
  drop_na() %>% 
  filter(!tag_num == "NA") %>% 
  distinct() 
# recaps <- dflt %>% 
#   filter(waterbody == "Wachusett Reservoir" & fish_code == "LT") %>% 
#   group_by(tag_num) %>% 
#   summarise(recaps = n()) %>% 
#   drop_na() %>% 
#   filter(!tag_num == "NA") %>% 
#   distinct() 

sum(recaps$recaps)
count(recaps)

########################################################################.
###         Lake Trout Mark & Recapture Length/Weight Relationships ####
########################################################################.

### df and dflt created in LT_import.R 
dflt <- df
### select only LT from Wachusett Reservoir 
dflt <- dflt %>% 
  filter(waterbody == "Wachusett Reservoir" & fish_code == "LT")

### males only 
dfltm <- dflt %>% 
  filter(gender == "M") %>% 
  group_by(year(sample_date)) %>% 
  summarise(caught = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
  rename(year = "year(sample_date)")
plotmales <- ggplot(dfltm, aes(year, mean_length))+
  geom_point()

### remove NAs 
#dflt <- subset(dflt, !is.na(weight) & !is.na(length))
dfltag <- dflt %>% 
  drop_na(tag_num) %>% 
  filter(!tag_num=="NA") %>% 
  filter(!is.na(weight)&!is.na(length))
dfltmort <- dflt %>% 
  filter(is.na(tag_num)|tag_num=="NA") #%>% 
  filter(tag_num=="NA")

  

  summarise(ciw = list(mean_cl_normal(weight) %>% 
                        rename(mean=y, lwrw=ymin, uprw=ymax))) %>%   
  
filter(gender=="F"|gender=="M")  
  
d <- dflt %>% 
  group_by(year(sample_date), gender) %>% 
  summarise(caught = n(), mean_weight = mean(weight, na.rm=T), mean_length = mean(length, na.rm = T),   
            cil = list(mean_cl_normal(length) %>% 
            rename(mean=y, lwrl=ymin, uprl=ymax)), 
            ciw = list(mean_cl_normal(weight) %>% 
            rename(mean=y, lwrw=ymin, uprw=ymax))) %>%  
  unnest %>% 
  mutate(freq = (caught/sum(caught)*100)) %>% 
  rename(year = 'year(sample_date)') #%>% 
  adorn_totals("row")

f <- dfltag %>% 
    group_by(gender) %>% 
    summarise(tagged = n(), mean_weight = mean(weight), mean_length = mean(length)) %>% 
    mutate(freq = (caught/sum(caught)*100)) %>% 
    filter(gender=="F"|gender=="M")  
### plot tagged fish summary data by year ####
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
  
tag_prop <- ggplot(data = d, aes(year, freq, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")

tag_caught <- ggplot(data = d, aes(year, caught, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")


tag_prop
tag_weight
tag_length
tag_caught


### plot tagged fish summary data by gender ####
tag_weight <- ggplot(data = f, aes(gender,mean_weight, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")

tag_length <- ggplot(data = f, aes(gender,mean_length, fill=gender))+
  geom_bar(aes(col=gender), size=3)

tag_prop <- ggplot(data = f, aes(gender, freq, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")

tag_caught <- ggplot(data = f, aes(gender, caught, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")


tag_prop
tag_weight
tag_length
tag_caught



### log transform weight and length 
### put them into new columns  
dflt$logw <- log(dflt$weight)
dflt$logl <- log(dflt$length)
### create linear model using log transformed weight and length 
model <- lm(logw~logl, data = dflt)
### if log columns not created, use this code 
#model1 <- lm(log(dflt$weight)~log(dflt$length))
### display coefficients of model
coef(model)
### summary of model 
summary(model)
### coefficients as numeric values  
a <- (round(as.numeric(exp(model$coefficients[1])), 7))
b <- (round(as.numeric(model$coefficients[2]),5))

### Log-transformed linear regression  ####

# plot_model <- ggplot(dflt, aes(logl, logw))+
#   geom_point()+
#   geom_smooth(method = "lm", formula = "y~x", se = F, col = "red")
#   #annotate("text", )
# 
# plot_model
ggplotRegression <- function(fit){
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point(color = "gray") + 
    stat_smooth(method = "lm", col = "black", se=F) +
    labs(title = "Wachusett Lake Trout log-transformed Length-Weight",
        subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]],5), 
                       " P =",signif(summary(fit)$coef[2,4],5)), 
         xlab="log Total Length (mm)", 
         ylab="log Weight (g)")
 
  
  
  
  
}

log_plot <- ggplotRegression(model)


log_plot+theme_bw()


fitPlot(model, xlab="log Total Length (mm)", ylab="log Weight (g)")

### Length-Weight with allometric growth Curve ####


my.label <- bquote(Weight==~.(format(a,digits = 8))~Length^.(format(b, digits = 5)))  

plot <- ggplot(data= dflt, aes(x = length, y= weight)) +
  geom_point(color = "gray")+
  geom_smooth(data = dflt, aes(x = length, y = weight), se=T, method = "loess", color = "black", formula = y ~x, level=0.95)+
  annotate('text', x = 600, y = 10000, label = my.label)+
  xlab("Total Length (mm)")+
  ylab("Total Weight (g)")+
  ggtitle("Wachusett Lake Trout Length-Weight Relationship")+
  theme_bw()

plot





####Because logl (b) not equal to 3, considered allometric growth according to FishR vignette above 
####extract coefficients and log transform them back for use in allometric growth equation 
####Researcher interested in predicting mean weight on original scale. back-transforming mean underestimates mean value on original scale. 
###predicted values on log scale can be back-tranformed to original scale with exp()

###derive correction factor for back-transformation from Ogle 2013
syx <- summary(model)$sigma
( cf <- exp((syx^2)/2) )
( pred.log <- predict(model, data.frame(logl=log(500)), interval = "c") )
( bias.pred.orig <- exp(pred.log) )
( pred.orig <- cf*bias.pred.orig )

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


# p1 <- plot +geom_text( label = lm_eqn(dflt), parse = T)
# p1
# coefflog <- enframe(exp(coef(model)))
# coeff <- enframe(coef(model))
# coeffa <- as.numeric(exp(coefflog[1, 2]))
# coeffb <- as.numeric(coeff[2,2])

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




