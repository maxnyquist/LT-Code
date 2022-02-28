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
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "grDevices", "janitor", "FSA", "Hmisc")
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
 #con <- dbConnect(odbc::odbc(),
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



#dflt <- readRDS("ltdata_2020.rds")

### SCRIPT BEGIN ####
########################################################################.
###                      Lake Trout Mark & Recapture Recap Analysis ####
########################################################################.
dflt <- ltdata_2020
dflt <- ltdata
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

### df and dflt created in LT_import.
### use .rds
#dflt <- df
### select only LT from Wachusett Reservoir 
dflt <- dflt %>% 
  filter(waterbody == "Wachusett Reservoir" & fish_code == "LT")


test <- dflt
dflt$gender[dflt$gender != "F" & dflt$gender != "M"]  <- "U/I"
dflt$gender[is.na(dflt$gender)] <- "U/I"


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
  group_by(year(sample_date),gender) %>% 
  summarise(caught = n(), mean_weight = mean(weight, na.rm=T), mean_length = mean(length, na.rm = T),   
            cil = list(mean_cl_normal(length) %>% 
            rename(meanl=y, lwrl=ymin, uprl=ymax)), 
            ciw = list(mean_cl_normal(weight) %>% 
            rename(meanw=y, lwrw=ymin, uprw=ymax))) %>%  
  unnest(cols = c(cil, ciw)) %>% 
  mutate(freq = (caught/sum(caught)*100)) %>% 
  rename(year = 'year(sample_date)') %>% 
  adorn_totals("row")
test <- dflt %>% 
  group_by(sample_date)%>% 
  summarise(sample_date_length = length(sample_date), caught = n())
view(test)  
test <- d  
test$gender[test$gender != "F" | test$gender != "M"]  <- "U/I"

ortnotes$morts[mortnotes$morts == ""] <- 1 
  
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
  
tag_prop <- ggplot(data = d, aes(x=factor(year), y=freq, fill=gender))+
  geom_bar(stat="identity", aes(col=gender), position = "dodge")+
  expand_limits(y=0)+  
  scale_y_continuous(expand = c(0,0), limits = c(0, 100))+
  labs(x="Year", y="Percentage of Total Lake Trout Caught", legend = "Sex")+
  scale_fill_manual("Sex", values = c("F"= "#E69F00", "M"= "#56B4E9", "U/I" = "#009E73" ))+
  scale_color_manual("Sex", values = c("F"= "#E69F00", "M"= "#56B4E9", "U/I" = "#009E73" ))

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
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12)) 

tag_prop

ggsave(paste0(ltdata_dir,"/laketroutproportion.png"), width = 10, height = 6, dpi = 600)


test <- dflt %>% 
  dplyr::filter(year(sample_date)!=2020) %>% 
  group_by() %>% 
  summarise(mean_weight = mean(weight, na.rm=T), mean_ength = mean(length, na.rm = T)) 
  
test <- dflt %>% 
  group_by() %>% 
  summarise(mean_weight = mean(weight, na.rm=T), mean_ength = mean(length, na.rm = T)) 



panel.grid.major = element_blank(),



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



### log transform weight and length 
### put them into new columns  
dflt$logw <- log(dflt$weight)
dflt$logl <- log(dflt$length)
dflt <- dflt %>% 
  filter(!logw=="-Inf")
dflt20 <- dflt %>% 
  filter(year(sample_date) == 2020)
### create linear model using log transformed weight and length 
model <- stats::lm(logw~logl, data = dflt)
#model <- stats::lm(logw~logl, data = dflt20)
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
myformula <- logw~logl
plot_model <- ggplot(data = dflt, mapping = aes(logl, logw))+
  geom_point(shape=1, size=2)+
  geom_smooth(method = "lm", formula = "y~x", se = F, col = "#0072B2")+
  labs(title = "Wachusett Lake Trout log-transformed Length-Weight",
  x = "log[Length (mm)]", 
  y = "log[Weight (g)]")
  #annotate("text", )

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


p1 <- plot_model+ 
  geom_text(x = 6, y = 8, aes(label =eq),data = dftext, parse = TRUE)+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "#999999"))
 


p1


ggsave("laketroutlinearregression.png", width = 10, height = 6, dpi = 300)



### Create and then view color palette 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
show_col(cbPalette)



plot_model

    
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

# 
# log_plot+
#   theme_light() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = NA),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(color = "grey"))
# 

 # ggtitle("Wachusett Lake Trout log-transformed Length-Weight") +
 # xlab("Month") +
 # ylab("Year")


# 
# geom_linerangeh(aes(y = as.factor(Year),
#                       xmin = lo,
#                       xmax = hi,
#                       group = as.factor(Year))) +
# scale_x_date(date_labels = "%b") +
#   scale_y_discrete(limits = rev(unique(sort(as.factor(tbl_ice$Year))))) +
#   
# # 
# ggplot() +
#   theme_light() +
#   geom_linerangeh(aes(y = as.factor(Year),
#                       xmin = lo,
#                       xmax = hi,
#                       group = as.factor(Year))) +
#   theme(plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = NA),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(color = "grey")) +
#   scale_x_date(date_labels = "%b") +
#   scale_y_discrete(limits = rev(unique(sort(as.factor(tbl_ice$Year))))) +
#   ggtitle("Wachusett Ice Cover Duration") +
#   xlab("Month") +
#   ylab("Year")
# 


#fitPlot(model, xlab="log Total Length (mm)", ylab="log Weight (g)")

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

# ggplot() +
#   theme_light() +
#   geom_linerangeh(aes(y = as.factor(Year),
#                       xmin = lo,
#                       xmax = hi,
#                       group = as.factor(Year))) +
#   theme(plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = NA),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(color = "grey")) +
#   scale_x_date(date_labels = "%b") +
#   scale_y_discrete(limits = rev(unique(sort(as.factor(tbl_ice$Year))))) +
#   ggtitle("Wachusett Ice Cover Duration") +
#   xlab("Month") +
#   ylab("Year")
# 

