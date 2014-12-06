#

#library(gdata)
library(dplyr) # manipulate dataframes
#library(plyr) # for manipulating data
library(lubridate) # calculate dates
library(stringr) # manipulate strings
library(eeptools) # calculate ages


##read deduped data

#sa_dat1 <- read.csv("sa_dat1_new.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",", quote = "")
#sa_dat2 <- read.csv("sa_dat2_new.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",", quote = "")
#TCNTEL <- read.csv("all-groups-without 51--Remove-Duplicate-Lines Final.txt", sep = ",", quote = "")

sa_dat1d_a <- read.csv("Sa_dat1_1M_dedup.csv", header = TRUE, stringsAsFactors = FALSE, 
                       sep = ",", quote = "\"'")

sa_dat1d_b <- read.csv("Sa_dat1M1_2M_dedup.csv", header = TRUE, stringsAsFactors = FALSE, 
                       sep = ",")

sa_dat1d_c <- read.csv("Sa_dat2M1_3M_dedup.csv", header = TRUE, stringsAsFactors = FALSE, 
                       sep = ",")

sa_dat1d_d <- read.csv("Sa_dat3M1_end_dedup.csv", header = TRUE, stringsAsFactors = FALSE, 
                       sep = ",")


headers <- names(sa_dat1d_b) # change to ensure all of the columns are named the same
names(sa_dat1d_a) <- headers  


## combine to create database1
sa_dat1_dedup <- rbind(sa_dat1d_a, sa_dat1d_b, sa_dat1d_c, sa_dat1d_d)

sa_dat1_dedup$db <- "sa_dat1"
sa_dat1_dedup$AGE <- 1 # placeholder

sa_dat1_dedup_clean <- sa_dat1_dedup
sa_dat1.tbl <- tbl_df(sa_dat1_dedup_clean)

##########################
## Format dates

sa_dat1_dedup$NACIMIENTO <- parse_date_time(sa_dat1_dedup$NACIMIENTO, "mdy")
sa_dat1.tbl$NACIMIENTO <- as.Date(sa_dat1.tbl$NACIMIENTO, "%m-%d-%y") # warning ok




################################
## factorize variables for classification
factors_unique <- unique(sa_dat1_dedup$SEXO)
factors_unique <- factors_unique[4:length(factors_unique)]

factor_names <- c("Femenino", "Masculino", "No response", factors_unique)

sa_dat1.tbl$SEXO <- factor(sa_dat1.tbl$SEXO, levels = factor_names, 
                           labels = factor_names) # factorize gender



###############################
## create new dataframe without NA for calculations
sa_dat1.tbl_na <- na.omit(sa_dat1.tbl) # omits rows with NA

## calculate age
#sa_dat1_dedup$AGE <- parse_date_time(today(), "ymd") - sa_dat1_dedup$NACIMIENTO
age <- age_calc(sa_dat_na$NACIMIENTO, units = "years") 
sa_dat1.tbl_na$AGE <- floor(age) # whole years using eeptools

## create age group
age_group <- c("18-20", "21-25", "26-29", "30-36", "37-45", "46-55", "55+")
sa_dat1.tbl_na$AGE_GROUP <- cut(sa_dat1.tbl_n$AGE, breaks = c(20, 25, 29, 36, 45, 55, Inf), 
                               labels = age_group)

################################
## find unique districts
districts <- unique(sa_dat1.tbl$DISTRITOO)

sa_dat1.tbl$DISTRITOO <- factor(sa_dat1.tbl$DISTRITOO, labels = districts)

sa_dat1.tbl_na$DISTRITOO <- factor(sa_dat1.tbl_na$DISTRITOO, labels = districts)





summary_data <- summary(sa_dat1.tbl_na)

####################################################

## entire 2nd file
#sa_dat2_a <- read.csv("Sa_dat2_1-1M.csv", header = TRUE, stringsAsFactors = FALSE,
#                      sep = ",") #entire file

#sa_dat2_b <- read.csv("Sa_dat2_1M1_2M.csv", header = TRUE, stringsAsFactors = FALSE,
#                      sep = ",") #entire file

#sa_dat2_c <- read.csv("Sa_dat2_2M-3M.csv", header = TRUE, stringsAsFactors = FALSE,
#                      sep = ",") #entire file

#sa_dat2_d <- read.csv("Sa_dat2_3M-4M.csv", header = TRUE, stringsAsFactors = FALSE,
#                      sep = ",") #entire file

## 2nd deduped file

sa_dat2d_a <- read.csv("Sa_dat2_1-1M_dedup.csv", header = TRUE, stringsAsFactors = FALSE,
                       sep = ",") #deduped
sa_dat2d_a <- na.omit(sa_dat2d_a) # remove extra rows filled with NA

sa_dat2d_b <- read.csv("Sa_dat2_1M1_2M_dedup.csv", header = TRUE, stringsAsFactors = FALSE,
                       sep = ",") #deduped

sa_dat2d_c <- read.csv("Sa_dat2_2M-3M_dedup.csv", header = TRUE, stringsAsFactors = FALSE,
                       sep = ",") #deduped

sa_dat2d_d <- read.csv("Sa_dat2_3M-4M_dedup.csv", header = TRUE, stringsAsFactors = FALSE,
                       sep = ",") # deduped

sa_dat2d_e <- read.csv("Sa_dat2_4M-end_dedup.csv", header = TRUE, stringsAsFactors = FALSE,
                       sep = ",") #deduped

sa_dat2_dedup <- rbind(sa_dat2d_a, sa_dat2d_b, sa_dat2d_c, sa_dat2d_d, sa_dat2d_e)

sa_dat2_dedup$db <- "sa_dat2"

sa_dat2_dedup_clean <- sa_dat2_dedup 

sa_dat2_dedup_clean$NACIMIENTO <- parse_date_time(sa_dat2_dedup$NACIMIENTO, "mdy") # convert to parse
sa_dat2_dedup_clean$NACIMIENTO <- as.Date(sa_dat2_dedup_clean$NACIMIENTO, "%m-%d-%y") # convert to date format

sa_dat2.tbl <- tbl_df(sa_dat2_dedup_clean)
sa_dat2.tbl$AGE <- 1


## Factorize gender


sa_dat2.tbl$SEXO <- factor(sa_dat2.tbl$SEXO, c("Femenino", "Masculino", "No response")) 

sa_dat2.tbl_na <- na.omit(sa_dat2.tbl) # removes rows with NA


## calculate age

sa_dat2.tbl_na$AGE <- 1 # placeholder
  
age <- age_calc(sa_dat2.tbl_na$NACIMIENTO, units = "years") # using eeptools 

sa_dat2.tbl_na$AGE <- floor(age) # whole years


###################################
## Factorize Districts


## find unique districts

districts2 <- unique(sa_dat2.tbl$DISTRITOO)

sa_dat2.tbl$DISTRITOO <- factor(sa_dat2.tbl$DISTRITOO, labels = districts2)


## combine deduped files

sa_dat <- rbind(sa_dat1.tbl, sa_dat2.tbl)

Factor_SEXO <- unique(sa_dat$SEXO)
Factor_SEXO <- c(Factor_SEXO[1], Factor_SEXO[3], "No response", Factor_SEXO[4:length(Factor_SEXO)])
sa_dat$SEXO <- factor(sa_dat$SEXO)

Factor_DISTRITOO <- unique(sa_dat$DISTRITOO)
sa_dat$DISTRITOO <- factor(sa_dat$DISTRITOO, levels = Factor_DISTRITOO, labels = Factor_DISTRITOO)

summary_all <- summary(sa_dat)
overall_summary <- summary(sa_dat$SEXO) # summary by gender

## combine NA files

sa_dat_na <- rbind(sa_dat1.tbl_na, sa_dat2.tbl_na)

summary_without_na <- summary(sa_dat_na)

###################################
# summarize by district

group_by_district <- group_by(Provinciao, DISTRITOO)

summary_by_district <- summarise(group_by_district, count = n())

districts_by_gender <- group_by(Provinciao, DISTRITOO, SEXO)

summary_counts_gender_district <- summarise(districts_by_gender, count = n())

#summary_counts_gender_female <- length(which(sa_dat1_dedup$SEXO == "Femenino"))


###########################################
# Subset for states of Lima or Callao or blank
#

Province <- c("LIMA", "CALLAO", "")


Provinciao <- filter(sa_dat_clean1.tbl, PROVINCIAO %in% Province)

Provinciao_na <- filter(sa_dat_clean_na.tbl, PROVINCIAO %in% Province)


############################################
# By Gender by Age Group
#

group_by_age <- group_by(Provinciao_na.tbl, SEXO, AGE_GROUP)

by_age_group_female <- filter(group_by_age, SEXO == "Femenino")
by_age_group_male <- filter(group_by_age, SEXO == "Masculino")

summary_by_age_female <- summarize(by_age_group_female, count = n())
summary_by_age_male <- summarize(by_age_group_male, count = n())
