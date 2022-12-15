##############################################################################################################
## HMS 520                                                                                                  ##
## Students: Case Keltner & Huong Chu                                                                       ##
## Final Project                                                                                            ##
##############################################################################################################

##############################################################################################################
## Notes: In this code file, the original dataset called "LLCP2021.XPT" is big, ~1GB, so our team needed    ##
## to process the dataset outside of this project folder. We saved a subset dataset in this folder in case  ##
## you would like to test the codes out. Please ignore code lines from 21 to 41                             ##
##############################################################################################################

## EMPTY THE ENVIRONMENT
rm(list = ls())

# LOAD PACKAGES
library(data.table)
library(haven)
library(expss)

##############################################################################################################
##                          SAVE A SUBSET FILE FOR DATA PROCESSING AND ANALYSIS                             ##
##############################################################################################################

# SET WORKING DIRECTORY
setwd("C:/1.First year PhD/1. Fall quarter/HMS 520/BRFSS")

# IMPORT DATA
dt <- read_xpt("LLCP2021.XPT")
dt <- data.table(dt)

# PREPARE DATASET
setnames(dt, old=names(dt), new=tolower(gsub("X.", "", names(dt), fixed = TRUE)))

final <- dt[, c("_state", "_age_g", "_racegr3", "_educag", "_incomg1", "_sex", 
                "marital", "flushot7", "genhlth", "_hlthpln", "persdoc3", 
                "medcost1", "checkup1", "_rfsmok3", "_rfbing5", "_urbstat",
                "_asthms1")]

# SAVE A SUBSET DATA FILE
write.csv(final, "C:/1.First year PhD/1. Fall quarter/HMS 520/HMS-520-Final-Project/brfss2021.csv", row.names = TRUE)


##############################################################################################################
##                                                START FROM HERE                                           ##
##############################################################################################################


# SET WORKING DIRECTORY
setwd("C:/1.First year PhD/1. Fall quarter/HMS 520/HMS-520-Final-Project")

# READ IN THE SUBSET DATAFILE
final <- fread("brfss2021.csv")
final[, V1 := NULL]

##############################################################################################################
##                                            CLEANING DATA FILE                                            ##
##############################################################################################################

# RENAME VARIABLES
old_names <- c("_state", "_age_g", "_racegr3", "_educag", "_incomg1", "_sex", 
               "marital", "flushot7", "genhlth", "_hlthpln", "persdoc3", 
               "medcost1", "checkup1", "_rfsmok3", "_rfbing5", "_urbstat",
               "_asthms1")

new_names <- c("state", "age", "race", "education", "income", "sex",'marital_status',
               "flushot","health_status", "insurance", "mul_doc", "not_see_doc",
               "checkup", "smoke", "alcohol", "urban_rural", "asthma_status")

setnames(final, old=old_names, new=new_names)

# RECODE MISSING VALUES

code_9 <- c("race", "income", "education", "marital_status", "flushot", "health_status",
            "insurance", "mul_doc", "not_see_doc", "checkup", "smoke", "alcohol",
            "asthma_status")

code_7 <- c( "flushot", "health_status", "mul_doc","not_see_doc", "checkup")

col_names <- names(final)

for (x in col_names) {
  if (x %in% code_9) {
    final[get(x) == 9, (x) := NA]
  }
}

for (x in col_names) {
  if (x %in% code_7) {
    final[get(x) == 7, (x) := NA]
  }
}

# CREATE A NEW VARIABLE: REGION

northeast = c(9, 23, 25, 33, 34, 36, 42, 44, 50)
midwest = c(18, 17, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
south = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
west = c(4, 8, 16, 35, 30, 49, 32, 56, 2, 6, 15, 41, 53)

states <- unique(final$state)
final$region <- "a"


for (i in states){
  if (i %in% northeast){
    final[state == i, region := "Northeast"]
  } else if (i %in% midwest){
    final[state == i, region :=  "Midwest"]
  } else if (i %in% south){
    final[state == i, region :=  "South"]
  } else if (i %in% west){
    final[state == i, region :=  "West"]
  }
}

final[region =="a", region := NA]
final<- final[complete.cases(final)]

# LABEL VARIABLES
clean_dt = apply_labels(final,
                     age = c("18-24" = 1,
                             "25-34" = 2,
                             "35-44" = 3,
                             "45-54" = 4,
                             "55-64" = 5,
                             "65+"   = 6),
                     race = c("White American" = 1,
                              "Black American" = 2,
                              "Other races"    = 3,
                              "Multiracial"    = 4,
                              "Hispanic"       = 5),
                     education = c("Less than high school" = 1,
                                   "High school"           = 2,
                                   "Some college"          = 3,
                                   "College graduate"      = 4),
                     income = c("<$15,000"          = 1,
                                "$15,000-$24,999"   = 2,
                                "$25,000-$34,999"   = 3,
                                "$35,000-$49,999"   = 4,
                                "$50,000-$99,999"   = 5,
                                "$100,000-$199,999" = 6,
                                "$200,000+"         = 7),
                     sex = c("Male"    = 1,
                             "Female"  = 2),
                     flushot = c("Yes" = 1,
                                 "No"  = 2),
                     health_status = c("Excellent" = 1,
                                       "Very good" = 2,
                                       "Good"      = 3,
                                       "Fair"      = 4,
                                       "Poor"      = 5),
                     insurance = c("Yes" = 1,
                                   "No"  = 2),
                     mul_doc = c("Only one"      = 1,
                                 "More than one" = 2,
                                 "No"            = 3),
                     checkup = c("Within past year"    = 1,
                                 "Within past 2 years" = 2,
                                 "Within past 5 years" = 3,
                                 "5 or more years ago" = 4,
                                 "Never"               = 8),
                     marital_status = c("Married"          = 1,
                                        "Divorced"         = 2,
                                        "Widowed"          = 3,
                                        "Separated"        = 4,
                                        "Never married"    = 5,
                                        "Unmarried couple" = 6),
                     smoke = c("No"    = 1,
                               "Yes"   = 2),
                     alcohol = c("No"  = 1,
                                 "Yes" = 2),
                     not_see_doc = c("Yes" = 1,
                                     "No"  = 2),
                     urban_rural = c("Urban counties" = 1,
                                     "Rural counties" = 2),
                     asthma_status = c("Current" = 1,
                                       "Former"  = 2,
                                       "Never"   = 3))



# SAVE CLEAN DATA FOR THE NEXT ANALYSIS
save(clean_dt, file = "C:/1.First year PhD/1. Fall quarter/HMS 520/HMS-520-Final-Project/clean_data.RData")

