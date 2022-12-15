##############################################################################################################
## HMS 520                                                                                                  ##
## Students: Case Keltner & Huong Chu                                                                       ##
## Final Project                                                                                            ##
##############################################################################################################

## EMPTY THE ENVIRONMENT
rm(list = ls())

# LOAD PACKAGES
library(data.table)
library(ggplot2)
library(haven)
library(expss)
library(glmnet)
library(boot)
library(pander)

# SET WORKING DIRECTORY
setwd("C:/1.First year PhD/1. Fall quarter/HMS 520/HMS-520-Final-Project")

# IMPORT DATA
load("clean_data.Rdata")

##############################################################################################################
#######                  DESCRIPTIVE ANALYSES - CHARACTERISTICS BY VACCINATION STATUS                  #######
##############################################################################################################

# Test for single table : Vaccination rate by region

cross_cases(clean_dt, region, flushot) # distribution
cross_rpct(clean_dt, region, flushot) # row percent

# Produce a series of tables by vaccination status

variables <- c("region", "age", "race", "education", "income", "sex",'marital_status',
               "flushot","health_status", "insurance", "mul_doc", "not_see_doc",
               "checkup", "smoke", "alcohol", "urban_rural", "asthma_status")
  
for (i in variables){
  print(paste0("Vacinnation rates by ", i, ":"))
  print(cross_cases(clean_dt, get(i), flushot))
  print(cross_rpct(clean_dt, get(i), flushot))
}


##############################################################################################################
#######                                                PLOTS                                           #######
##############################################################################################################

# Create plots by using loop

variables2 <- c("region", "age", "race", "education", "sex",'marital_status',
               "health_status", "insurance", "mul_doc", "not_see_doc",
               "checkup", "smoke", "alcohol", "urban_rural", "asthma_status")

title <- c("Region", "Age Group", "Race", "Highest Education Attained", "Sex","Marital Status",
           "Health Status", "Insurance Status", "Having a Healthcare Provider", "Inability to See a Physician in the Last Year Due to Cost",
           "Date of Last Routine Health Checkup", "Smoking Status", "Alcohol Consumption Status", "Urban vs. Rural Residential Status", "Asthma Status")

title_names <- as.list(title)
names(title_names) <- variables2

for (i in variables2){
  plots <- ggplot(clean_dt, aes(x= flushot,  group=get(i))) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(x = paste0(title_names[[i]]), y = "Percent", fill="Received Flu Shot") +
    ggtitle(paste0("Vaccination Coverage by ", title_names[[i]], " in 2021")) +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), # adjust the plot title to the center and adjust font
          axis.text.x=element_blank()) + # Remove X axis ticks
    facet_grid(~get(i)) +
    scale_y_continuous(labels = scales::percent)+
    scale_fill_discrete(labels=c('Yes', 'No')) # change legend values' names 
  
  print(plots)
  #ggsave(paste0("Plot for ", i,".png"), plot = plots)
  print(paste0("Done plotting variable: ", i))
}


# Plot for income (making it separately because the plot saved was not in the right size for the label of income)

plots <- ggplot(clean_dt, aes(x= flushot,  group=income)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x = paste0("Income"), y = "Percent", fill="Received Flu Shot") +
  ggtitle(paste0("Vaccination Coverage by Income in 2021")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), # adjust the plot title to the center and adjust font
        axis.text.x=element_blank()) + # Remove X axis ticks
  facet_grid(~income) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_discrete(labels=c('Yes', 'No')) # change legend values' names

#print(plots)
ggsave("Plot for income.png", plot = plots, width = 15, height = 10, units = "in")
print(paste0("Done plotting variable: income"))



##############################################################################################################
#######                                             MODELING                                           #######
##############################################################################################################

# 1. LOGISTIC REGRESSION MODEL - Which health characteristics are associated with getting a flu shot?

# Treating covariates as continuous variables
mod1 <- glm(data = clean_dt, flushot~.)
summary(mod1)
pander(mod1) # make a nice table for the coefficients
exp(coef(mod1))

# Treating covariates as categorical variables
mod2 <- glm(data = clean_dt, flushot~as.factor(region) + as.factor(age) + as.factor(race)+
             as.factor(education)+ as.factor(income) +as.factor(sex)+ as.factor(marital_status)+
             as.factor(health_status)+as.factor(insurance) + as.factor(mul_doc) +as.factor(not_see_doc)+
             as.factor(checkup) + as.factor(smoke) + as.factor(alcohol) + as.factor(urban_rural) + 
             as.factor(asthma_status))
summary(mod2)
pander(mod2) 
exp(coef(mod2))

# 2. LASSO REGRESSION MODEL - FIND THE BEST COEFFICIENTS FOR THE MODEL


x <- clean_dt[, c("region", "age", "race", "education", "income", "sex",'marital_status',
               "health_status", "insurance", "mul_doc", "not_see_doc",
               "checkup", "smoke", "alcohol", "urban_rural", "asthma_status")]

y <- clean_dt$flushot

x <- data.matrix(clean_dt[, c("region", "age", "race", "education", "income", "sex",'marital_status',
                           "health_status", "insurance", "mul_doc", "not_see_doc",
                           "checkup", "smoke", "alcohol", "urban_rural", "asthma_status")])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


