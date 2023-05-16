# Open country code
setwd("~/Downloads")
library(countrycode)
countrycode <- read.csv(file = 'COW country codes.csv')
celi <- read.csv(file = 'yale_celi.csv')

library(dplyr)
library(tidyr)


# Update factor levels
levels(celi$Country)[levels(celi$Country) == "United States"] <- "United States of America"


# Rename values in column A
celi$Country[celi$Country == "United States"] <- "United States of America"


celi <- celi %>%
  rename(StateNme = Country) 

countrycode <- countrycode %>%
  select("CCode", "StateNme")

merged_celi <- merge(celi, countrycode, by = "StateNme")
merged_celi <- unique(merged_celi)

merged_celi$Russia <- 365
merged_celi$year <- 2014

merged_celi <- merged_celi %>%
  rename(ccode1 = CCode) %>%
  rename(ccode2 = Russia)

################################################################
#### Merge ideal point distance from Bailey et. al (2017) #####
###############################################################

# Import Ideal point estimates (Bailey et al 2017) dataset

library(haven)
Dyadicdata <- read_dta("Dyadicdata.dta")

#lagged year variable

library("dplyr")

Dyadicdata_lag <- Dyadicdata %>%
  select(ccode2, ccode1, year, absidealdiff) %>%
  group_by(ccode2, ccode1) %>%
  dplyr::mutate(lag_absidealdiff = lag(absidealdiff, n = 1, default = NA))


data_dyad_ideal <- Dyadicdata_lag  %>%
  select(ccode2, ccode1, year, absidealdiff)

merged_celi <- merge(merged_celi, data_dyad_ideal, by=c("ccode1", "ccode2", "year"))

# Create a binary variable named "is_withdrawal" that is 1 if "transaction_type" is "withdrawal", and 0 otherwise
merged_celi$withdrawl <- ifelse(merged_celi$Grade == "Withdrawl", 1, 0)


# Quick Run: logit


model1 <- glm(withdrawl~absidealdiff,
               family=binomial(link="logit"),
               data = merged_celi)

summary(model1)

model1.fe <- glm(withdrawl~absidealdiff + factor(Industry),
               family=binomial(link="logit"),
               data = merged_celi)

summary(model1.fe)


# Use the sandwich and lmtest packages to estimate the robust standard errors.

library(sandwich)
library(lmtest)

# Compute robust standard errors using the sandwich package
robust_se <- sqrt(diag(vcovHC(model1, type = "HC")))
robust_se_fe <- sqrt(diag(vcovHC(model1.fe, type = "HC")))

# Compute the z-score for each coefficient
z_scores <- coef(model1) / robust_se
z_scores_fe<- coef(model1.fe) / robust_se_fe

# Compute p-values using the lmtest package
p_values <- 2 * (1 - pnorm(abs(z_scores)))
p_values_fe <- 2 * (1 - pnorm(abs(z_scores_fe)))

# View the results
summary(model1.fe)
cbind(coef(model1.fe), robust_se, z_scores, p_values)


# Create a table using stargazer
library(stargazer)
stargazer(model1.fe, type = "latex", 
          se = list(robust_se_fe), 
          coef = list(coef(model1.fe)), 
          t = list(z_scores_fe), 
          p = list(p_values_fe),
          title = "Logistic Regression Model Results",
          header = FALSE,
          digits = 2, omit = "startyear")


stargazer(model1, model1.fe, type = "latex", 
          se = list(robust_se, robust_se_fe), 
          coef = list(coef(model1), coef(model1.fe)), 
          t = list(z_scores, z_scores_fe), 
          p = list(p_values, p_values_fe),
          title="Foreign Policy Similarity and Firm's Exit",
          header = FALSE,
          covariate.labels = c("Foreign Policy Similarity"),
          font.size="scriptsize",
          style = "ajps",
          digits = 2,
          align=TRUE,
          no.space=TRUE,
          omit = "Industry",
          add.lines = list(c("Industry FE", "No", "YES")))


stargazer(model1, model1.fe, type = "latex", 
          se = list(robust_se, robust_se_fe), 
          coef = list(coef(model1), coef(model1.fe)), 
          t = list(z_scores, z_scores_fe), 
          p = list(p_values, p_values_fe),
          title="Foreign Policy Similarity and Firm's Exit",
          header = FALSE,
          covariate.labels = c("Foreign Policy Similarity"),
          style = "ajps",
          digits = 2,
          align=TRUE,
          no.space=TRUE,
          omit = "Industry",
          add.lines = list(c("Industry FE", "No", "YES")))

