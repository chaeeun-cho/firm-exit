# Open country code
setwd("~/Downloads")
df <- read.csv(file = 'link_procurement.csv')

library(dplyr)
library(tidyr)


df$KSEstatus <- as.factor(df$KSEstatus)

df <- df %>%
  rename(Firm = Company.name)


# Assuming your data frame is df and you want to exclude a category 'CategoryToExclude'
filtered_df <- df %>% filter(KSEstatus != 'n/a')


library(ggplot2)
library(ggrepel)


ggplot(filtered_df, aes(x = KSEstatus)) +
  geom_bar() +
  labs(title = "Firm's Exit Decision",
       x = "Exit Decision",
       y = "Count") +
  theme_minimal()


ggplot(filtered_df, aes(x = KSEstatus, fill = KSEstatus)) +
  geom_bar() +
  labs(title = "US Firm's Status of Business Decision in Russia",
       x = "Status",
       y = "Count") +
  theme_minimal()




# Assuming 'df' is your data frame, with 'Firm' for firm names and 'Decision' for their decisions.
# We'll add a numeric value to cluster the firms by their decision for plotting purposes.

# Convert the 'Decision' to a factor if it's not already
df$KSEstatus <- as.factor(df$KSEstatus)


# Create a binary variable named "is_withdrawal" that is 1 if "transaction_type" is "withdrawal", and 0 otherwise
df$exit <- ifelse(df$KSEstatus == "exited", 1, 0)

# Create a binary variable named "is_withdrawal" that is 1 if "transaction_type" is "withdrawal", and 0 otherwise
df$exit2 <- ifelse(df$KSEstatus == "exited" | df$KSEstatus == "leave", 1, 0)




# Replace NA with 0
#df$award21[is.na(df$award21)] <- 0



df$revRF21 <- as.numeric(as.character(df$revRF21))
df$staff21 <- as.numeric(as.character(df$staff21))




df$sharerevRF21 <- gsub("%", "", df$sharerevRF21)
df$sharerevRF21 <- gsub(",", ".", df$sharerevRF21)
df$sharerevRF21 <- as.numeric(df$sharerevRF21)
df$assetRF21 <- as.numeric(df$assetRF21)

df$numlocalRU[is.na(df$numlocalRU)] <- 0

df$contract21 <- gsub(",", ".", df$contract21)
df$contract21 <- as.numeric(as.character(df$contract21))



df$globalrev21 <- gsub(" ", "", df$globalrev21)
df$globalrev21 <- as.numeric(as.character(df$globalrev21))


df$contract21[is.na(df$contract21)] <- 0

df$contract21 <- ifelse(df$contract21 <= 0, 0.000001, df$contract21)

df$sharecontract <- df$contract21 / df$globalrev21


df$award21[is.na(df$award21)] <- 0


# Apply the natural logarithm to global revenue and reliance on procurement as a revenue, employee
df$log_globalrev21 <- log(df$globalrev21)
df$log_sharecontract <- log(df$sharecontract)
df$log_staff21 <- log(df$staff21)


summary_stat <- df %>%
  select("Firm", "award21", "contract21", "globalrev21", "log_globalrev21", "sharecontract", "staff21", "log_staff21", "sharerevRF21", "numlocalRU", "assetRF21", "Industry")

summary(summary_stat)


m1 <- glm(exit~ award21,
          family=binomial(link="logit"),
          data = df)

summary(m1)


m1 <- glm(exit ~ award21 + log_globalrev21 + sharerevRF21 + numlocalRU,
          family=binomial(link="logit"),
          data = df)

summary(m1)


m1 <- glm(exit ~ award21 + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + assetRF21,
          family=binomial(link="logit"),
          data = df)

summary(m1)

m1_fe <- glm(exit ~ award21 + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + as.factor(Industry),
             family=binomial(link="logit"),
             data = df)

summary(m1_fe)


m1 <- glm(exit ~ sharecontract,
          family=binomial(link="logit"),
          data = df)


summary(m1)



m1 <- glm(exit ~ sharecontract + log_globalrev21 +  sharerevRF21 + numlocalRU ,
          family=binomial(link="logit"),
          data = df)

summary(m1)



m1 <- glm(exit ~ sharecontract + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + assetRF21,
          family=binomial(link="logit"),
          data = df)

summary(m1)


m1_fe <- glm(exit ~ sharecontract + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + as.factor(Industry),
             family=binomial(link="logit"),
             data = df)

summary(m1_fe)



m1 <- glm(exit ~ award21 + sharecontract + log_globalrev21 + log_staff21,
          family=binomial(link="logit"),
          data = df)

summary(m1)


m1 <- glm(exit ~ award21 + sharecontract + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + assetRF21,
          family=binomial(link="logit"),
          data = df)

summary(m1)


m1_fe <- glm(exit ~ award21 + sharecontract + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + as.factor(Industry),
             family=binomial(link="logit"),
             data = df)

summary(m1_fe)





m1_alt <- glm(exit2~ award21,
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)

m1_alt <- glm(exit2~ award21 + log_globalrev21 + sharerevRF21 + numlocalRU,
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)


m1_alt <- glm(exit2~ award21 + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + as.factor(Industry),
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)

m1_alt <- glm(exit2~ award21 + sharecontract + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU + as.factor(Industry),
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)


m1_alt <- glm(exit2~ sharecontract,
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)




m1_alt <- glm(exit2~ sharecontract + log_globalrev21 + sharerevRF21 + numlocalRU,
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)


m1_alt <- glm(exit2~ sharecontract + log_globalrev21 + log_staff21 + sharerevRF21 + numlocalRU,
              family=binomial(link="logit"),
              data = df)

summary(m1_alt)


library(dotwhisker)
library(sjPlot)
library(sjmisc)

plot_model(m1) + theme_minimal()




library(ggplot2)


model1 <- glm(withdrawal~ sanction*gov10 + revenue + as.factor(Industry),
              family=binomial(link="logit"),
              data = merged_celi)

summary(model1)
















# original code #


# Open country code
setwd("~/Downloads")
library(countrycode)
countrycode <- read.csv(file = 'COW country codes.csv')
celi <- read.csv(file = 'yale_celi.csv')

library(readxl)
kyiv_list <- read_excel("kyiv_list.xlsx")

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
  rename(ccode2 = Russia) %>%
  rename(name = Name)

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
merged_celi$withdrawal <- ifelse(merged_celi$Grade == "Withdrawl", 1, 0)

# Create a binary variable named "is_withdrawal" that is 1 if "transaction_type" is "withdrawal", and 0 otherwise
merged_celi$withdrawal2 <- ifelse(merged_celi$Grade == "Withdrawl" | merged_celi$Grade == "Suspension", 1, 0)

# Create a binary variable named "is_withdrawal" that is 1 if "transaction_type" is "withdrawal", and 0 otherwise
merged_celi$withdrawal3 <- ifelse(merged_celi$Grade == "Withdrawl" | merged_celi$Grade == "Suspension" | merged_celi$Grade == "Scaling Back", 1, 0)


## July 8 ##
# merge orbis revenue and employees
orbis_firm <- read.csv(file = 'orbis_rev.csv')
orbis_firm <- read.csv(file = 'orbis_updated.csv') #figure out

#selected <- orbis_firm %>%
#  select(num, Name)

# Use duplicated() to find duplicated rows
#duplicated_rows <- duplicated(selected$Name)


selected <- orbis_firm %>%
  select(name)

# Use duplicated() to find duplicated rows
duplicated_rows <- duplicated(selected$name)


# Print the duplicated rows
print(orbis_firm[duplicated_rows, ])


########

merged_celi <- merge(merged_celi, orbis_firm, by = c ("name"),all.x = TRUE)

# Rename values in column A
merged_celi$last.avail.yr[merged_celi$last.avail.yr == "#N/A"] <- NA
merged_celi$revenue[merged_celi$revenue == "#N/A"] <- NA
merged_celi$employee[merged_celi$employee == "#N/A"] <- NA

merged_celi$last.avail.yr[merged_celi$last.avail.yr == "n.a."] <- NA
merged_celi$revenue[merged_celi$revenue == "n.a."] <- NA
merged_celi$employee[merged_celi$employee == "n.a."] <- NA

merged_celi$last.avail.yr[merged_celi$last.avail.yr == "#VALUE!"] <- NA
merged_celi$revenue[merged_celi$revenue == "#VALUE!"] <- NA
merged_celi$employee[merged_celi$employee == "#VALUE!"] <- NA


merged_celi$last.avail.yr <- as.numeric(as.character(merged_celi$last.avail.yr))
merged_celi$revenue <- as.numeric(as.character(merged_celi$revenue))
merged_celi$employee <- as.numeric(as.character(merged_celi$employee))


merged_celi$revenue <- log(merged_celi$revenue)




# kyiv_list

names(kyiv_list)



kyiv_list <- kyiv_list %>%
  select(yale_name,num.local, hq, num.staff.21,
         rev.in.RF.22, rev.in.RF.21, capital.RF.22,
         capital.RF.21, asset.RF.22, asset.RF.21,
         tax, profit.tax, globalrev22, shareRUrev22,
         globalrev21, shareRUrev21, num.location.RF, num.location.global)

kyiv_list <- kyiv_list %>%
  rename(name = yale_name)

merged_celi <- merge(merged_celi, kyiv_list, all.x = TRUE)



model1 <- glm(withdrawal~ absidealdiff + revenue ,
              family=binomial(link="logit"),
              data = merged_celi)


summary(model1)

soe <- read.csv(file = 'orbis_soe_jan.csv')

soe <- soe %>%
  rename("name" = "Name")

merged_celi <- merge(merged_celi, soe, by = c ("name"),all.x = TRUE)
merged_celi <- merged_celi %>%
  mutate(sanction = ifelse(is.na(sanction), 0, sanction))

# Use duplicated() to find duplicated rows
duplicated_rows <- duplicated(merged_celi$Name)

# Print the duplicated rows
print(merged_celi[duplicated_rows, ])

# Drop the row with duplicated name

merged_celi <- subset(merged_celi, !(name == "Systembolaget" & is.na(gov10)))

merged_celi$GUO_GOV <- as.numeric(as.character(merged_celi$GUO_GOV))

# Descriptive Figure
dv <- read.csv(file = 'firm_dv.csv')

x <- c("Digging In", "Buying Time", "ScalingBack", "Suspension", "Withdrawal")
y <- c(234, 176, 148, 503, 520)

barplot(y, names.arg = x, xlab = "Response Type", ylab = "Frequency", main = "Summary of Firms’ Response to Economic Sanctions Against Russia")



#install.packages("ggplot2")
#install.packages("sf")
#install.packages("maps")

library(ggplot2)
library(sf)
library(maps)


# Convert the target variable to a factor

merged_celi$Withdrawal = as.factor(merged_celi$withdrawal)


ggplot(merged_celi, aes(Industry, fill = Withdrawal)) +
  geom_bar() +
  coord_flip()


# Summarize firm data to get counts of state-owned enterprises by country
ownership_summary <- merged_celi %>%
  group_by(Country) %>%
  summarize(soe_count = sum(GUO_GOV == 1, na.rm = TRUE))



world_map <- map_data("world")
map_data_merged <- merge(world_map, ownership_summary, by.x = "region", by.y = "Country", all.x = TRUE)


ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(title = "State-Owned Enterprises by Country",
       subtitle = "Global map showing the count of state-owned enterprises by country",
       fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")



ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")



# GOV10


merged_celi <- merged_celi %>% mutate(
  gov10 = ifelse(GUO_GOV == 1, 1, gov10),
  gov20 = ifelse(GUO_GOV == 1, 1, gov20),
  gov25 = ifelse(GUO_GOV == 1, 1, gov25),
  gov30 = ifelse(GUO_GOV == 1, 1, gov30),
  gov40 = ifelse(GUO_GOV == 1, 1, gov40),
  gov50 = ifelse(GUO_GOV == 1, 1, gov50)
)


ownership10 <- merged_celi %>%
  filter(gov10 == 0 & GUO_GOV == 1) %>%
  select(name, Country, GUO_GOV, gov10)

ownership_summary <- merged_celi %>%
  group_by(Country) %>%
  summarize(soe_count = sum(gov10 == 1, na.rm = TRUE))



world_map <- map_data("world")
map_data_merged <- merge(world_map, ownership_summary, by.x = "region", by.y = "Country", all.x = TRUE)



plot_gov10 <-ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(title = "State-Owned Enterprises by Country (>=10%)",
       subtitle = "Global map showing the count of state-owned enterprises by country",
       fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")


# gov20


ownership_summary <- merged_celi %>%
  group_by(Country) %>%
  summarize(soe_count = sum(gov20 == 1, na.rm = TRUE))



world_map <- map_data("world")
map_data_merged <- merge(world_map, ownership_summary, by.x = "region", by.y = "Country", all.x = TRUE)


plot_gov20 <-ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(title = "State-Owned Enterprises by Country",
       subtitle = "Global map showing the count of state-owned enterprises by country",
       fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")


# gov30


ownership_summary <- merged_celi %>%
  group_by(Country) %>%
  summarize(soe_count = sum(gov30 == 1, na.rm = TRUE))



world_map <- map_data("world")
map_data_merged <- merge(world_map, ownership_summary, by.x = "region", by.y = "Country", all.x = TRUE)


plot_gov30 <-ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(title = "State-Owned Enterprises by Country",
       subtitle = "Global map showing the count of state-owned enterprises by country",
       fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")

# gov40


ownership_summary <- merged_celi %>%
  group_by(Country) %>%
  summarize(soe_count = sum(gov40 == 1, na.rm = TRUE))



world_map <- map_data("world")
map_data_merged <- merge(world_map, ownership_summary, by.x = "region", by.y = "Country", all.x = TRUE)


plot_gov40 <-ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(title = "",
       subtitle = "",
       fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")


# gov50


ownership_summary <- merged_celi %>%
  group_by(Country) %>%
  summarize(soe_count = sum(gov50 == 1, na.rm = TRUE))



world_map <- map_data("world")
map_data_merged <- merge(world_map, ownership_summary, by.x = "region", by.y = "Country", all.x = TRUE)


plot_gov50 <-ggplot() +
  geom_polygon(data = map_data_merged, aes(x = long, y = lat, group = group, fill = soe_count)) +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust color scale
  labs(title = "",
       subtitle = "",
       fill = "SOE Count") +
  theme_minimal() +
  theme(legend.position = "bottom")



library(ggpubr)

combined_plot <- ggarrange(plot_gov10, plot_gov20, plot_gov30, plot_gov40, plot_gov50, ncol = 3, nrow = 2)
print(combined_plot)


##########################################################

# Add Control Variables

##########################################################


merged_celi$energy <- ifelse(merged_celi$Industry == "Energy", 1, 0)
merged_celi$consumer <- ifelse(merged_celi$Industry == "Consumer Discretionary", 1, 0)



#Regime Type: PolityIV
library(readxl)
polity_df <- read_excel("~/Downloads/p5v2018.xls")

polity_df <- polity_df %>%
  select(c(ccode, year, polity2)) %>%
  filter(year == 2018) 

polity_df <- polity_df %>%
  mutate(democ = ifelse(polity2 >= 6, 1, 0))  %>%
  select(-polity2) %>%
  rename(ccode1 = ccode)

merged_celi <- merge(merged_celi, polity_df, by = c("ccode1"), all.x = TRUE) # merge primary sender's coercive over the target


# gdppc
mpd2020_gdppc <- read_excel("mpd2020_gdppc.xlsx")


countrycode <- countrycode %>%
  distinct(CCode, .keep_all = TRUE)

#start from here

gdppc <- mpd2020_gdppc %>%
  rename(StateAbb = countrycode) %>%
  filter(year >= 1944)


countrycode <- read.csv(file = 'COW country codes.csv')

countrycode <- countrycode %>%
  distinct(CCode, .keep_all = TRUE)

gdppc <- merge(gdppc, countrycode, by = "StateAbb", all.x = TRUE, all.y = TRUE)

gdppc <- gdppc %>%
  select(-c(StateNme)) %>%
  rename(StateNme = country)

gdppc <- merge(gdppc, countrycode, by = "StateNme", all.x = TRUE, all.y = TRUE)

# Replace values in CCode.x with values from CCode.y if CCode.x is NA
gdppc$CCode.x[is.na(gdppc$CCode.x)] <- gdppc$CCode.y[is.na(gdppc$CCode.x)]

gdppc <- gdppc %>%
  select(-c(CCode.y)) %>%
  rename(CCode = CCode.x)


#Viet Nam, U.R. of Tanzania: Mainland, TFYR of Macedonia, Taiwan, Province of China, Sudan (Former), State of Palestine,
# Serbia, Saint Lucia, Republic of Moldova, Puerto Rico, Former USSR, D.R. of the Congo, Côte d'Ivoire, China, Hong Kong SAR, Cabo Verde
gdppc$CCode[gdppc$StateNme == "Viet Nam"] <- 816
gdppc$CCode[gdppc$StateNme == "U.R. of Tanzania: Mainland"] <- 510
gdppc$CCode[gdppc$StateNme == "TFYR of Macedonia"] <- 343
gdppc$CCode[gdppc$StateNme == "Taiwan, Province of China"] <- 713
gdppc$CCode[gdppc$StateNme == "Sudan (Former)"] <- 625
gdppc$CCode[gdppc$StateNme == "Republic of Moldova"] <- 359
gdppc$CCode[gdppc$StateNme == "Former USSR"] <- 365
gdppc$CCode[gdppc$StateNme == "D.R. of the Congo"] <- 490
gdppc$CCode[gdppc$StateNme == "Cabo Verde"] <- 402
gdppc$CCode[gdppc$StateNme == "Saint Lucia"] <- 56

subset_df <- subset(gdppc, is.na(CCode))

subset_df <- subset_df %>%
  distinct(StateNme, .keep_all = TRUE)

#CAP   402                       Cape Verde
#SLU    56                        St. Lucia


gdppc <- gdppc %>%
  select(c(CCode, year, gdppc)) %>%
  rename(primarysender = CCode) %>%
  rename(gdppc_primarysender = gdppc) %>%
  rename(startyear = year)


list_soe <- merged_celi %>%
  filter(GUO_GOV == 1)

list_soe <- list_soe %>%
  select(name, StateNme, Industry, Grade)


write.csv(list_soe, file = "~/Downloads/list_soe.csv", row.names = FALSE)

#############################

# Run Logit 
# DV = Withdrwal 

########################################
# Consumer * Size
########################################

library(dotwhisker)
library(sjPlot)
library(sjmisc)

library(tradepolicy)



model1 <- glm(withdrawal~ sanction*gov10 + revenue + energy + consumer,
              family=binomial(link="logit"),
              data = merged_celi)

summary(model1)


model1 <- glm(withdrawal~ sanction*gov10 + revenue + as.factor(Industry),
              family=binomial(link="logit"),
              data = merged_celi)

summary(model1)


model1 <- glm(withdrawal~ sanction*gov10 + revenue + energy,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)



model1 <- glm(withdrawal~consumer*revenue,
              family=binomial(link="logit"),
              data = merged_celi)

model1 <- glm(withdrawal~consumer*revenue*sanction,
              family=binomial(link="logit"),
              data = merged_celi)

########################################
# IV1: sanctioning vs. non-sanctioning
########################################

model1 <- glm(withdrawal~GUO_GOV,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

#SANCTION
sanction <- glm(withdrawal~sanction,
                family=binomial(link="logit"),
                data = merged_celi)
summary(sanction)

plot_model(model1)

model1 <- glm(withdrawal~ sanction + GUO_GOV + revenue,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)


plot_model(model1)


model1 <- glm(withdrawal~sanction+ GUO_GOV + employee,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)



model1 <- glm(withdrawal~ GUO_GOV + sanction + GUO_GOV * sanction,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)


model1 <- glm(withdrawal~ sanction + GUO_GOV + revenue + factor(Industry),
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)


model1.f <- glm(withdrawal~ sanction * GUO_GOV + revenue + energy,
                family=binomial(link="logit"),
                data = merged_celi)
summary(model1.f)

plot_model(model1)


model_soe <- glm(withdrawal~ sanction * GUO_GOV,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

library(dotwhisker)
library(sjPlot)
library(sjmisc)


plot_model(model_soe, type="int",  axis.title = "Withdrawal", title = "Figure 3. Predicted Probabilities of Withdrawal") + theme_minimal()



model_soe <- glm(withdrawal~ sanction * GUO_GOV
                 + revenue,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)


model_soe <- glm(withdrawal~ sanction * GUO_GOV
                 + revenue + democ,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()

model1.fe <- glm(withdrawal~sanction * GUO_GOV
                 + revenue  + democ + factor(Industry),
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model1.fe)

plot_model(model1.fe, type="int") + theme_minimal()


model_soe.fe <- glm(withdrawal~sanction * GUO_GOV + factor(Industry) ,
                    family=binomial(link="logit"),
                    data = merged_celi)

summary(model_soe.fe)
plot_model(model_soe.fe, type="int") + theme_minimal()

library(stargazer)
stargazer(model_soe.fe)

### gov10 & DV = withdrawal ###

model_gov10 <- glm(withdrawal~ sanction * gov10 + revenue + energy,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)


model_gov10 <- glm(withdrawal~ sanction * gov10 + revenue + democ,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal~ sanction * gov20 + revenue + energy,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov20, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal~ sanction * gov20 + revenue + democ,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

model_gov25 <- glm(withdrawal~ sanction * gov25 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)

model_gov25 <- glm(withdrawal~ sanction * gov25 + revenue + democ,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


plot_model(model_gov25, type="int") + theme_minimal()

model_gov30 <- glm(withdrawal~ sanction * gov30 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)


model_gov30 <- glm(withdrawal~ sanction * gov30 + revenue + democ,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()


model_gov40 <- glm(withdrawal~ sanction * gov40 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

model_gov40 <- glm(withdrawal~ sanction * gov40 + revenue + democ,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()



model_gov50 <- glm(withdrawal~ sanction * gov50 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)


model_gov50 <- glm(withdrawal~ sanction * gov50 + revenue + democ,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

plot_model(model_gov50, type="int", title = "",
           legend.title = "Government as a Majority Share Holder", axis.title = c("Home Country's Participation in a Sanction", "Firm Exit")) + theme_minimal()


### fixed effect ###
### gov10 & DV = withdrawal ###

model_gov10 <- glm(withdrawal~ sanction * gov10 + revenue + energy + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)


model_gov10 <- glm(withdrawal~ sanction * gov10 + revenue + democ + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal~ sanction * gov20 + revenue + energy + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov20, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal~ sanction * gov20 + revenue + democ + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

model_gov25 <- glm(withdrawal~ sanction * gov25 + revenue + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)

model_gov25 <- glm(withdrawal~ sanction * gov25 + revenue + democ + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


plot_model(model_gov25, type="int") + theme_minimal()

model_gov30 <- glm(withdrawal~ sanction * gov30 + revenue + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)


model_gov30 <- glm(withdrawal~ sanction * gov30 + revenue + democ + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()


model_gov40 <- glm(withdrawal~ sanction * gov40 + revenue + as.factor(Industry)+ as.factor(Country),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

model_gov40 <- glm(withdrawal~ sanction * gov40 + revenue + democ + as.factor(Industry) + as.factor(Country),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()




# Use gsub to replace '%' with '' (empty string)
merged_celi$shareRUrev21 <- gsub("%", "", merged_celi$shareRUrev21)

# Optionally, convert the variable to a numeric type if needed
merged_celi$shareRUrev21 <- as.numeric(merged_celi$shareRUrev21)


model_gov50 <- glm(withdrawal~ sanction * gov50 + revenue +  num.location.RF + democ + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

model_gov50 <- glm(withdrawal~ sanction * gov50 + revenue + democ
                   + as.factor(Industry),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

plot_model(model_gov50, type="int", title = "",
           legend.title = "Government as a Majority Share Holder", axis.title = c("Home Country's Participation in a Sanction", "Firm Exit")) + theme_minimal()


# Create a table using stargazer
library(stargazer)
stargazer(model_gov50, type = "text",
          covariate.labels = c("Sanction", 
                               "Government as a majority", 
                               "Revenue",
                               "Joint Democracy",
                               "Sanction * Government as a majority"))

stargazer(model_gov50, type = "latex",
          covariate.labels = c("Sanction", 
                               "Government as a majority", 
                               "Revenue",
                               "Joint Democracy",
                               "Sanction * Government as a majority"))


############################
# gov10 & DV = withdrawal2
############################

model_gov10 <- glm(withdrawal2~ sanction * gov10,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov10 <- glm(withdrawal2~ sanction * gov10 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()

model_gov20 <- glm(withdrawal2~ sanction * gov20,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov20, type="int") + theme_minimal()

model_gov20 <- glm(withdrawal2~ sanction * gov20 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)
plot_model(model_gov20, type="int") + theme_minimal()

model_gov25 <- glm(withdrawal2~ sanction * gov25,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


model_gov25 <- glm(withdrawal2~ sanction * gov25 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)

plot_model(model_gov25, type="int") + theme_minimal()


model_gov30 <- glm(withdrawal2~ sanction * gov30,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()


model_gov30 <- glm(withdrawal2~ sanction * gov30 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()

model_gov40 <- glm(withdrawal2~ sanction * gov40,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()

model_gov40 <- glm(withdrawal2~ sanction * gov40 + revenuee,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()

model_gov50 <- glm(withdrawal2~ sanction * gov50,
                   family=binomial(link="logit"),
                   data = merged_celi)

plot_model(model_gov50, type="int") + theme_minimal()

model_gov50 <- glm(withdrawal2~ sanction * gov50 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

plot_model(model_gov50, type="int") + theme_minimal()


model_gov50 <- glm(withdrawal2~ sanction * gov50 + revenue + factor(energy) + factor(consumer),
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

plot_model(model_gov50, type="int") + theme_minimal()



model_gov50 <- glm(withdrawal2~ sanction * gov50 + revenue*consumer + energy,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

# gov10 & DV = withdrawal3

model_gov10 <- glm(withdrawal3~ sanction * gov10,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()

model_gov10 <- glm(withdrawal3~ sanction * gov10 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal3~ sanction * gov20,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov30, type="int") + theme_minimal()

model_gov20 <- glm(withdrawal3~ sanction * gov20 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)
plot_model(model_gov20, type="int") + theme_minimal()


model_gov25 <- glm(withdrawal3~ sanction * gov25,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)

plot_model(model_gov25, type="int") + theme_minimal()

model_gov25 <- glm(withdrawal3~ sanction * gov25 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


model_gov30 <- glm(withdrawal3~ sanction * gov30,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()



model_gov30 <- glm(withdrawal3~ sanction * gov30 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()

model_gov40 <- glm(withdrawal3~ sanction * gov40,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)


plot_model(model_gov40, type="int") + theme_minimal()


model_gov40 <- glm(withdrawal3~ sanction * gov40 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()


model_gov50 <- glm(withdrawal3~ sanction * gov50,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)
plot_model(model_gov50, type="int") + theme_minimal()

model_gov50 <- glm(withdrawal3~ sanction * gov50 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)
plot_model(model_gov50, type="int") + theme_minimal()


selected <- merged_celi %>%
  filter(GUO_GOV == 1)


m_industry <- glm(withdrawal ~ Industry  + revenue, family=binomial(link="logit"),
                  data = merged_celi)
summary(m_industry)

plot_model(m_industry) + theme_minimal()

## other controls

# Sending level: country GDP, tax haven status, democracy score (Polity), US ally. Provincial fixed effects 
# WTO membership of the sending country, polity of the sending country
# BIT with Russia?


#From other dataset
#1. FDI market: level of economic reliance on Russia
#2. Firms’ market capitalization: capital-intensive firms


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



###################


setwd("~/Desktop")
library(countrycode)
# countrycode <- read.csv(file = 'COW country codes.csv')
celi <- read.csv(file = 'yale_celi_20230708.csv')

celi <- celi %>%
  select(name, Grade, Action, Country, Industry, GUO, GUO_GOV)

library(dplyr)
library(tidyr)


orbis <- read.csv(file = 'Orbis_Az_US_Israel_latest_20230707.csv')


orbis <- orbis %>% filter(X != 'NA')

write.csv(orbis, file = "~/Downloads/orbis.csv", row.names = FALSE)


Andorra_Canada_latest <- read.csv(file = '~/Downloads/Adorra to Canada_2022.csv')


orbis <- orbis %>%
  select(-X)

orbis <- unique(orbis)





merged_celi <- merge(celi, orbis, by = "name")
merged_celi <- unique(merged_celi)





####
# Install and load the necessary package

install.packages("nnet")
library(nnet)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create the multinomial logit regression model
model <- multinom(formula = Grade ~ sanction + GUO_GOV, data = merged_celi)

# Print the summary of the model
summary(model)

# Predict on the test data
predictions <- predict(model, newdata = merged_celi, type = "class")

# Evaluate the model
accuracy <- sum(predictions == merged_celi$Grade) / nrow(merged_celi)
print(paste("Accuracy:", accuracy))



# To address missing value:

# Predict on the test data
predictions <- predict(model, newdata = merged_celi, type = "class")

# Remove missing values from predictions and actual values
predictions <- na.omit(predictions)
actual_values <- na.omit(merged_celi$Grade)

# Evaluate the model
accuracy <- sum(predictions == actual_values) / length(actual_values)
print(paste("Accuracy:", accuracy))






# Multinomial analysis
#install.packages("nnet")
library(nnet)

merged_celi$Grade2 <- relevel(merged_celi$Grade, ref = "Digging In")

model <- multinom(Grade2 ~ sanction + revenue,
                  data = merged_celi)

summary(model)

exp(coef(model))

z <- summary(model)$coefficients/summary(model)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

##




############################
# foreign policy alignment
############################
model1 <- glm(withdrawal~GUO_GOV,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)

model1 <- glm(withdrawal~GUO_GOV + absidealdiff + revenue,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)


plot_model(model1)


model1 <- glm(withdrawal~GUO_GOV + absidealdiff + employee,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)



model1 <- glm(withdrawal~ GUO_GOV + absidealdiff + GUO_GOV * absidealdiff,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)


model1 <- glm(withdrawal~ GUO_GOV * absidealdiff+ revenue + factor(Industry),
              family=binomial(link="logit"),
              data = merged_celi)
summary(model1)

plot_model(model1)


model_soe <- glm(withdrawal~ absidealdiff * GUO_GOV,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

library(dotwhisker)
library(sjPlot)
library(sjmisc)


plot_model(model_soe, type="int",  axis.title = "Withdrawal", title = "Figure 3. Predicted Probabilities of Withdrawal") + theme_minimal()



model_soe <- glm(withdrawal~ absidealdiff * GUO_GOV
                 + revenue,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()

model1.fe <- glm(withdrawal~absidealdiff * GUO_GOV
                 + revenue  + factor(Industry),
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model1.fe)

plot_model(model1.fe, type="int") + theme_minimal()


model_soe.fe <- glm(withdrawal~absidealdiff * GUO_GOV + factor(Industry),
                    family=binomial(link="logit"),
                    data = merged_celi)

summary(model_soe.fe)
plot_model(model_soe.fe, type="int") + theme_minimal()


# DV = withdrawal2

model2 <- glm(withdrawal2 ~  absidealdiff * GUO_GOV,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model2)

plot_model(model2, type="int") + theme_minimal()


model_soe <- glm(withdrawal2~ absidealdiff * GUO_GOV
                 + revenue ,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()


model1.fe <- glm(withdrawal2~absidealdiff * GUO_GOV
                 + revenue + factor(Industry),
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model1.fe)

plot_model(model1.fe, type="int") + theme_minimal()

model_soe.fe <- glm(withdrawal2~absidealdiff * GUO_GOV + factor(Industry),
                    family=binomial(link="logit"),
                    data = merged_celi)

summary(model_soe.fe)

plot_model(model_soe.fe, type="int") + theme_minimal()

# DV = withdrawal3

model3 <- glm(withdrawal3~absidealdiff,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model3)

model3 <- glm(withdrawal3~absidealdiff + revenue + employee,
              family=binomial(link="logit"),
              data = merged_celi)

summary(model3)


model3 <- glm(withdrawal3~GUO_GOV,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model3)

model3 <- glm(withdrawal3~GUO_GOV + revenue + employee,
              family=binomial(link="logit"),
              data = merged_celi)

summary(model3)


model_soe <- glm(withdrawal3~ absidealdiff * GUO_GOV,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()

model_soe <- glm(withdrawal3~ absidealdiff * GUO_GOV
                 + revenue,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()

model1.fe <- glm(withdrawal3~absidealdiff * GUO_GOV
                 + revenue + factor(Industry),
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model1.fe)

plot_model(model1.fe, type="int") + theme_minimal()

model_soe.fe <- glm(withdrawal3~absidealdiff * GUO_GOV + factor(Industry),
                    family=binomial(link="logit"),
                    data = merged_celi)

summary(model_soe.fe)

plot_model(model_soe.fe, type="int") + theme_minimal()


### gov10 & DV = withdrawal ###

model_gov10 <- glm(withdrawal~ absidealdiff * gov10,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal~ absidealdiff * gov20,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov20, type="int") + theme_minimal()

model_gov25 <- glm(withdrawal~ absidealdiff * gov25,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


plot_model(model_gov25, type="int") + theme_minimal()

model_gov30 <- glm(withdrawal~ absidealdiff * gov30,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()


model_gov40 <- glm(withdrawal~ absidealdiff * gov40,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()


model_gov50 <- glm(withdrawal~ absidealdiff * gov50,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

plot_model(model_gov50, type="int") + theme_minimal()



# gov10 & DV = withdrawal2


model_gov10 <- glm(withdrawal2~ absidealdiff * gov10,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov10 <- glm(withdrawal2~ absidealdiff * gov10 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()

model_gov20 <- glm(withdrawal2~ absidealdiff * gov20,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov20, type="int") + theme_minimal()

model_gov20 <- glm(withdrawal2~ absidealdiff * gov20 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)
plot_model(model_gov20, type="int") + theme_minimal()

model_gov25 <- glm(withdrawal2~ absidealdiff * gov25,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


model_gov25 <- glm(withdrawal2~ absidealdiff * gov25 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)

plot_model(model_gov25, type="int") + theme_minimal()


model_gov30 <- glm(withdrawal2~ absidealdiff * gov30,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()


model_gov30 <- glm(withdrawal2~ absidealdiff * gov30 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()

model_gov40 <- glm(withdrawal2~ absidealdiff * gov40,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()

model_gov40 <- glm(withdrawal2~ absidealdiff * gov40 + revenuee,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()

model_gov50 <- glm(withdrawal2~ absidealdiff * gov50,
                   family=binomial(link="logit"),
                   data = merged_celi)

plot_model(model_gov50, type="int") + theme_minimal()

model_gov50 <- glm(withdrawal2~ absidealdiff * gov50 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)

plot_model(model_gov50, type="int") + theme_minimal()


# gov10 & DV = withdrawal3

model_gov10 <- glm(withdrawal3~ absidealdiff * gov10,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()

model_gov10 <- glm(withdrawal3~ absidealdiff * gov10 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov10)

plot_model(model_gov10, type="int") + theme_minimal()


model_gov20 <- glm(withdrawal3~ absidealdiff * gov20,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)

plot_model(model_gov30, type="int") + theme_minimal()

model_gov20 <- glm(withdrawal3~ absidealdiff * gov20 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov20)
plot_model(model_gov20, type="int") + theme_minimal()


model_gov25 <- glm(withdrawal3~ absidealdiff * gov25,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)

plot_model(model_gov25, type="int") + theme_minimal()

model_gov25 <- glm(withdrawal3~ absidealdiff * gov25 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov25)


model_gov30 <- glm(withdrawal3~ absidealdiff * gov30,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()



model_gov30 <- glm(withdrawal3~ absidealdiff * gov30 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov30)

plot_model(model_gov30, type="int") + theme_minimal()

model_gov40 <- glm(withdrawal3~ absidealdiff * gov40,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)


plot_model(model_gov40, type="int") + theme_minimal()


model_gov40 <- glm(withdrawal3~ absidealdiff * gov40 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov40)

plot_model(model_gov40, type="int") + theme_minimal()


model_gov50 <- glm(withdrawal3~ absidealdiff * gov50,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)
plot_model(model_gov50, type="int") + theme_minimal()

model_gov50 <- glm(withdrawal3~ absidealdiff * gov50 + revenue,
                   family=binomial(link="logit"),
                   data = merged_celi)

summary(model_gov50)
plot_model(model_gov50, type="int") + theme_minimal()


selected <- merged_celi %>%
  filter(GUO_GOV == 1)


m_industry <- glm(withdrawal ~ Industry  + revenue, family=binomial(link="logit"),
                  data = merged_celi)
summary(m_industry)

plot_model(m_industry) + theme_minimal()

## other controls

# Sending level: country GDP, tax haven status, democracy score (Polity), US ally. Provincial fixed effects 
# WTO membership of the sending country, polity of the sending country
# BIT with Russia?


#From other dataset
#1. FDI market: level of economic reliance on Russia
#2. Firms’ market capitalization: capital-intensive firms


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



###################


setwd("~/Desktop")
library(countrycode)
# countrycode <- read.csv(file = 'COW country codes.csv')
celi <- read.csv(file = 'yale_celi_20230708.csv')

celi <- celi %>%
  select(name, Grade, Action, Country, Industry, GUO, GUO_GOV)

library(dplyr)
library(tidyr)


orbis <- read.csv(file = 'Orbis_Az_US_Israel_latest_20230707.csv')


orbis <- orbis %>% filter(X != 'NA')

write.csv(orbis, file = "~/Downloads/orbis.csv", row.names = FALSE)


Andorra_Canada_latest <- read.csv(file = '~/Downloads/Adorra to Canada_2022.csv')


orbis <- orbis %>%
  select(-X)

orbis <- unique(orbis)





merged_celi <- merge(celi, orbis, by = "name")
merged_celi <- unique(merged_celi)





####
# Install and load the necessary package

install.packages("nnet")
library(nnet)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create the multinomial logit regression model
model <- multinom(formula = Grade ~ absidealdiff + GUO_GOV, data = merged_celi)

# Print the summary of the model
summary(model)

# Predict on the test data
predictions <- predict(model, newdata = merged_celi, type = "class")

# Evaluate the model
accuracy <- sum(predictions == merged_celi$Grade) / nrow(merged_celi)
print(paste("Accuracy:", accuracy))



# To address missing value:

# Predict on the test data
predictions <- predict(model, newdata = merged_celi, type = "class")

# Remove missing values from predictions and actual values
predictions <- na.omit(predictions)
actual_values <- na.omit(merged_celi$Grade)

# Evaluate the model
accuracy <- sum(predictions == actual_values) / length(actual_values)
print(paste("Accuracy:", accuracy))






# Multinomial analysis
#install.packages("nnet")
library(nnet)

merged_celi$Grade2 <- relevel(merged_celi$Grade, ref = "Digging In")

model <- multinom(Grade2 ~ absidealdiff + revenue,
                  data = merged_celi)

summary(model)

exp(coef(model))

z <- summary(model)$coefficients/summary(model)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

##



# DV = withdrawal2

model2 <- glm(withdrawal2 ~  sanction * GUO_GOV,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model2)

plot_model(model2, type="int") + theme_minimal()


model_soe <- glm(withdrawal2~ sanction * GUO_GOV
                 + revenue ,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()


model1.fe <- glm(withdrawal2~sanction * GUO_GOV
                 + revenue + factor(Industry),
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model1.fe)

plot_model(model1.fe, type="int") + theme_minimal()

model_soe.fe <- glm(withdrawal2~sanction * GUO_GOV + factor(Industry),
                    family=binomial(link="logit"),
                    data = merged_celi)

summary(model_soe.fe)

plot_model(model_soe.fe, type="int") + theme_minimal()

# DV = withdrawal3

model3 <- glm(withdrawal3~sanction,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model3)

model3 <- glm(withdrawal3~sanction + revenue + employee,
              family=binomial(link="logit"),
              data = merged_celi)

summary(model3)


model3 <- glm(withdrawal3~GUO_GOV,
              family=binomial(link="logit"),
              data = merged_celi)
summary(model3)

model3 <- glm(withdrawal3~GUO_GOV + revenue + employee,
              family=binomial(link="logit"),
              data = merged_celi)

summary(model3)


model_soe <- glm(withdrawal3~ sanction * GUO_GOV,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()

model_soe <- glm(withdrawal3~ sanction * GUO_GOV
                 + revenue,
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model_soe)

plot_model(model_soe, type="int") + theme_minimal()

model1.fe <- glm(withdrawal3~sanction * GUO_GOV
                 + revenue + factor(Industry),
                 family=binomial(link="logit"),
                 data = merged_celi)

summary(model1.fe)

plot_model(model1.fe, type="int") + theme_minimal()

model_soe.fe <- glm(withdrawal3~sanction * GUO_GOV + factor(Industry),
                    family=binomial(link="logit"),
                    data = merged_celi)

summary(model_soe.fe)

plot_model(model_soe.fe, type="int") + theme_minimal()
