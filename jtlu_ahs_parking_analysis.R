## AHS parking analysis

ahs_ca <- readRDS("data/ahs_ca_0110.rds")
ahs_west <- readRDS("data/ahs_west_0110.rds")

###############parking instrumental variables
## share of housing units with bundled parking in each MSA
## (units with garage/carport + units with driveway/off-street parking)/total units of housing
ahs_ca$bundled_parking <- NA
ahs_ca$bundled_parking[ahs_ca$garage_carport==1 | ahs_ca$offstr_park==1] <- 1
ahs_ca$bundled_parking[ahs_ca$garage_carport==0 & ahs_ca$offstr_park==0] <- 0
table(ahs_ca$bundled_parking, useNA = "ifany")
table(ahs_ca$garage_carport, useNA = "ifany")
table(ahs_ca$offstr_park, useNA = "ifany")

## create bundled parking variable for ahs_west
ahs_west$bundled_parking <- NA
ahs_west$bundled_parking[ahs_west$garage_carport==1 | ahs_west$offstr_park==1] <- 1
ahs_west$bundled_parking[ahs_west$garage_carport==0 & ahs_west$offstr_park==0] <- 0
table(ahs_west$bundled_parking, useNA = "ifany")
table(ahs_west$garage_carport, useNA = "ifany")
table(ahs_west$offstr_park, useNA = "ifany")

library(dplyr)
bdpark_msa <- ahs_ca[,c("msa","bundled_parking")] %>%
  group_by(msa) %>%
  summarise(share_bundledpark_units = mean(bundled_parking,na.rm = T))

sum(!is.na(ahs_ca$bundled_parking))

## share of housing units with garage/carport in each MSA
ahs_ca$garage_carport_n <- ahs_ca$garage_carport %>% 
  as.numeric() %>%
  dplyr::recode(`1`=1,`2`=0)

table(ahs_ca$garage_carport_n)
table(ahs_ca$garage_carport)

grg_msa <- ahs_ca[,c("msa","garage_carport_n","bundled_parking")] %>%
  group_by(msa) %>%
  summarise(share_units_garage = sum(garage_carport_n,na.rm = T)/sum(!is.na(bundled_parking)))

## share of housing units with driveway/off-street parking in each MSA
ahs_ca$offstr_park_n <- ahs_ca$offstr_park %>% 
  as.numeric() %>%
  dplyr::recode(`1`=1,`2`=0)

table(ahs_ca$offstr_park)
table(ahs_ca$offstr_park_n)

offstr_msa <- ahs_ca[,c("msa","offstr_park_n","bundled_parking")] %>%
  group_by(msa) %>%
  summarise(share_units_offstr = sum(offstr_park_n,na.rm = T)/sum(!is.na(bundled_parking)))

#########merge three dataframes
library(tidyverse)
df_list <- list(bdpark_msa,grg_msa,offstr_msa)
parking <- df_list %>% reduce(full_join,by="msa")

## write to csv
write.csv(parking, "data/CA_msa_parking_shares_0110.csv",row.names = FALSE)

###############predicting bundled parking
# names(data)
# table(data$unit_age)
# table(data$unit_age_f)
# logitmod <- glm(bundled_parking ~ central_city + NUNITS_v1 + unit_age + household_income,
#                 data = data, family = "binomial")
# summary(logitmod)

## some unit age values are the upper limit of the time interval, e.g. 31 is 27-31,51 is 42-51,
## recode and take the middle value of the interval
ahs_ca$unit_age_v1 <- dplyr::recode(ahs_ca$unit_age,`16`=14,`21`=19,`26`=24,`31`=29,`36`=34,`41`=39,
                             `51`=46,`61`=56,`71`=66,`81`=76,`91`=86)
table(ahs_ca$unit_age_v1)
table(ahs_ca$unit_age)

## recode unit age for ahs_west
ahs_west$unit_age_v1 <- dplyr::recode(ahs_west$unit_age,`16`=14,`21`=19,`26`=24,`31`=29,`36`=34,`41`=39,
                             `51`=46,`61`=56,`71`=66,`81`=76,`91`=86)
table(ahs_west$unit_age_v1)
table(ahs_west$unit_age)

## run regression with the recoded unit age variable
# logitmod_v1 <- glm(bundled_parking ~ central_city + NUNITS_v1 + unit_age_v1 + household_income,
#                    data = data, family = "binomial")
# summary(logitmod_v1)

#################improving the logistic model
## add MSAs to the model
# logitmod_v2 <- glm(bundled_parking ~ central_city + msa + NUNITS_v1 + unit_age_v1 + household_income,
#                    data = data, family = "binomial")
# summary(logitmod_v2)

############## other variables present in both CHTS and AHS
## household size
# persons_count in CHTS, PER in AHS
summary(ahs_ca$household_size)

summary(ahs_west$household_size)

## unit type
# residence_type in CHTS, structure type/NUNIT2 in AHS
summary(ahs_ca$structure_type)
str(ahs_ca$structure_type)

summary(ahs_west$structure_type)
str(ahs_west$structure_type)

## home ownership
# home_own in CHTS, TENURE in AHS
table(ahs_ca$home_own)

table(ahs_west$home_own)

# ## add household size, unit type, home_own to the model, and take out NUNITS_v1 and unit_age_v1
# logitmod_v3 <- glm(bundled_parking ~ central_city + msa + household_income + 
#                                      household_size + structure_type + home_own,
#                    data = ahs_ca, family = "binomial")
# summary(logitmod_v3)



## model for ahs_west non-MSA area
summary(ahs_west$msa)
table(ahs_west$SMSA)

# # take msa out of the model
# logitmod_v3_west <- glm(bundled_parking ~ central_city + household_income + 
#                                           household_size + structure_type + home_own,
#                         data = ahs_west[is.na(ahs_west$msa),], family = "binomial")
# summary(logitmod_v3_west)


## compare predictor variables in AHS and CHTS
chts_hh <- readRDS("data/data_household_0315.rds")

## household income; need to convert AHS household income to factor
levels(chts_hh$income_f)

ahs_ca$household_income_f <- NA
ahs_ca$household_income_f[ahs_ca$household_income<10000] <- "0-9,999"
ahs_ca$household_income_f[ahs_ca$household_income>=10000&ahs_ca$household_income<25000] <- "10,000-24,999"
ahs_ca$household_income_f[ahs_ca$household_income>=25000&ahs_ca$household_income<35000] <- "25,000-34,999"
ahs_ca$household_income_f[ahs_ca$household_income>=35000&ahs_ca$household_income<50000] <- "35,000-49,999"
ahs_ca$household_income_f[ahs_ca$household_income>=50000&ahs_ca$household_income<75000] <- "50,000-74,999"
ahs_ca$household_income_f[ahs_ca$household_income>=75000&ahs_ca$household_income<100000] <- "75,000-99,999"
ahs_ca$household_income_f[ahs_ca$household_income>=100000&ahs_ca$household_income<150000] <- "100,000-149,999"
ahs_ca$household_income_f[ahs_ca$household_income>=150000&ahs_ca$household_income<200000] <- "150,000-199,999"
ahs_ca$household_income_f[ahs_ca$household_income>=200000&ahs_ca$household_income<250000] <- "200,000-249,999"
ahs_ca$household_income_f[ahs_ca$household_income>=250000] <- "250,000+"

ahs_ca$household_income_f <- factor(ahs_ca$household_income_f, 
                                    levels = c("0-9,999","10,000-24,999","25,000-34,999",
                                               "35,000-49,999","50,000-74,999","75,000-99,999",
                                               "100,000-149,999","150,000-199,999","200,000-249,999",
                                               "250,000+"))
summary(ahs_ca$household_income_f)

## make household_income_f for ahs_west
ahs_west$household_income_f <- NA
ahs_west$household_income_f[ahs_west$household_income<10000] <- "0-9,999"
ahs_west$household_income_f[ahs_west$household_income>=10000&ahs_west$household_income<25000] <- "10,000-24,999"
ahs_west$household_income_f[ahs_west$household_income>=25000&ahs_west$household_income<35000] <- "25,000-34,999"
ahs_west$household_income_f[ahs_west$household_income>=35000&ahs_west$household_income<50000] <- "35,000-49,999"
ahs_west$household_income_f[ahs_west$household_income>=50000&ahs_west$household_income<75000] <- "50,000-74,999"
ahs_west$household_income_f[ahs_west$household_income>=75000&ahs_west$household_income<100000] <- "75,000-99,999"
ahs_west$household_income_f[ahs_west$household_income>=100000&ahs_west$household_income<150000] <- "100,000-149,999"
ahs_west$household_income_f[ahs_west$household_income>=150000&ahs_west$household_income<200000] <- "150,000-199,999"
ahs_west$household_income_f[ahs_west$household_income>=200000&ahs_west$household_income<250000] <- "200,000-249,999"
ahs_west$household_income_f[ahs_west$household_income>=250000] <- "250,000+"

ahs_west$household_income_f <- factor(ahs_west$household_income_f, 
                                    levels = c("0-9,999","10,000-24,999","25,000-34,999",
                                               "35,000-49,999","50,000-74,999","75,000-99,999",
                                               "100,000-149,999","150,000-199,999","200,000-249,999",
                                               "250,000+"))
summary(ahs_west$household_income_f)

## make a factor variable in CHTS for structure type
chts_hh$structure_type <- NA
chts_hh$structure_type[chts_hh$resty_dtchsglfmly==1] <- "detached single"
chts_hh$structure_type[chts_hh$resty_atchsglfmly==1] <- "attached single"
chts_hh$structure_type[chts_hh$resty_mltifmly==1] <- "2+ apartments"
chts_hh$structure_type[chts_hh$resty_mblhome==1] <- "mobile home"

chts_hh$structure_type <- factor(chts_hh$structure_type, 
                                 levels = c("detached single",
                                            "attached single",
                                            "2+ apartments",
                                            "mobile home"))

table(chts_hh$structure_type,useNA = "ifany")

## central city and MSA in CHTS
## Anaheim-Santa Ana, LA-Long Beach, Oakland, 
## Riverside-San Bernardino, Sacramento, San Diego, San Francisco, San Jose. 
## 0680: Bakersfield, CA
## 2840: Fresno, CA
## 5170: Modesto, CA
## 6000: Oxnard-Ventura, CA
## 7120: Salinas-Seaside-Monterey, CA
## 7480: Santa Barbara-Santa Maria, CA
## 7500: Santa Rosa-Petaluma, CA
## 8120: Stockton, CA
## 8720: Vallejo-Fairfield-Napa, CA
chts_hh$central_city <- 0
chts_hh$central_city[chts_hh$home_city %in% c("ANAHEIM","SANTA ANA","LOS ANGELES",
                                              "LONG BEACH","OAKLAND","RIVERSIDE",
                                              "SAN BERNARDINO","SACRAMENTO","SAN DIEGO",
                                              "SAN FRANCISCO","SAN JOSE","BAKERSFIELD",
                                              "FRESNO","MODESTO","OXNARD","VENTURA",
                                              "SALINAS","SEASIDE","MONTEREY",
                                              "SANTA BARBARA","SANTA MARIA",
                                              "SANTA ROSA","PETALUMA","STOCKTON",
                                              "VALLEJO","FAIRFIELD","NAPA")] <-1
chts_hh$central_city <- factor(chts_hh$central_city)
summary(chts_hh$central_city)

# sort(unique(chts_hh$home_city), decreasing = T)

## counties in MSA
sort(unique(chts_hh$home_county_id))
# Residential county:
# 06001Alameda, 06003Alpine, 06005Amador, 06007-Butte, 06009-Calaveras,
# 06011-Colusa, 06013Contra Costa, 06015Del Norte, 06017El Dorado, 06019Fresno, 06021-Glenn,
# 06023-Humboldt,06025-Imperial, 06027Inyo, 06029-Kern, 06031-Kings, 06033-Lake, 06035-Lassen,
# 06037-Los Angeles, 06039-Madera, 06041-Marin, 06043-Mariposa, 06045-Mendocino, 06047-Merced, 
# 06049-Modoc, 06051Mono, 06053-Monterey, 06055-Napa, 06057-Nevada, 06059Orange, 06061-Placer, 
# 06063-Plumas, 06065-Riverside, 06067Sacramento, 06069-San Benito, 06071-San Bernardino, 
# 06073San Diego, 06075-San Francisco, 06077-San Joaquin, 06079-San Luis Obispo, 06081-San Mateo,
# 06083-Santa Barbara, 06085Santa Clara, 06087-Santa Cruz, 06089-Shasta, 06091-Sierra, 06093-Siskiyou,
# 06095-Solano, 06097-Sonoma, 06099-Stanislaus, 06101-Sutter, 06103-Tehama, 06105-Trinity, 06107-Tulare, 
# 06109-Tuolumne, 06111-Ventura, 06113-Yolo, 06115-Yuba

## county key dataframe
key_numbers <- seq(1,115,2)
key_names <- c("Alameda","Alpine","Amador","Butte","Calaveras","Colusa","Contra Costa","Del Norte",
               "El Dorado","Fresno","Glenn","Humboldt","Imperial","Inyo","Kern","Kings","Lake",
               "Lassen","Los Angeles","Madera","Marin","Mariposa","Mendocino","Merced","Modoc",
               "Mono","Monterey","Napa","Nevada","Orange","Placer","Plumas","Riverside","Sacramento",
               "San Benito","San Bernardino","San Diego","San Francisco","San Joaquin","San Luis Obispo",
               "San Mateo","Santa Barbara","Santa Clara","Santa Cruz","Shasta","Sierra","Siskiyou",
               "Solano","Sonoma","Stanislaus","Sutter","Tehama","Trinity","Tulare","Tuolumne",
               "Ventura","Yolo","Yuba")
county_key <- data.frame(county_id=key_numbers,
                         county_name=key_names)

## add county names to chts data
chts_hh <- merge(chts_hh,county_key,by.x = "home_county_id",
                 by.y = "county_id", all.x = TRUE)
names(chts_hh)

## assign PMSA based on county name
## Anaheim-Santa Ana, LA-Long Beach, Oakland, 
## Riverside-San Bernardino, Sacramento, San Diego, San Francisco, San Jose.
## all MSAs in updated ahs_ca: 
# "Anaheim-Santa Ana","Bakersfield","Fresno","LA-Long Beach","Modesto","Oakland",
# "Oxnard-Ventura","Riverside-SB","Sacramento","Salinas-Seaside-Monterey","San Diego",
# "San Francisco","San Jose","Santa Barbara-Santa Maria","Santa Rosa-Petaluma",
# "Stockton","Vallejo-Fairfield-Napa"   
# https://www.labormarketinfo.edd.ca.gov/LMID/Metropolitan_Area_Definitions.html
# library(dplyr)
chts_hh$msa <- dplyr::recode(chts_hh$county_name, 
                      "Orange"="Anaheim-Santa Ana",
                      "Kern"="Bakersfield",
                      "Fresno"="Fresno",
                      "Madera"="Fresno",
                      "Los Angeles"="LA-Long Beach",
                      "Stanislaus"="Modesto",
                      "Alameda"="Oakland",
                      "Contra Costa"="Oakland",
                      "Ventura"="Oxnard-Ventura",
                      "Riverside"="Riverside-SB",
                      "San Bernardino"="Riverside-SB",
                      "Sacramento"="Sacramento",
                      "Placer"="Sacramento",
                      "El Dorado"="Sacramento",
                      "Monterey"="Salinas-Seaside-Monterey",
                      "San Diego"="San Diego",
                      "San Francisco"="San Francisco",
                      "San Mateo"="San Francisco",
                      "Marin"="San Francisco",
                      "Santa Clara"="San Jose",
                      "Santa Barbara"="Santa Barbara-Santa Maria",
                      "Sonoma"="Santa Rosa-Petaluma",
                      "San Joaquin"="Stockton",
                      "Napa"="Vallejo-Fairfield-Napa",
                      "Solano"="Vallejo-Fairfield-Napa",
                      .default=NA_character_)
table(chts_hh$msa)

## renaming variables for the logistic model
## vars in ahs data: central_city + msa + household_income_f + 
## household_size + structure_type + home_own
# chts_hh$central_city
# chts_hh$msa
colnames(chts_hh)[which(names(chts_hh)=="income_f")] <- "household_income_f"
# chts_hh$household_income_f
colnames(chts_hh)[which(names(chts_hh)=="persons_count")] <- "household_size"
# chts_hh$household_size
# chts_hh$structure_type
colnames(chts_hh)[which(names(chts_hh)=="homeowner")] <- "home_own"
# chts_hh$home_own

############## final model for ahs_ca, using household_income_f to match CHTS data
logitmod_v4 <- glm(bundled_parking ~ central_city + msa + household_income_f + 
                                     household_size + structure_type + home_own,
                   data = ahs_ca, family = "binomial")
summary(logitmod_v4)

# summary(logitmod_v4$fitted.values)
# prop.table(table(logitmod_v4$y))
# quantile(logitmod_v4$fitted.values,0.05190682)
# # #cutoff at 0.7390365
# length(logitmod_v4$fitted.values)
# length(logitmod_v4$fitted.values[logitmod_v4$fitted.values>0.7390365])
# length(logitmod_v4$fitted.values[logitmod_v4$fitted.values<=0.7390365])
# 1437/27087
# 25650/27087

##### use confusion matrix to determine cutoff value 
# that yield roughly equal false positive and false negatives
bdpking_raw <- as.factor(logitmod_v4$y)

bdpking_pd <- logitmod_v4$fitted.values %>%
  as.data.frame() %>%
  mutate(bdpking_pd = if_else(. >0.945, 1,0)) 

bdpking_pd_f <- as.factor(bdpking_pd$bdpking_pd)

library(caret)

confusionMatrix(data = bdpking_pd_f,reference = bdpking_raw)

## predict bundled parking probability for CHTS using model based on ahs_ca MSA data
chts_hh$bundled_parking_prob <- predict(logitmod_v4,
                                        newdata = chts_hh, 
                                        type = "response")
hist(chts_hh$bundled_parking_prob)
summary(chts_hh$bundled_parking_prob)

########final model for ahs_west non-MSA area, using household_income_f and taking out msa
logitmod_v4_west <- glm(bundled_parking ~ central_city + household_income_f + 
                                          household_size + structure_type + home_own,
                        data = ahs_west[is.na(ahs_west$msa),], family = "binomial")
summary(logitmod_v4_west)

bdpking_west_raw <- as.factor(logitmod_v4_west$y)

bdpking_west_pd <- logitmod_v4_west$fitted.values %>%
  as.data.frame() %>%
  mutate(bdpking_pd = if_else(. >0.978, 1,0))

bdpking_west_pd_f <- as.factor(bdpking_west_pd$bdpking_pd)
####confusion matrix

confusionMatrix(data = bdpking_west_pd_f,reference = bdpking_west_raw)

# summary(logitmod_v4_west$fitted.values)
# prop.table(table(logitmod_v4_west$y))
# quantile(logitmod_v4_west$fitted.values,0.02196653)
# #cutoff at 0.8981331
# length(logitmod_v4_west$fitted.values)
# length(logitmod_v4_west$fitted.values[logitmod_v4_west$fitted.values>0.8981331])
# length(logitmod_v4_west$fitted.values[logitmod_v4_west$fitted.values<=0.8981331])
# 101/4780
# 4679/4780
# 
# prop.table(table(ahs_west$bundled_parking[is.na(ahs_west$msa)]))#virtually all had bundled parking

## predict bundled parking probability for CHTS households outside the 17 MSAs 
## using model based on ahs_west non MSA data
chts_hh$bundled_parking_prob[is.na(chts_hh$msa)] <- 
  predict(logitmod_v4_west,
          newdata = chts_hh[is.na(chts_hh$msa),],
          type = "response")

# table(chts_hh$msa, useNA = "ifany")
hist(chts_hh$bundled_parking_prob)
summary(chts_hh$bundled_parking_prob)

#create bundled_parking dummy, using 0.85 as the break
chts_hh$bundled_parking <- NA
chts_hh$bundled_parking[chts_hh$bundled_parking_prob>0.85] <- 1
chts_hh$bundled_parking[chts_hh$bundled_parking_prob<=0.85] <- 0
table(chts_hh$bundled_parking, useNA = "ifany")

#create bundled_parking dummy
# proportions of bundled parking in ash_ca data (used in prediction model)
prop.table(table(ahs_ca$bundled_parking))
prop.table(table(logitmod_v4$y))
# 
# proportions of bundled parking in ash_west data (used in prediction model)
prop.table(table(ahs_west$bundled_parking[is.na(ahs_west$msa)]))
prop.table(table(logitmod_v4_west$y))

# percentile values corresponding to original proportions, to be used as cutoff values
quantile(chts_hh$bundled_parking_prob[!is.na(chts_hh$msa)],0.05190682, na.rm = T)
quantile(chts_hh$bundled_parking_prob[is.na(chts_hh$msa)],0.02196653, na.rm = T)

# using above cutoff values to make dummy
chts_hh$bundled_parking_v2 <- NA
chts_hh$bundled_parking_v2[!is.na(chts_hh$msa)&chts_hh$bundled_parking_prob>=0.945]<-1
chts_hh$bundled_parking_v2[!is.na(chts_hh$msa)&chts_hh$bundled_parking_prob<0.945]<-0
chts_hh$bundled_parking_v2[is.na(chts_hh$msa)&chts_hh$bundled_parking_prob>=0.978]<-1
chts_hh$bundled_parking_v2[is.na(chts_hh$msa)&chts_hh$bundled_parking_prob<0.978]<-0

# chts_hh$bundled_parking_v3 <- NA
# chts_hh$bundled_parking_v3[!is.na(chts_hh$msa)&chts_hh$bundled_parking_prob>=0.867]<-1
# chts_hh$bundled_parking_v3[!is.na(chts_hh$msa)&chts_hh$bundled_parking_prob<0.867]<-0
# chts_hh$bundled_parking_v3[is.na(chts_hh$msa)&chts_hh$bundled_parking_prob>=0.945]<-1
# chts_hh$bundled_parking_v3[is.na(chts_hh$msa)&chts_hh$bundled_parking_prob<0.945]<-0

# prop.table(table(chts_hh$bundled_parking_v2[is.na(chts_hh$msa)]))
# prop.table(table(ahs_west$bundled_parking[is.na(ahs_west$msa)]))
# 
# prop.table(table(chts_hh$bundled_parking_v2[!is.na(chts_hh$msa)]))
# prop.table(table(ahs_ca$bundled_parking))

table(chts_hh$bundled_parking_v2)
# table(chts_hh$bundled_parking_v3)

table(chts_hh$msa, useNA = "ifany")
saveRDS(chts_hh, "data/data_household_w_bundled_parking_0315.rds")
