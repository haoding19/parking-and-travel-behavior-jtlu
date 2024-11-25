#####RIMI paper regressions 12/19



## packages
library(dplyr)
library(tidyverse)
# library(gmodels)
library(MASS)
library(pscl)
library(ggeffects)
library(margins)
# library(marginaleffects)
# library(QuantPsyc)
# library(VGAM)
library(censReg)
# library(mhurdle)
# mhurdle::margins()
# library(stargazer)
library(oddsratio)

## data sets: household, trip, person
# hh data with bundled parking variable exlcuding income
dt_hh <- readRDS("data/data_household_merged_1220.rds")
dt_trip <- readRDS("data/data_place_trip_1220.rds")
dt_psn <- readRDS("data/data_person_merged_1220.rds")

#################################################### Individual/person level independent variables
########## make hh adult count variable
dt_hh$adult_count <- dt_hh$household_size - dt_hh$child_count

########## merge household level variables to individuals
# parking, neighborhood type, vehicle counts, adults and child,HH income, home ownership,
# access to broadband, type of housing unit
dt_psn <- merge(dt_psn,dt_hh[,c("sampno","bundled_parking","bundled_parking_v2","bundled_parking_v3",
                                "bundled_parking_prob","ntype","vehicle_count","vehicle_op_count",
                                "adult_count","has_child","household_income_f","home_own",
                                "share_tot_popn_broadband","share_wking_age_broadband",
                                "share_tot_popn_no_computer","structure_type")],
                by="sampno", all.x = TRUE)


########### rename ntype variables and make factor
colnames(dt_psn)[which(colnames(dt_psn)=="ntype.x")] <- "work_ntype"
colnames(dt_psn)[which(colnames(dt_psn)=="ntype.y")] <- "home_ntype"

dt_psn$home_ntype_f <- factor(dt_psn$home_ntype,
                              levels = c("Mixed-use","Old urban","Urban residential","Established suburb",
                                         "Patchwork","New development","Rural"))
dt_psn$work_ntype_f <- factor(dt_psn$work_ntype,
                              levels = c("Mixed-use","Old urban","Urban residential","Established suburb",
                                         "Patchwork","New development","Rural"))

########### free work parking, made from trip level parking data
# summary(dt_trip$park_job_free)
# library(dplyr)
psn_work_pking <- dt_trip %>% 
  subset(!is.na(park_job_free)) %>%
  group_by(sampno,perno) %>%
  summarise(free_work_pking = max(park_job_free))

dt_psn <- merge(dt_psn,psn_work_pking,by=c("sampno","perno"), all.x = TRUE)

rm(psn_work_pking)

########### attach labels to employment occupation
# table(dt_psn$empl_occupation, useNA = "ifany")

empl_occu_key_no <- seq(11,55,2)
empl_occu_key_name <- c("management","business/financial","computer/math","architecture/engineering",
                        "science","community/social services","legal","education","arts/entertainment",
                        "healthcare practitioners","healthcare support","protective service",
                        "food prep./serving","cleaning/grounds keeping","personal care","sales",
                        "office/administrative","farm./fish./forestry","construction",
                        "install./maint./repair","production","transportation","military")
empl_occu_key <- data.frame(empl_occu_key_no,empl_occu_key_name)

dt_psn <- merge(dt_psn, empl_occu_key,by.x = "empl_occupation",by.y = "empl_occu_key_no", all.x = T)

colnames(dt_psn)[which(colnames(dt_psn)=="empl_occu_key_name")] <- "occupation"

# table(dt_psn$occupation, useNA = "ifany")
rm(empl_occu_key,empl_occu_key_name,empl_occu_key_no)

########### employment and home county

# table(dt_psn$empl_county_id)

## county key dataframe
key_numbers <- seq(1,115,2)
key_names <- c("Alameda","Alpine","Amador","Butte","Calaveras","Colusa","Contra Costa","Del Norte",
               "El Dorado","Fresno","Glenn","Humboldt","Imperial","Inyo","Kern","Kings","Lake","Lassen",
               "Los Angeles","Madera","Marin","Mariposa","Mendocino","Merced","Modoc","Mono","Monterey",
               "Napa","Nevada","Orange","Placer","Plumas","Riverside","Sacramento","San Benito",
               "San Bernardino","San Diego","San Francisco","San Joaquin","San Luis Obispo","San Mateo",
               "Santa Barbara","Santa Clara","Santa Cruz","Shasta","Sierra","Siskiyou","Solano","Sonoma",
               "Stanislaus","Sutter","Tehama","Trinity","Tulare","Tuolumne","Ventura","Yolo","Yuba")
county_key <- data.frame(county_id=key_numbers,county_name=key_names)

## employment county names
dt_psn <- merge(dt_psn,county_key, by.x = "empl_county_id", by.y = "county_id", all.x = TRUE)

## home county names
dt_psn <- merge(dt_psn,dt_hh[,c("sampno","county_name")], by = "sampno", all.x = TRUE)
# names(dt_psn)

## rename merged variables
colnames(dt_psn)[which(colnames(dt_psn)=="county_name.x")] <- "work_county"
colnames(dt_psn)[which(colnames(dt_psn)=="county_name.y")] <- "home_county"

rm(county_key,key_names,key_numbers)

############################################################## Household level independent variables
########### making factor variable for home neighborhood type
dt_hh$home_ntype_f <- factor(dt_hh$ntype, 
                             levels = c("Mixed-use","Old urban","Urban residential","Established suburb",
                                        "Patchwork","New development","Rural"))
# summary(dt_hh$ntype_f)

########### compute hh shares for individual level control variables
hh_ind_vars <- dt_psn %>%
  group_by(sampno) %>%
  summarise_at(vars(white,asian,black,latino,race_other_d,educ_BAorhigher,educ_lessthanHS,employed,
                    over65,forgnborn,disabled_d,male,free_work_pking,wloc_home,wloc_fixed,wday_flexible
                    # ,chained_commute
                    ), mean, na.rm=T)

dt_hh <- merge(dt_hh,hh_ind_vars,by="sampno",all.x = T)
rm(hh_ind_vars)

########### Household models control for number of licensed drivers
# table(dt_psn$driver_license_d, useNA = "ifany")
# str(dt_psn$driver_license_d)

## sum licensed drivers for each HH
HH_licensed_drivers <- dt_psn %>%
  group_by(sampno) %>%
  summarise(licensed_driver_count = sum(driver_license_d, na.rm = T))

# table(HH_licensed_drivers$licensed_driver_count, useNA = "ifany")

## merge with dt_hh
dt_hh <- merge(dt_hh, HH_licensed_drivers, by = "sampno")
# table(dt_hh$licensed_driver_count, useNA = "ifany")
# table(dt_hh$household_size, useNA = "ifany")
rm(HH_licensed_drivers)

## HH licensed drivers > HH size = NA
dt_hh$licensed_driver_count[dt_hh$licensed_driver_count > dt_hh$household_size] <- NA

################################################################### Dependent variables
############ Regression 2: invididual-level tot transit trips and share of trips that are transit trips
# count transit trips at individual level
psn_trips <- dt_trip %>%
  group_by(sampno, perno) %>%
  summarise(tot_trips_transit = sum(mode_v1=="transit", na.rm = T),
            tot_trips = sum(!is.na(tripno))) %>%
  mutate(transit_trips_frac=tot_trips_transit/tot_trips)

# merge to dt_psn
dt_psn <- merge(dt_psn, psn_trips, by=c("sampno","perno"), all.x = T)
rm(psn_trips)

############ Regression 3: using transit for commute
###### individual level, dummy for using transit for commute 
# table(dt_psn$commute_mode_v1, useNA = "ifany")
## make commute_transit dummy
dt_psn$commute_transit <- factor(dt_psn$commute_mode_v1,
                                 levels = c("drive","other","rides","taxi/ridehail","transit","walk/bike"),
                                 labels = c(0,0,0,0,1,0))
# table(dt_psn$commute_transit,useNA = "ifany")
# table(as.numeric(dt_psn$commute_transit), useNA = "ifany")
## change to numeric
dt_psn$commute_transit_n <- as.numeric(dt_psn$commute_transit) %>%
                                dplyr::recode(.,`2`=1,`1`=0)
# table(dt_psn$commute_transit_n)

###### HH level, share of hh that use transit for commute
hh_share_transit_commute <- dt_psn %>%
  group_by(sampno) %>%
  summarise(hh_share_transit_commute = mean(commute_transit_n, na.rm=T))

dt_hh <- merge(dt_hh, hh_share_transit_commute, by="sampno", all.x = T)
rm(hh_share_transit_commute)

############ Regression 4: driving to commute 
###### individual level, dummy for driving to commute
dt_psn$commute_driving <- factor(dt_psn$commute_mode_v1,
                                 levels = c("drive","other","rides","taxi/ridehail","transit","walk/bike"),
                                 labels = c(1,0,0,0,0,0))
# table(dt_psn$commute_mode_v1, useNA = "ifany")
# table(dt_psn$commute_driving, useNA = "ifany")
# table(dt_psn$commute_driving_n)
## change to numeric
dt_psn$commute_driving_n <- as.numeric(dt_psn$commute_driving) %>%
                                dplyr::recode(.,`1`=1,`2`=0)

####### HH level, share of hh driving commute
hh_share_driving_commute <- dt_psn %>%
  group_by(sampno) %>%
  summarise(hh_share_driving_commute = mean(commute_driving_n, na.rm=T))

dt_hh <- merge(dt_hh, hh_share_driving_commute, by="sampno", all.x = T)
rm(hh_share_driving_commute)

##################################################################### Regressions

########### Regression 1: vehicle ownership and parking
#### V1, bundled_parking_v2 + household_income_f;
#### V2, HH income only;
#### V3, bundled_parking_v3 (excluding income) + household_income_f.

m1_hhveh_glmnb <- glm.nb(vehicle_count ~ 
                          # bundled_parking_v2 # with HH income
                          bundled_parking_v3 # without HH income
                        + licensed_driver_count + household_income_f
                        # Controls
                        + home_ntype_f + asian + black + latino + race_other_d + male + over65
                        + forgnborn + disabled_d + employed + educ_BAorhigher + educ_lessthanHS
                        + county_name, data = dt_hh)
summary(m1_hhveh_glmnb)
## N
length(m1_hhveh_glmnb$fitted.values)

## average marginal effects
ame_m1 <- summary(margins(m1_hhveh_glmnb,variables =
                            c("bundled_parking_v3","licensed_driver_count","household_income_f")))
ame_m1[c(1,11,2,6,8:10,3:5,7),]

########### Regression 2: transit trips and parking

###### individual level transit trips model
m2_transit_trips_psn_glmnb <- glm.nb(tot_trips_transit ~
                                       # bundled_parking_v2
                                       bundled_parking_v3
                                     + vehicle_count + employed 
                                     # Controls  
                                     + household_income_f + home_ntype_f  + race_f + male + over65 
                                     + forgnborn + disabled_d + educ_BAorhigher + educ_lessthanHS
                                     + home_county, data=dt_psn)
summary(m2_transit_trips_psn_glmnb)
length(m2_transit_trips_psn_glmnb$fitted.values)

## average marginal effects
ame_m2_psn <- summary(margins(m2_transit_trips_psn_glmnb,
                              variables = c("bundled_parking_v3","vehicle_count")))
ame_m2_psn

###### household level transit trips model
m2_transit_trips_hh_glmnb <- glm.nb(tot_trips_transit~
                                      # bundled_parking_v2
                                      bundled_parking_v3
                                    # + vehicle_count + licensed_driver_count 
                                    + employed 
                                    # Controls
                                    + household_income_f + home_ntype_f + asian + black + latino 
                                    + race_other_d + male + over65 + forgnborn + disabled_d
                                    + educ_BAorhigher + educ_lessthanHS + county_name, data = dt_hh)
summary(m2_transit_trips_hh_glmnb)
length(m2_transit_trips_hh_glmnb$fitted.values)

## average marginal effects
ame_m2_hh <- summary(margins(m2_transit_trips_hh_glmnb,
                             variables =c("bundled_parking_v3","vehicle_count","licensed_driver_count")))
ame_m2_hh

########### Regression 3: using transit for commute and parking
####### individual level transit commute model
m3_commute_transit_psn_glmb <- glm(commute_transit_n ~
                                     # bundled_parking_v2
                                     bundled_parking_v3
                                   + free_work_pking + vehicle_count + chained_commute + distance_work
                                   # Controls
                                   + household_income_f + home_ntype_f + work_ntype_f + race_f + male 
                                   + over65 + forgnborn + disabled_d + educ_BAorhigher + educ_lessthanHS
                                   + home_county, data=dt_psn, family = "binomial")
summary(m3_commute_transit_psn_glmb)
## N
length(m3_commute_transit_psn_glmb$fitted.values)
## odds ratios
exp(m3_commute_transit_psn_glmb$coefficients)


####### household level transit commute model
m3_commute_transit_hh_glmb <- glm(hh_share_transit_commute~
                                    # bundled_parking_v2
                                    bundled_parking_v3
                                  + free_work_pking + vehicle_count + chained_commute 
                                  + licensed_driver_count
                                  # Controls
                                  + household_income_f + home_ntype_f + asian + black + latino
                                  + race_other_d + male + over65 + forgnborn + disabled_d
                                  + educ_BAorhigher + educ_lessthanHS + county_name,
                                  data = dt_hh, family = "binomial")
summary(m3_commute_transit_hh_glmb)
## N
length(m3_commute_transit_hh_glmb$fitted.values)
## odds ratios
exp(m3_commute_transit_hh_glmb$coefficients)

########### Regression 4: driving for commute and parking
####### individual level driving commute model
m4_commute_driving_psn_glmb <- glm(commute_driving_n ~
                                     # bundled_parking_v2
                                     bundled_parking_v3
                                   + free_work_pking + vehicle_count + chained_commute + distance_work
                                   # Controls
                                   + household_income_f + home_ntype_f + work_ntype_f + race_f + male
                                   + over65 + forgnborn + disabled_d + educ_BAorhigher + educ_lessthanHS
                                   + home_county, data=dt_psn, family = "binomial")
summary(m4_commute_driving_psn_glmb)
## N
length(m4_commute_driving_psn_glmb$fitted.values)
## odds ratios
exp(m4_commute_driving_psn_glmb$coefficients)


####### HH level driving commute model
m4_commute_driving_hh_glmb <- glm(hh_share_driving_commute~
                                    # bundled_parking_v2
                                    bundled_parking_v3
                                  + free_work_pking + vehicle_count + chained_commute 
                                  + licensed_driver_count
                                  # Controls
                                  + household_income_f + home_ntype_f + asian + black + latino
                                  + race_other_d + male + over65 + forgnborn + disabled_d
                                  + educ_BAorhigher + educ_lessthanHS + county_name,
                                  data = dt_hh, family = "binomial")
summary(m4_commute_driving_hh_glmb)
## N
length(m4_commute_driving_hh_glmb$fitted.values)
## odds ratios
exp(m4_commute_driving_hh_glmb$coefficients)


########### Regression 5: VMT/car distance and parking
####### individual level VMT/car distance model without WFH variable
m5_VMT_psn_tobit <- censReg(tot_dis_car ~ 
                              # bundled_parking_v2
                              bundled_parking_v3
                            + vehicle_count + employed
                            # Controls
                            + household_income_f + home_ntype_f + race_f + male + over65 + forgnborn 
                            + disabled_d + educ_BAorhigher + educ_lessthanHS + home_county,
                            left = 0, data=dt_psn)
summary(m5_VMT_psn_tobit)

##### average marginal effects (manual calculation)

#### average marginal effect of bundled_parking_v3
### model coefficients as a matrix
# length(coefficients(m5_VMT_psn_tobit))
b <- matrix(coefficients(m5_VMT_psn_tobit)[1:86], 86)
### model matrix
x <- model.matrix(m5_VMT_psn_tobit)
### change all values of bundled_parking_v3 to 0
x[,2] <- 0
### model predictions when bundled_parking_v3 == 0
m5_VMT_psn_tobit$predict0 <- x%*%b
### check for negative predicted values
sort(m5_VMT_psn_tobit$predict0)
m5_VMT_psn_tobit$predict0[m5_VMT_psn_tobit$predict0<0] <- 0

### new model matrix,change all values of bundled_parking_v3 to 1
x1 <- x
x1[,2] <- 1
### model predictions when bundled_parking_v3 == 1
m5_VMT_psn_tobit$predict1 <- x1%*%b
### check for negative predicted values
sort(m5_VMT_psn_tobit$predict1)
m5_VMT_psn_tobit$predict1[m5_VMT_psn_tobit$predict1<0] <- 0

### average marginal effect = mean of all marginal effects
ame_m5_psn_bdp <- mean(m5_VMT_psn_tobit$predict1-m5_VMT_psn_tobit$predict0)
ame_m5_psn_bdp

#### average marginal effect of vehicle count
### model coefficients as a matrix
# length(coefficients(m3_VMT_psn_tobit))
b <- matrix(coefficients(m5_VMT_psn_tobit)[1:86], 86)
### model matrix
x <- model.matrix(m5_VMT_psn_tobit)
### model predictions
m5_VMT_psn_tobit$predict0 <- x%*%b
### check for negative predicted values
sort(m5_VMT_psn_tobit$predict0)
m5_VMT_psn_tobit$predict0[m5_VMT_psn_tobit$predict0<0] <- 0

### new model matrix, add 1 unit to vehicle count
x1 <- x
x1[,3] <- x[,3] + 1
### model predictions when vehicle count increase by one unit
m5_VMT_psn_tobit$predict1 <- x1%*%b
### check for negative predicted values
sort(m5_VMT_psn_tobit$predict1)
m5_VMT_psn_tobit$predict1[m5_VMT_psn_tobit$predict1<0] <- 0

### average marginal effect = mean of all marginal effects
ame_m5_psn_vehct <- mean(m5_VMT_psn_tobit$predict1-m5_VMT_psn_tobit$predict0)
ame_m5_psn_vehct


####### individual level VMT/car distance model WITH WFH variable
m5_VMT_psn_tobit_wfh <- censReg(tot_dis_car ~ 
                                  # bundled_parking_v2
                                  bundled_parking_v3
                                + wloc_home + vehicle_count # + employed 
                                # Controls
                                + household_income_f + home_ntype_f + race_f + male + over65 + forgnborn 
                                + disabled_d + educ_BAorhigher + educ_lessthanHS + home_county,
                                left = 0, data=dt_psn)
summary(m5_VMT_psn_tobit_wfh)


####### HH level VMT/car distance model without WFH variable
m5_VMT_hh_tobit <- censReg(tot_dis_car~
                             # bundled_parking_v2
                             bundled_parking_v3
                           + vehicle_count + licensed_driver_count + employed 
                           # Controls
                           + household_income_f + home_ntype_f + asian + black + latino + race_other_d
                           + male + over65 + forgnborn + disabled_d + educ_BAorhigher + educ_lessthanHS
                           + county_name, left = 0, data = dt_hh)
summary(m5_VMT_hh_tobit)

##### average marginal effects (manual calculation)

#### average marginal effect of bundled_parking_v3
### model coefficients as a matrix
length(coefficients(m5_VMT_hh_tobit))
b <- matrix(coefficients(m5_VMT_hh_tobit)[1:87], 87)
### model matrix
x <- model.matrix(m5_VMT_hh_tobit)
### change all values of bundled_parking_v3 to 0
x[,2] <- 0
### model predictions when bundled_parking_v3 == 0
m5_VMT_hh_tobit$predict0 <- x%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit$predict0)
m5_VMT_hh_tobit$predict0[m5_VMT_hh_tobit$predict0<0] <- 0

### new model matrix,change all values of bundled_parking_v3 to 1
x1 <- x
x1[,2] <- 1
### model predictions when bundled_parking_v3 == 1
m5_VMT_hh_tobit$predict1 <- x1%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit$predict1)
m5_VMT_hh_tobit$predict1[m5_VMT_hh_tobit$predict1<0] <- 0

### average marginal effect = mean of all marginal effects
ame_m5_hh_bdp <- mean(m5_VMT_hh_tobit$predict1-m5_VMT_hh_tobit$predict0)
ame_m5_hh_bdp

#### average marginal effect of vehicle count
### model coefficients as a matrix
# length(coefficients(m5_VMT_hh_tobit))
b <- matrix(coefficients(m5_VMT_hh_tobit)[1:87], 87)
### model matrix
x <- model.matrix(m5_VMT_hh_tobit)
### model predictions
m5_VMT_hh_tobit$predict0 <- x%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit$predict0)
m5_VMT_hh_tobit$predict0[m5_VMT_hh_tobit$predict0<0] <- 0

### new model matrix, add 1 unit to vehicle count
x1 <- x
x1[,3] <- x[,3] + 1
### model predictions when vehicle count increase by one unit
m5_VMT_hh_tobit$predict1 <- x1%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit$predict1)
m5_VMT_hh_tobit$predict1[m5_VMT_hh_tobit$predict1<0] <- 0

### average marginal effect = mean of all marginal effects
ame_m5_hh_vehct <- mean(m5_VMT_hh_tobit$predict1-m5_VMT_hh_tobit$predict0)
ame_m5_hh_vehct

####### HH level VMT/car distance model WITH WFH variable
m5_VMT_hh_tobit_wfh <- censReg(tot_dis_car~
                                 # bundled_parking_v2
                                 bundled_parking_v3
                               + wloc_home + vehicle_count + licensed_driver_count # + employed 
                               # Controls
                               + household_income_f + home_ntype_f + asian + black + latino + race_other_d
                               + male + over65 + forgnborn + disabled_d + educ_BAorhigher + educ_lessthanHS
                               + county_name, left = 0, data = dt_hh)
summary(m5_VMT_hh_tobit_wfh)

##### average marginal effects (manual calculation)

#### average marginal effect of bundled_parking_v3
### model coefficients as a matrix
length(coefficients(m5_VMT_hh_tobit_wfh))
b <- matrix(coefficients(m5_VMT_hh_tobit_wfh)[1:87], 87)
### model matrix
x <- model.matrix(m5_VMT_hh_tobit_wfh)
### change all values of bundled_parking_v3 to 0
x[,2] <- 0
### model predictions when bundled_parking_v3 == 0
m5_VMT_hh_tobit_wfh$predict0 <- x%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit_wfh$predict0)
m5_VMT_hh_tobit_wfh$predict0[m5_VMT_hh_tobit_wfh$predict0<0] <- 0

### new model matrix,change all values of bundled_parking_v3 to 1
x1 <- x
x1[,2] <- 1
### model predictions when bundled_parking_v3 == 1
m5_VMT_hh_tobit_wfh$predict1 <- x1%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit_wfh$predict1)
m5_VMT_hh_tobit_wfh$predict1[m5_VMT_hh_tobit_wfh$predict1<0] <- 0

### average marginal effect = mean of all marginal effects
ame_m5_hh_wfh_bdp <- mean(m5_VMT_hh_tobit_wfh$predict1-m5_VMT_hh_tobit_wfh$predict0)
ame_m5_hh_wfh_bdp

#### average marginal effect of vehicle count
### model coefficients as a matrix
length(coefficients(m5_VMT_hh_tobit_wfh))
b <- matrix(coefficients(m5_VMT_hh_tobit_wfh)[1:87], 87)
### model matrix
x <- model.matrix(m5_VMT_hh_tobit_wfh)
### model predictions
m5_VMT_hh_tobit_wfh$predict0 <- x%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit_wfh$predict0)
m5_VMT_hh_tobit_wfh$predict0[m5_VMT_hh_tobit_wfh$predict0<0] <- 0

### new model matrix, add 1 unit to vehicle count
x1 <- x
x1[,4] <- x[,4] + 1
### model predictions when vehicle count increase by one unit
m5_VMT_hh_tobit_wfh$predict1 <- x1%*%b
### check for negative predicted values
sort(m5_VMT_hh_tobit_wfh$predict1)
m5_VMT_hh_tobit_wfh$predict1[m5_VMT_hh_tobit_wfh$predict1<0] <- 0

### average marginal effect = mean of all marginal effects
ame_m5_hh_wfh_vehct <- mean(m5_VMT_hh_tobit_wfh$predict1-m5_VMT_hh_tobit_wfh$predict0)
ame_m5_hh_wfh_vehct

########### Regression 6: total car trips and parking
####### individual level car trips model
#### V1: without wloc_home but with employed
#### V2: with wloc_home but without employed
m6_cartrips_psn_glmnb = glm.nb(tot_trips_car ~ 
                                 # bundled_parking_v2
                                 bundled_parking_v3
                               # + wloc_home
                               # + wloc_fixed
                               + vehicle_count 
                               + employed
                               # Controls
                               + household_income_f + home_ntype_f + race_f + male + over65 + forgnborn
                               + disabled_d + educ_BAorhigher + educ_lessthanHS + home_county, data=dt_psn)
summary(m6_cartrips_psn_glmnb)
# N
length(m6_cartrips_psn_glmnb$fitted.values)
## Average marginal effects
ame_m6_psn <- summary(margins(m6_cartrips_psn_glmnb,
                              variables = c("bundled_parking_v3","vehicle_count")))
ame_m6_psn

###### add a control for individuals having secondary jobs

## get job count from original data
data_psn <- read.csv("CHTS data/caltrans_full_survey/survey_person.csv")
table(data_psn$job_count)

dt_psn <- merge(dt_psn,data_psn[,c("sampno","perno","job_count")], by=c("sampno", "perno"), all.x = T )
table(dt_psn$job_count)

## a dummy for secondary jobs based on job count
dt_psn$sec_jobs <- NA
dt_psn$sec_jobs[dt_psn$job_count==1] <-0
dt_psn$sec_jobs[dt_psn$job_count==2 | dt_psn$job_count==3] <- 1
table(dt_psn$sec_jobs)

## run model with sec_jobs
m6_cartrips_psn_glmnb_v1 = glm.nb(tot_trips_car ~ 
                                 # bundled_parking_v2
                                 bundled_parking_v3
                               + wloc_home
                               + vehicle_count 
                               + sec_jobs
                               # + employed
                               # Controls
                               + household_income_f + home_ntype_f + race_f + male + over65 + forgnborn
                               + disabled_d + educ_BAorhigher + educ_lessthanHS + home_county, data=dt_psn)
summary(m6_cartrips_psn_glmnb_v1)


####### HH level car trips model
#### V1: without wloc_home but with employed
#### V2: with wloc_home but without employed
m6_cartrips_hh_glmnb <- glm.nb(tot_trips_car~
                                 # bundled_parking_v2
                                 bundled_parking_v3
                               + wloc_home
                               + vehicle_count
                               + licensed_driver_count
                               # + employed
                               # Controls
                               + household_income_f + home_ntype_f + asian + black + latino + race_other_d
                               + male + over65 + forgnborn + disabled_d + educ_BAorhigher + educ_lessthanHS
                               + county_name, data = dt_hh)
summary(m6_cartrips_hh_glmnb)
# N
length(m6_cartrips_hh_glmnb$fitted.values)
## average marginal effects
ame_m6_hh <- summary(margins(m6_cartrips_hh_glmnb,
                             variables = c("bundled_parking_v3","vehicle_count")))
ame_m6_hh


########### Regression 7: WFH and parking
####### individual level model
m7_wfh_psn_glmb <- glm(wloc_home ~ 
                         # bundled_parking_v2
                         bundled_parking_v3
                       + vehicle_count + wday_flexible + share_wking_age_broadband
                       + educ_BAorhigher + educ_lessthanHS
                       # Controls
                       + household_income_f 
                       #+ home_ntype_f 
                       + relevel(home_ntype_f_v1,ref = "rural" )* bundled_parking_v3
                       + occupation + race_f + male + over65
                       + forgnborn + disabled_d, data=dt_psn[dt_psn$employed==1,],family = "binomial")
summary(m7_wfh_psn_glmb)

table(dt_psn$home_ntype_f)
dt_psn$home_ntype_f_v1 <- NA
dt_psn$home_ntype_f_v1[dt_psn$home_ntype_f %in% c("Mixed-use","Old urban","Urban residential")] <- "urban"
dt_psn$home_ntype_f_v1[dt_psn$home_ntype_f %in% c("Established suburb","Patchwork","New development")] <- "suburban"
dt_psn$home_ntype_f_v1[dt_psn$home_ntype_f == "Rural"] <- "rural"
dt_psn$home_ntype_f_v1 <- factor(dt_psn$home_ntype_f_v1,
                                 levels = c("urban","suburban","rural"))

# N
length(m7_wfh_psn_glmb$fitted.values)
## odds ratios
exp(m7_wfh_psn_glmb$coefficients)

####### HH level model
### Household model
m7_wfh_hh_glmb <- glm(wloc_home~
                        # bundled_parking_v2
                        bundled_parking_v3
                      + vehicle_count + wday_flexible + share_wking_age_broadband
                      + educ_BAorhigher + educ_lessthanHS
                      # Controls
                      + household_income_f #+ home_ntype_f 
                      + relevel(home_ntype_f_v1,ref = "rural" )* bundled_parking_v3
                      + asian + black + latino + race_other_d
                      + male + over65 + forgnborn + disabled_d, data = dt_hh, family = "binomial")
summary(m7_wfh_hh_glmb)

dt_hh$home_ntype_f_v1 <- NA
dt_hh$home_ntype_f_v1[dt_hh$home_ntype_f %in% c("Mixed-use","Old urban","Urban residential")] <- "urban"
dt_hh$home_ntype_f_v1[dt_hh$home_ntype_f %in% c("Established suburb","Patchwork","New development")] <- "suburban"
dt_hh$home_ntype_f_v1[dt_hh$home_ntype_f == "Rural"] <- "rural"
dt_hh$home_ntype_f_v1 <- factor(dt_hh$home_ntype_f_v1,
                                 levels = c("urban","suburban","rural"))

# N
length(m7_wfh_hh_glmb$fitted.values)
## odds ratios
exp(m7_wfh_hh_glmb$coefficients)