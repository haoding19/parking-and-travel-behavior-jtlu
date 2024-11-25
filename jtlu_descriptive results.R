###### descriptive results

#data sets: household, trip, person
dt_hh <- readRDS("data/data_household_merged_0315.rds")
dt_trip <- readRDS("data/data_place_trip_0315.rds")
dt_psn <- readRDS("data/data_person_merged_0515.rds")

# original data sets, for weights
chts_hh <- readRDS("data/survey_hh_raw.rds")
chts_hh$hhwgt

dt_hh <- merge(dt_hh, chts_hh[,c("sampno","hhwgt")], by="sampno",all.x = T)
dt_hh$hhwgt

chts_psn <- readRDS("data/survey_psn_raw.rds")
chts_psn$perwgt

dt_psn <- merge(dt_psn,chts_psn[,c("sampno","perno","perwgt")],
                   by=c("sampno","perno"), all.x = T)
dt_psn$perwgt


library(stargazer)
library(mosaic)
# We want to first sketch the overall situation 
# with parking, driving, transit use and working from home,
# and the sketch the low price of operating a vehicle. 
# So a table that shows means and medians of:

# Total Daily HH VMT and PMT
dt_hh$tot_dis_car
dt_hh$tot_dis

dt_hh$tot_dis_car_wgt <- dt_hh$tot_dis_car * dt_hh$hhwgt
dt_hh$tot_dis_wgt <- dt_hh$tot_dis * dt_hh$hhwgt

favstats(dt_hh$tot_dis_car_wgt)
favstats(dt_hh$tot_dis_wgt)

# Total Daily Trips
dt_hh$tot_trips
dt_hh$tot_trips_wgt <- dt_hh$tot_trips * dt_hh$hhwgt
favstats(dt_hh$tot_trips_wgt)
sum(dt_hh$tot_trips_wgt)/sum(dt_hh$hhwgt)

dt_hh$tot_trips_car
# Total HH Transit trips
dt_hh$tot_trips_transit
dt_hh$tot_trips_transit_wgt <- dt_hh$tot_trips_transit * dt_hh$hhwgt
favstats(dt_hh$tot_trips_transit_wgt)

stargazer(dt_hh[,c("tot_dis_car","tot_dis","tot_trips","tot_trips_transit")], 
          type = "text", median = T)

# Fraction of Trips that are Transit
dt_hh$transit_trips_frac
summary(dt_hh$transit_trips_frac)
fav_stats(dt_hh$transit_trips_frac)
fav_stats(dt_hh$transit_trips_frac*dt_hh$hhwgt)

# Share of VMT/PMT that is commuting
sort(dt_hh$tot_commute_VMT)
dt_hh$tot_dis_car
dt_hh$tot_commute_PMT
dt_hh$tot_dis

mean(dt_hh$tot_commute_PMT*dt_hh$hhwgt)
mean(dt_hh$tot_commute_PMT)

favstats(dt_hh$tot_commute_VMT)

dt_hh$share_commute_vmt <- dt_hh$tot_commute_VMT/dt_hh$tot_dis_car
dt_hh$share_commute_pmt <- dt_hh$tot_commute_PMT/dt_hh$tot_dis
fav_stats(dt_hh$share_commute_vmt)
fav_stats(dt_hh$share_commute_pmt)


dt_trip$commute_trip
table(dt_trip$mode, useNA = "ifany")
dt_trip$mode_v2 <- car::recode(dt_trip$mode,
                               "5='drive alone';15:28='transit';NA=NA;else='other'")
table(dt_trip$mode_v2, useNA = "ifany")
sum(dt_trip$commute_trip, na.rm=T)
prop.table(table(dt_trip$mode_v2,dt_trip$commute_trip))

dt_trip$trip_distance_miles
table(dt_trip$commute_trip, useNA = "ifany")
prop.table(table(dt_trip$commute_trip, dt_trip$mode_v1))

# Share of Commutes that are by transit and driving alone
# as above, no info on which trips are commutes, but who use what mode for commute
prop.table(table(dt_psn$commute_mode_v1))
table(dt_psn$commute_mode_v1, useNA = "ifany")
sum(!is.na(dt_psn$commute_mode_v1))
# Share of HHs with Bundled Parking at Home
dt_hh$share_bundledpark_units
prop.table(table(dt_hh$bundled_parking_v2))
87.9
sum(!is.na(dt_hh$bundled_parking_v2))

# Share of Employed with Free Parking at Work
############## free work parking, made from trip level parking data
# summary(dt_trip$park_job_free)
# library(dplyr)
psn_work_pking <- dt_trip %>% 
  subset(!is.na(park_job_free)) %>%
  group_by(sampno,perno) %>%
  summarise(free_work_pking = max(park_job_free))

dt_psn <- merge(dt_psn,psn_work_pking,
                by=c("sampno","perno"), all.x = TRUE)

# rm(psn_work_pking)

####share of emloyed who have free parking at work
dt_psn %>%
  filter(employed==1 & !is.na(employed)) %>%
  summarise(mean(free_work_pking,na.rm = T),sum(!is.na(free_work_pking)))


# For Driving:
# Share of trips that involve leaving the vehicle
table(dt_trip$got_out_vehicle, useNA = "ifany")

dt_trip$got_out_vehicle_v1 <- car::recode(dt_trip$got_out_vehicle,
                                          "1=1;2=0;else=NA")
table(dt_trip$got_out_vehicle_v1, useNA = "ifany")

# Share of trips where parking was free
table(dt_trip$parked_payed, useNA = "ifany")

table(dt_trip$parked_loc_type)


dt_trip$parked_free <- 0
dt_trip$parked_free[dt_trip$parked_loc_type==1 |
                      dt_trip$parked_payed==2 |
                      dt_trip$park_emp_reimburse==1] <- 1
## NOTE: here it is assumed that parking onsite is free; if someone parked onsite, 
## parked offsite but didn't pay, or parked offsite but reimbursed by employer, 
## then 1 for parked_free; otherwise, 0
dt_trip$parked_free[dt_trip$got_out_vehicle_v1==0 |
                      is.na(dt_trip$got_out_vehicle_v1)] <- NA
table(dt_trip$parked_free, useNA = "ifany")

#### parked for free v1, many NAs in previous parked for free were 0s
dt_trip$parked_free_v1 <- NA
dt_trip$parked_free_v1[dt_trip$parked_loc_type==1 |
                         dt_trip$parked_loc_type==5 |
                         dt_trip$parked_payed==2 |
                         dt_trip$park_emp_reimburse==1] <- 1
dt_trip$parked_free_v1[dt_trip$parked_payed==1 &
                         dt_trip$park_emp_reimburse==0] <- 0
table(dt_trip$parked_free_v1, useNA = "ifany")


# (If we have it: share of parking that was in offstreet vs on)
table(dt_trip$parked_loc_type)
table(dt_trip$parked_street, useNA = "ifany")

psn_pking_shares <- dt_trip %>% 
                      group_by(sampno,perno) %>%
                      summarise_at(vars(got_out_vehicle_v1,parked_free,parked_free_v1,parked_street),
                                   mean,na.rm=T)

favstats(psn_pking_shares$got_out_vehicle_v1)
favstats(psn_pking_shares$parked_free)
hist(psn_pking_shares$parked_free)
table(psn_pking_shares$parked_free, useNA = "ifany")
favstats(psn_pking_shares$parked_free_v1)

favstats(psn_pking_shares$parked_street)

dt_psn_v1 <- merge(dt_psn, psn_pking_shares, by = c("sampno", "perno"), all.x = T)

View(dt_psn_v1[,c("parked_free","tot_park_cost")])

## share of street parking that paid
table(dt_trip$parked_payed)
dt_trip$parked_payed_v1 <- recode(dt_trip$parked_payed,`1`=1,`2`=0, .default =NaN)
table(dt_trip$parked_payed_v1)

dt_trip$paid_str_pking <- NA
dt_trip$paid_str_pking[dt_trip$parked_street==1 &
                         dt_trip$parked_payed_v1==1] <- 1
dt_trip$paid_str_pking[dt_trip$parked_street==1 &
                         dt_trip$parked_payed_v1==0] <- 0


table(dt_trip$paid_str_pking, useNA = "ifany")

paid_str_pking <- dt_trip %>% 
                  group_by(sampno,perno) %>%
                  summarise(sum(paid_str_pking,na.rm=T))
table(paid_str_pking$`sum(paid_str_pking, na.rm = T)`,useNA = "ifany")
paid_str_pking$paid_str_pking <- 1
paid_str_pking$paid_str_pking[paid_str_pking$`sum(paid_str_pking, na.rm = T)`==0] <- 0
table(paid_str_pking$paid_str_pking, useNA = "ifany")

prop.table(table(paid_str_pking$paid_str_pking, useNA = "ifany"))

# Amount Paid to park
dt_psn$tot_park_cost
favstats(dt_psn$tot_park_cost)
favstats(dt_psn$tot_park_cost[dt_psn$tot_park_cost>0])
favstats(dt_psn$tot_park_cost[dt_psn$tot_park_cost>0 & dt_psn$tot_park_cost <300])
sort(dt_psn$tot_park_cost,decreasing = T)

900/111976     
# Amount spent on gas (do we have this?)
# Amount spent on tolls (do we have this?)
## no info for these two

# What share of employees without free parking at work drove to work?
# What  share of employees with free parking at work drove?

table(dt_psn$free_work_pking, useNA = "ifany")
table(dt_psn$free_work_pking, dt_psn$commute_mode_v1, useNA = "ifany")
prop.table(table(dt_psn$free_work_pking, dt_psn$commute_mode_v1), margin = 1)


# We show that a small share of respondents are in old urban neighborhoods, 
# which previous research shows are more hospitable to non-auto uses. 
# We should generate some quick stats on them. 


# 1.What is their average density, compared to the rest of the sample?
tract_density <- readRDS("data/tract_population density_0208.rds")

dt_hh <- merge(dt_hh, tract_density, 
               by.x = "home_tract_id",by.y = "tract_id", all.x = T)
dt_hh %>% 
  select(home_tract_id,ntype,tract_density) %>%
  distinct() %>%
  summarise(mean(tract_density, na.rm=T))

dt_hh %>% 
  select(home_tract_id,ntype,tract_density) %>%
  distinct() %>%
  group_by(ntype) %>%
  summarise(mean(tract_density))

dt_hh %>%
  group_by(ntype) %>%
  summarise(count=n(), avg_tract_density = mean(tract_density))

dt_psn %>%
  group_by(ntype) %>%
  summarise(count=n(), avg_tract_density = mean(tract_density))


# 2.Are they concentrated in some counties in particular?

table(dt_hh$county_name[dt_hh$ntype=="Old urban"])

# 3.What share of respondents in these neighborhoods had 
# a) no HH cars, and b) no bundled parking?

prop.table(table(dt_hh$hh_ownveh[dt_hh$ntype=="Old urban"]))

prop.table(table(dt_hh$bundled_parking_v2[dt_hh$ntype=="Old urban"]))

# 4.Very important: what share of total transit trips in the sample do they account for,
# and what share of housing without bundled parking, and what share of carless HHs?
sum(dt_hh$tot_trips_transit, na.rm=T)
sum(dt_hh$tot_trips_transit[dt_hh$ntype=="Old urban"], na.rm=T)/sum(dt_hh$tot_trips_transit, na.rm=T)

sum(dt_hh$bundled_parking_v2==0, na.rm=T)

table(dt_hh$bundled_parking_v2[dt_hh$ntype=="Old urban"])[1]/sum(dt_hh$bundled_parking_v2==0, na.rm=T)

table(dt_hh$hh_ownveh)
table(dt_hh$hh_ownveh[dt_hh$ntype=="Old urban"])[1]/table(dt_hh$hh_ownveh)[1]


# 5.What share of trips that involved paid parking occurred in these neighborhoods? 
table(dt_trip$parked_free)

#########merge ntype to dt_trip
str(dt_trip$tract_id)
dt_trip$tract_id_ch <- formatC(dt_trip$tract_id, width = 6, format = "d", flag = "0")

str(dt_trip$county_id)
dt_trip$county_id_ch <- formatC(dt_trip$county_id, width = 3, format = "d", flag = "0")

dt_trip$tract_id_v1 <- paste(dt_trip$state_id,dt_trip$county_id_ch,dt_trip$tract_id_ch,
                             sep ="")

str(dt_trip$tract_id_v1)

nhood_type <- read.csv("data/nhood_types_voulgaris.csv")

dt_trip <- merge(dt_trip,nhood_type[,c("tractid","ntype")],
                 by.x = "tract_id_v1",by.y = "tractid", all.x = T)
table(dt_trip$ntype, useNA = "ifany")

table(dt_trip$ntype[dt_trip$parked_free==0], useNA = "ifany")
prop.table(table(dt_trip$ntype[dt_trip$parked_free==0]))
prop.table(table(dt_trip$ntype[dt_trip$parked_payed==1]))

# 6.What share of respondents in these neighborhoods live in 
# detached single family homes, compared to the sample overall?

prop.table(table(dt_hh$structure_type))
prop.table(table(dt_hh$structure_type[dt_hh$ntype=="Old urban"]))


## additional descriptive stats
### a) the average time a household vehicle is not in motion (parked)
data_plc <- read.csv("CHTS data/caltrans_full_survey/survey_place.csv")

# bring in vehno and trip duration
dt_trip_v1 <- merge(dt_trip,data_plc[,c("sampno","perno","plano","vehno","prev_trip_duration_min")], 
                    by=c("sampno","perno","plano"))

library(dplyr)

# calculate vehicle use duration (mins) for each household vehicle
veh_use_dur <- dt_trip_v1 %>% 
                  group_by(sampno,vehno) %>%
                  distinct(sampno,trip_distance_miles,prev_trip_duration_min) %>%
                  summarise(use_dur = sum(prev_trip_duration_min,na.rm = T))
                  
# View(dt_trip_v1[,c("sampno","perno","plano","vehno","prev_trip_duration_min")])

# View(veh_use_dur[veh_use_dur$sampno %in% c(1447340,1530063,1759857,1782552),])

# compute vehicle parked duration (mins)
veh_use_dur$parked_dur <- 24*60 - veh_use_dur$use_dur

# average time a household vehicle is not in motion:
table(veh_use_dur$vehno, useNA = "ifany")
summary(veh_use_dur$use_dur)
hist(veh_use_dur$use_dur)
table(dt_trip_v2$prev_trip_duration_min)
mosaic::favstats(veh_use_dur$parked_dur[!is.na(veh_use_dur$vehno) 
                                        & veh_use_dur$vehno!=97 
                                        & veh_use_dur$parked_dur>0])
quantile(veh_use_dur$parked_dur[!is.na(veh_use_dur$vehno) 
                                & veh_use_dur$vehno!=97 
                                & veh_use_dur$parked_dur>0],
         c(0.1,0.9))

### b) the average and median VMT for a household with a person who works from home 

table(dt_psn$wloc_home)

#calculate total vmt for hh with at least one person to works from/at home
hh_vmt_telework <- dt_psn %>%
                     filter(wloc_home==1) %>%
                     group_by(sampno) %>%
                     summarise(hh_vmt = sum(tot_dis_car,na.rm = T))

mosaic::favstats(hh_vmt_telework$hh_vmt)

# weighted
hh_vmt_telework <- dt_psn %>%
  filter(wloc_home==1) %>%
  group_by(sampno) %>%
  summarise(hh_vmt_wgt = sum(tot_dis_car*perwgt,na.rm = T))

mosaic::favstats(hh_vmt_telework$hh_vmt_wgt)
##### VMT for HH with a person who has a fixed work location

hh_vmt_fixedworkloc <- dt_psn %>%
                          filter(wloc_fixed==1) %>%
                          group_by(sampno) %>%
                          summarise(hh_vmt = sum(tot_dis_car,na.rm = T))

mosaic::favstats(hh_vmt_fixedworkloc$hh_vmt)

#weighted
hh_vmt_fixedworkloc <- dt_psn %>%
  filter(wloc_fixed==1) %>%
  group_by(sampno) %>%
  summarise(hh_vmt_wgt = sum(tot_dis_car*perwgt,na.rm = T))

mosaic::favstats(hh_vmt_fixedworkloc$hh_vmt_wgt)
##### VMT for HH with a person who has a varied work location

hh_vmt_fixedworkvar <- dt_psn %>%
                          filter(wloc_vary==1) %>%
                          group_by(sampno) %>%
                          summarise(hh_vmt = sum(tot_dis_car,na.rm = T))

mosaic::favstats(hh_vmt_fixedworkvar$hh_vmt)

#weighted

hh_vmt_fixedworkvar <- dt_psn %>%
  filter(wloc_vary==1) %>%
  group_by(sampno) %>%
  summarise(hh_vmt_wgt = sum(tot_dis_car*perwgt,na.rm = T))

mosaic::favstats(hh_vmt_fixedworkvar$hh_vmt_wgt)
#### rearrange variable order and save as new dataset
names(dt_trip_v2)
dt_trip_v2 <- dt_trip_v1[,c(1:4,35,5:11,36,12:34)]

saveRDS(dt_trip_v2,file = "data/data_place_trip_0707.rds")

#### save all three data sets to stata data

library(haven)
write_dta(dt_trip_v2,"data/data_place_trip_0707.dta")
write_dta(dt_psn,"data/data_person_0707.dta")
names(dt_hh)[49] <- "tot_trips_drvtotrnsit_pkfree"
write_dta(dt_hh,"data/data_household_0707.dta")


## share of all trips that involved leaving the vehicle
table(dt_trip$got_out_vehicle)

## transit vs bus/rail/light rail
table(dt_trip$mode, dt_trip$mode_v1)
