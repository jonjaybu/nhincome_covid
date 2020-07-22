###### REPRODUCTION CODE V1 -- NOT YET FULLY ANNOTATED
## NEIGHBORHOOD INCOME AND PHYSICAL DISTANCING BEHAVIOR DURING THE COVID-19 PANDEMIC IN THE UNITED STATES
## PREPARED 7/22/20

########################## ########################## ########################## 
########################## PART 1: SET UP NON-SAFEGRAPH DATA
########################## ########################## ########################## 

########################## STEP 1: PREPARE DATA ON CLOSURES  
library(tidyverse)
library(lubridate)

tracker <- read_csv("COVID-19 US state policy database 5_11_2020.csv") %>% # THESE DATA ARE FROM THE COVID-19 US STATE POLICY DATABASE
  slice(1:51) %>% select(c(1, 6:9))
colnames(tracker) <- c("state", "shelter", "shelter_end", "businesses", "businesses_end") 

tracker$state <- state.abb[match(tracker$state, state.name)]
tracker$state[is.na(tracker$state)] <- "DC"

## the treatment indicator is called "txb" here. It switches on with business closures or SAH order, whichever is earlier; ends when either is removed
tracker <- tracker %>% group_by(state) %>% mutate(txb = min(c(shelter, businesses)[which(c(shelter, businesses)>0)]),
                                                  txb_end = min(c(shelter_end, businesses_end)[which(c(shelter_end, businesses_end)>0)]))

tracker[is.na(tracker)] <- 0
tracker$txb_end[tracker$txb==0] <- 0 #can't end if it didn't start

##########################  STEP 2: ADD COVID-19 CASE DATA
library(tidycensus)
library(lubridate)

data(fips_codes)
mystates <- c(state.abb, "DC")

cases <- read_csv("us-states_0518.csv") #DATA ARE FROM THE NYT COVID-19 TRACKER
cases <- left_join(cases %>% select(date, state_code=fips, cases, deaths), fips_codes %>% distinct(state_code, state) %>%
                     filter(state %in% mystates) %>% select(state_code, state)) %>% select(-state_code)

cases$date <- date(ymd(cases$date))

cases <- cases %>% group_by(state) %>% mutate(cases_lag=lag(cases, 1), #ADD A ONE-DAY LAG
                                              deaths_lag=lag(deaths, 1))
cases[is.na(cases)] <- 0

##########################  STEP 3: ADD AMERICAN COMMUNITY SURVEY DATA
acs <- read_csv("ACS_allvars_0530.csv")

############# STEP 4: ADD REGIONS
regions <- read_csv("us census bureau regions and divisions.csv")