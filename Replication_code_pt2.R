###### REPRODUCTION CODE V1 -- NOT YET FULLY ANNOTATED
## NEIGHBORHOOD INCOME AND PHYSICAL DISTANCING BEHAVIOR DURING THE COVID-19 PANDEMIC IN THE UNITED STATES
## PREPARED 7/22/20

########################## ########################## ########################## 
########################## PART 2: DESCRIPTIVES USING SAFEGRAPH DATA
########################## ########################## ########################## 
library(tidyverse)
library(data.table)
library(lubridate)

########################## THIS CODE REQUIRES SOCIAL DISTANCING METRICS V2 FROM SAFEGRAPH, WEEK OF 1/6/20 THROUGH WEEK OF 4/27/20
########################## WE COMPILED THESE AS A SINGLE DATA TABLE (USING ONLY SELECTED COLUMNS) BEFORE ANALYZING HERE

sd <- fread("social_distancing2_p1cols_0510.csv", 
            select=c(origin_census_block_group="numeric", completely_home_device_count="integer",
                     device_count="integer", candidate_device_count="integer", date_range_start="datetime", full_time_work_behavior_devices="integer", 
                     part_time_work_behavior_devices="integer", 
                     delivery_behavior_devices="integer")) %>% 
  mutate(GEOID=str_pad(origin_census_block_group, 12, "left", "0"), #otherwise reads as 11 digits only
         date=date(ymd_hms(date_range_start)),
         state_code=substr(GEOID, 1, 2),
         work_devices=full_time_work_behavior_devices+part_time_work_behavior_devices+delivery_behavior_devices) %>% 
  filter(GEOID %in% acs$GEOID)

## Add ACS data
sd <- left_join(sd %>% mutate(GEOID=as.character(GEOID)), acs) %>% filter(populationE>0) #drop BGs with zero population
sd <- left_join(sd, fips_codes %>% distinct(state, state_code))
sd <- left_join(sd, regions %>% select(state=`State Code`, region=Region, division=Division)) 

dates <- seq(as.Date("2020-01-06"), as.Date("2020-05-01"), 1)

### CREATE A DATAFRAME THAT DROPS OBS FROM DATE WITH KNOWN PROBLEMS (2/25/20) AND BGS WHERE NO INCOME DATA REPORTED
### AND AGGREGATES VALUES, FIRST BY WEEK + INCOME QUINTILE ONLY, AND THEN WITH ADDITIONAL GROUPING VARIABLES

tmp <- sd %>% filter(!is.na(income_quintile_wtd) & date %in% dates & date!="2020-02-25") %>% 
  group_by(week=isoweek(date), income_quintile_wtd) %>%
  summarize(end_date=max(date), 
            start_date=min(date),
            home_share=sum(completely_home_device_count)/sum(device_count),
            work_share=sum(work_devices)/sum(device_count))

############# FIGURES 1, 3
ggplot(tmp) + geom_line(aes(x=end_date, y=home_share, color=factor(as.numeric(factor(income_quintile_wtd)))), size=1.1) + 
  labs(x="", y="Proportion at home\n") +
  scale_color_discrete(name="Income\nquintile") +
  theme_minimal() 

ggplot(tmp) + geom_line(aes(x=start_date, y=work_share, color=factor(as.numeric(factor(income_quintile_wtd)))), size=1.1)+ 
  labs(x="", y="Proportion at work\n") +
  scale_color_discrete(name="Income\nquintile")+
  theme_minimal()

########## FIGURE 2

tmp <- sd %>% filter(!is.na(income_quintile_wtd) & date %in% dates & date!="2020-02-25") %>% 
  group_by(week=isoweek(date), income_quintile_wtd, NCHS) %>%
  summarize(end_date=max(date),
            start_date=min(date),
            home_share=sum(completely_home_device_count)/sum(device_count)) %>%
  mutate(NCHS=recode(NCHS, "1"="1_Large central metro",
                     "2"="2_Large fringe metro",
                     "3"="3_Medium metro",
                     "4"="3_Small metro",
                     "5"="5_Micropolitan",
                     "6"="6_Noncore"))
ggplot(tmp) + geom_line(aes(x=end_date, y=home_share, color=factor(as.numeric(factor(income_quintile_wtd)))), size=1.1) + 
  labs(x="", y="Proportion at home\n") +
  scale_color_discrete(name="Income\nquintile") +
  facet_wrap(~NCHS) +
  theme_minimal()

########## FIGURE S1
tmp <- sd %>% filter(!is.na(income_quintile_wtd) & date %in% dates & date!="2020-02-25") %>% 
  group_by(week=isoweek(date), income_quintile_wtd, region) %>%
  summarize(end_date=max(date),
            start_date=min(date),
            home_share=sum(completely_home_device_count)/sum(device_count)) 

ggplot(tmp) + geom_line(aes(x=end_date, y=home_share, color=factor(as.numeric(factor(income_quintile_wtd)))), size=1.1) + 
  labs(x="", y="Proportion at home\n") +
  scale_color_discrete(name="Income\nquintile") +
  facet_wrap(~region) +
  theme_minimal() 

