### REPLICATION CODE
################################### PART 2: PHYSICAL DISTANCING ANALYSES USING SAFEGRAPH "PATTERNS" (POINT OF INTEREST) DATA ################################### 
####################  CONTAINS CODE TO GENERATE FIG 3

######################### ######################### ######################### ######################### 
######################### ######################### REVISED VISITS ANALYSIS
library(tidyverse)
library(data.table)
library(lubridate)
library(SafeGraphR)

mypoi <- read_csv("POI vars.csv") #load information on points of interest (POI)
### dataset includes all POI of relevant types for which demographic information could be generated from Jan-Feb visits data
### variables: 
#safegraph_place_id
#adj_factor = inverse of device-to-population ratio in home CBGs of visitors to each POI in Jan-Feb (median, weighted by visits)
#median_income = median income of home CBGs of visitors to each POI in Jan-Feb (median, weighted by visits)
#type = out of 7 POI types used in study

my_ids <- mypoi_demo$safegraph_place_id

## Assemble data on visits
visits <- read_many_patterns(dir="YOUR DIRECTORY NAME HERE",
                             select=c("safegraph_place_id",
                                      "visits_by_day",
                                      "raw_visit_counts",
                                      "raw_visitor_counts",
                                      "bucketed_dwell_times"),
                             filter = "safegraph_place_id %in% my_ids") 

fwrite(visits, "Mypoi visits.csv", quote = TRUE, qmethod = "double") #write for easier recall later

## Read data back in
visits <- fread("Mypoi visits.csv")
visits$bucketed_dwell_times <- gsub("\"\"", "\"", visits$bucketed_dwell_times)

## Expand JSON with bucketed dwell times, so we can exclude visits >4hrs later
visit_buckets <- expand_cat_json(dt=visits, expand="bucketed_dwell_times", by=c("safegraph_place_id", "start_date")) #took a few hours

# identify long trips (assumed to be work visits)
long_trips <- subset(visit_buckets, index==">240") %>%
  select(safegraph_place_id, start_date, long_visits=bucketed_dwell_times)

weekly <- visits %>% select(safegraph_place_id, start_date, visits=raw_visit_counts)
weekly <- left_join(weekly, mypoi)

############################## We have median income, but not pop-weighted income quintiles; assign here using same cutpoints as in PT1
library(reldist)
acs <- read_csv("acs_final")
weekly <- weekly %>% mutate(income_quintile_wtd = cut(median_income, breaks = 
                                                        c(0, wtd.quantile(acs$medincomeE, 0.2, weight = acs$populationE),
                                                          wtd.quantile(acs$medincomeE, 0.4, weight = acs$populationE),
                                                          wtd.quantile(acs$medincomeE, 0.6, weight = acs$populationE),
                                                          wtd.quantile(acs$medincomeE, 0.8, weight = acs$populationE),
                                                          Inf), include.lowest = T)) %>%
  mutate(income_quintile_wtd=factor(as.numeric(income_quintile_wtd)))

weekly <- subset(weekly, !is.na(income_quintile_wtd)) # remove places that can't be assigned an income category

############################## SUBTRACT LONG VISITS (PRESUMED TO BE WORK) FROM TOTAL VISITS
weekly <- left_join(weekly %>% mutate(start_date=ymd(start_date)), long_trips %>% 
                      mutate(start_date=ymd(start_date)))

weekly$non_work <- weekly$visits-weekly$long_visits

weekly$non_work_adj <- weekly$non_work * weekly$adj_factor ## adjust for resident to device ratio (see above)

###############  ADD ALL OF THE WEEK-POI PAIRS WITH ZERO VISITS ############### 
weeks <- unique(weekly$start_date)
places <- unique(weekly$safegraph_place_id)

weekly2 <- data.frame(safegraph_place_id= rep(places, each=length(weeks)),
                      start_date = rep(weeks, length.out=length(weeks)*length(places)))

weekly2 <- left_join(weekly2, weekly %>% select(safegraph_place_id, start_date, 
                                                non_work_adj))

weekly2$non_work_adj[is.na(weekly2$non_work_adj)] <- 0

weekly <- left_join(weekly2, weekly %>% distinct(safegraph_place_id, 
                                                 type, income_quintile_wtd))


## RECODE LOCATION TYPES FOR READABILITY
weekly <- weekly %>% mutate(type=recode(type, "Carryout"="Carryout Restaurants",
                                        "Churches"="Places of Worship",
                                        "Convenience"="Convenience Stores",
                                        "Grocery"="Supermarkets",
                                        "Liquor"="Beer, Wine and Liquor Stores",
                                        "Park"="Parks and Playgrounds"))

############### VISUALIZE FOR FIG 3

weekly %>% filter(!is.na(income_quintile_wtd)) %>% 
  group_by(start_date, income_quintile_wtd, type) %>%
  summarize(visits_per=mean(non_work_adj)) %>%
  group_by(income_quintile_wtd, type) %>%
  ggplot() + geom_line(aes(x=start_date, y=visits_per, group=as.factor(income_quintile_wtd), color=as.factor(income_quintile_wtd)), size=1.05) +
  facet_wrap(~type, scales = "free_y") +
  labs(y="Mean device visits per week per location\n", x="Week starting date (2020)",
       color="Visitor\nincome\nquintile") +
  theme_minimal() +
  ylim(0, NA)
