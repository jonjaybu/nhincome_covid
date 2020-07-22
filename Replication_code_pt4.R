###### REPRODUCTION CODE V1 -- NOT YET FULLY ANNOTATED
## NEIGHBORHOOD INCOME AND PHYSICAL DISTANCING BEHAVIOR DURING THE COVID-19 PANDEMIC IN THE UNITED STATES
## PREPARED 7/22/20

########################## ########################## ########################## 
########################## PART 4: ANALYSES USING POINT OF INTEREST VISITS
########################## ########################## ########################## 

library(tidyverse)
library(data.table)
library(lubridate)
library(jsonlite)
library(purrr)

########################## STEP 1: ASSIGN DEMOGRAPHICS TO POINTS OF INTEREST (BASED ON MEDIAN VISITOR CHARACTERISTICS)
####### THIS STEP REQUIRES THE SAFEGRAPH JAN 2020 MONTHLY PATTERNS HOME PANEL SUMMARY DATA
panel <- read_csv("#FILENAME HERE")
panel <- left_join(panel %>% mutate(GEOID=census_block_group), acs %>% select(GEOID,  income_decile, pop=populationE))
panel <- panel %>% filter(!is.na(pop) & pop!=0) %>%
  mutate(total_devices=sum(number_devices_residing), total_pop=sum(pop), device_to_pop=number_devices_residing/pop,
         adj_factor=pop/total_pop * total_devices/number_devices_residing) %>% 
  filter(device_to_pop<1 & device_to_pop>0.01) #remove CBGs with too many devices or too few

###################### ###################### USE VISITOR COUNTS TO IDENTIFY MEDIAN INCOME BY LOCATION
####### THIS STEP REQUIRES SAFEGRAPH JANUARY AND FEBRUARY MONTHLY PATTERNS DATA
####### FIRST, FILTER BY POI CATEGORY, THEN "EXPLODE" VISITOR CBG COLUMN TO GET VISITORS BY HOME CBG
####### PYTHON CODE AVAILABLE UPON REQUEST -- JONJAY@BU.EDU

for(i in c("0120", "0220")){
  v <- read_csv(paste0(#FILENAME HERE,
                       i, ".csv")) %>% select(-1) %>%
    mutate(GEOID=str_pad(visitor_home_cbg, 12, "left", "0"),
           state_code=substr(GEOID, 1, 2)) %>% filter(GEOID %in% acs$GEOID & safegraph_place_id %in% mypoi$safegraph_place_id & 
                                                        GEOID %in% panel$GEOID) %>%
    mutate(week=paste0(substr(i, 1, 2), "-", substr(i, 3, 4))) #calling these weeks, then will divide by 4
  
  v <- bind_rows(v, read_csv(paste0(#FILENAME HERE,
                                    i, "_newcats1.csv")) %>% select(-1) %>%
                   mutate(GEOID=str_pad(visitor_home_cbg, 12, "left", "0"),
                          state_code=substr(GEOID, 1, 2)) %>% filter(GEOID %in% acs$GEOID & safegraph_place_id %in% mypoi$safegraph_place_id & 
                                                                       GEOID %in% panel$GEOID) %>%
                   mutate(week=paste0(substr(i, 1, 2), "-", substr(i, 3, 4))))
  
  v <- left_join(v, acs %>% select(GEOID,  pop=populationE))
  v <- left_join(v, fips_codes %>% distinct(state, state_code))
  v <- left_join(v, mypoi %>% select(safegraph_place_id, type))
  v <- left_join(v, panel %>% select(GEOID, devices=number_devices_residing, adj_factor)) %>%
    mutate(visitor_count_adj = visitor_count*adj_factor)
  
  assign(paste0("visitors_", i), v)
  rm(v)
}

visitors <- full_join(visitors_0120 %>% select(safegraph_place_id, GEOID, visitor_count_adj1 = visitor_count_adj),
                      visitors_0220 %>% select(safegraph_place_id, GEOID, visitor_count_adj2 = visitor_count_adj)) 
visitors[, 3:4][is.na(visitors[, 3:4])] <- 0
visitors <- visitors %>% mutate(visitor_count_adj=visitor_count_adj1+visitor_count_adj2) %>% select(-c(visitor_count_adj1, visitor_count_adj2))
visitors <- left_join(visitors, panel %>% select(GEOID, adj_factor)) 
rm(visitors_0120, visitors_0220, panel)

###################### ASSIGN INCOME QUINTILES (AND OTHER DEMOGRAPHICS, NOT USED HERE BUT POTENTIALLY USEFUL ELSEWHERE)
library("matrixStats")

visitors <- left_join(visitors, acs %>% mutate(black=blackE/populationE, latino=latinoE/populationE, poverty=povertyE/populationE ) %>%
                        select(GEOID, medincome=medincomeE, black, latino, poverty))

visitors <- visitors %>% 
  group_by(safegraph_place_id) %>%
  mutate(maxGEOID=GEOID[which.max(visitor_count_adj)]) %>%
  mutate(adj_factor=weightedMedian(adj_factor, visitor_count_adj, na.rm = T), #USE RELDIST PACKAGE FOR WEIGHTED MEDIANS
         median_income=weightedMedian(medincome, visitor_count_adj, na.rm = T),
         median_black=weightedMedian(black, visitor_count_adj, na.rm = T),
         median_latino=weightedMedian(latino, visitor_count_adj, na.rm = T),
         median_poverty=weightedMedian(poverty, visitor_count_adj, na.rm = T))

poi_demographics <-  visitors %>% 
  select(safegraph_place_id, adj_factor, median_income, median_black, median_latino, median_poverty, maxGEOID) %>% distinct()

########################## STEP 2: ANALYZE VISITS

########################## THIS CODE REQUIRES SAFEGRAPH WEEKLY PATTERNS DATA, WEEK OF 1/6/20 THROUGH WEEK OF 4/27/20
########################## PRIOR TO ANALYSIS HERE, WE COMBINED THESE WEEKLY DATA USING A SUBSET OF PLACES, PYTHON CODE AVAILABLE UPON REQUEST

################## Weekly patterns
visits <- fread(#[FILENAME GOES HERE], 
  header=T) %>% select(-1) %>%
  filter(safegraph_place_id %in% poi_demographics$safegraph_place_id) #only care about ones where we know demos

weeks <- unique(visits$week)
weekly <- vector("list", length(weeks))

### generate weekly visits by place id:
for(i in 1:length(weeks)){
  tmp <- map(visits$visits_by_day[visits$week==weeks[i]], ~ fromJSON(., flatten = T)) 
  tmp <- data.frame(matrix(unlist(tmp), nrow=nrow(visits[visits$week==weeks[i],]), byrow=T),stringsAsFactors=FALSE)
  tmp <- rowMeans(tmp)
  tmp <- cbind(visits[visits$week==weeks[i],] %>% select(safegraph_place_id), visits=tmp)
  tmp <- left_join(poi_demographics %>% select(safegraph_place_id), tmp)
  tmp$week <- weeks[i]
  tmp$visits[is.na(tmp$visits)] <- 0
  weekly[[i]] <- tmp 
  rm(tmp)
  print(paste(weeks[i], "done"))
}

weekly <- bind_rows(weekly) 
rm(visits)

###### ADD VARIABLES
weekly <- left_join(weekly,  poi_demographics) 
weekly <- left_join(weekly, mypoi %>% select(safegraph_place_id, type, state=region))
weekly$week <- ymd(weekly$week)
weekly <- weekly %>% mutate(visits_adj=visits*adj_factor)

####### RESTRICT SAMPLE TO CITIES
weekly <- left_join(weekly, acs %>% select(maxGEOID=GEOID, NCHS)) %>% filter(!NCHS %in% c("5", "6"))
weekly <- weekly %>% mutate(NCHS=recode(NCHS, "1"="1_Large central metro",
                                        "2"="2_Large fringe metro",
                                        "3"="3_Small-Medium metro",
                                        "4"="3_Small-Medium metro"))

library(reldist) 
weekly <- weekly %>% mutate(income_quintile_wtd = cut(median_income, breaks = 
                                                        c(0, wtd.quantile(acs$medincomeE, 0.2, weight = acs$populationE),
                                                          wtd.quantile(acs$medincomeE, 0.4, weight = acs$populationE),
                                                          wtd.quantile(acs$medincomeE, 0.6, weight = acs$populationE),
                                                          wtd.quantile(acs$medincomeE, 0.8, weight = acs$populationE),
                                                          Inf), include.lowest = T)) %>%
  mutate(income_quintile_wtd=factor(as.numeric(income_quintile_wtd)))

weekly <- weekly %>% mutate(type=recode(type, "Carryout"="Carryout Restaurants",
                                        "Churches"="Places of Worship",
                                        "Convenience"="Convenience Stores",
                                        "Grocery"="Supermarkets",
                                        "Liquor"="Beer, Wine and Liquor Stores",
                                        "Park"="Parks and Playgrounds"))

weekly <- weekly %>% select(-c(median_income, median_black, median_latino, median_poverty, maxGEOID)) #trim for memory

######################### VISUALIZE RESULTS
weekly %>% filter(!is.na(income_quintile_wtd)) %>% 
  group_by(week, income_quintile_wtd, type) %>%
  summarize(visits_per=mean(visits_adj)) %>%
  group_by(income_quintile_wtd, type) %>%
  ggplot() + geom_line(aes(x=week, y=visits_per, group=as.factor(income_quintile_wtd), color=as.factor(income_quintile_wtd)), size=1.05) +
  facet_wrap(~type, scales = "free_y") +
  labs(y="Mean device visits per week per location\n", x="Week starting date (2020)",
       #  title="Visits by location type (urban areas only)",
       color="Visitor\nincome\nquintile") +
  theme_minimal() 