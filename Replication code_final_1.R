### REPLICATION CODE
################################### PART 1: PHYSICAL DISTANCING ANALYSES USING SAFEGRAPH SOCIAL DISTANCING METRICS ################################### 
####################  CONTAINS CODE TO GENERATE FIGS 1, 2, 4 AND TABLE 4

library(tidyverse)
library(data.table)
library(lubridate)

########### Import ancillary datasets
#### ADD AMERICAN COMMUNITY SURVEY DATA
acs <- read_csv("acs_final.csv") #includes population-weighted income quintile, analyzed separately using weighted.quantile function in reldist package

#### State physical distancing orders, from the COVID-19 US State Policy Database: https://www.statepolicies.com/
closings <- read_csv("COVID-19 US state policy database 5_11_2020.csv") %>% 
  slice(1:51) %>% select(c(1:2, 6, 8))
colnames(closings) <- c("state", "emergency", "shelter", "businesses") 

closings$state <- state.abb[match(closings$state, state.name)]
closings$state[is.na(closings$state)] <- "DC"

## the treatment indicator is called "tx" here. It switches on with business closures or SAH order, whichever is earlier
closings <- closings %>% group_by(state) %>% mutate(tx = min(c(shelter, businesses)[which(c(shelter, businesses)>0)]))
closings$tx[is.na(closings$tx)] <- 0

########### Assemble SafeGraph social distancing data ########### ########### ########### ########### ###########  
### Assumes you have already downloaded SafeGraph social distancing metrics v2, for the dates specified below
remotes::install_github('SafeGraphInc/SafeGraphR') #Many thanks to Nick Huntington-Klein: https://safegraphinc.github.io/SafeGraphR/
library(SafeGraphR)

sd <- read_distancing(start=lubridate::ymd("2020-01-06"),
                        end=lubridate::ymd("2020-05-03"),
                        dir="",
                        by=F,
                        select=c("origin_census_block_group",
                                 "completely_home_device_count",
                                 "device_count",
                                 "date_range_start",
                                 "full_time_work_behavior_devices",
                                 "part_time_work_behavior_devices",
                                 "delivery_behavior_devices"))

sd <- subset(sd, date != "2020-02-25") #drop data from date with known problems

fwrite(sd, "sd_2020_raw.csv") #now save, for easier recall in future sessions

########### Prepare SafeGraph social distancing data for analysis

#Reload saved data
sd <- fread("sd_2020_raw.csv",
            select=c(origin_census_block_group="numeric", completely_home_device_count="integer",
                     device_count="integer", candidate_device_count="integer", date_range_start="datetime", full_time_work_behavior_devices="integer",
                     part_time_work_behavior_devices="integer",
                     delivery_behavior_devices="integer")) %>%
  mutate(GEOID=str_pad(origin_census_block_group, 12, "left", "0"), #otherwise reads as 11 digits only
         date=date(ymd_hms(date_range_start)),
         state_code=substr(GEOID, 1, 2), #for matching to state FIPS codes
         work_devices=full_time_work_behavior_devices+part_time_work_behavior_devices+delivery_behavior_devices) %>%
  filter(GEOID %in% acs$GEOID) #dropping GEOIDs from outside our sample (e.g. Puerto Rico)

#Join with census data
sd <- left_join(sd, acs) %>% filter(populationE>0 & !is.na(income_quintile_wtd)) #join with census data, remove BGs with zero population and/or quintile can't be calculated
sd$income_quintile_wtd <- factor(sd_income5$income_quintile_wtd) #make sure this variable is read correctly

### Aggregate by week for figs 1-2
tmp <- sd %>%
  group_by(week=isoweek(ymd(date)), income_quintile_wtd) %>% #group outcomes by week
  summarize(start_date=min(date),
            home_share=sum(completely_home_device_count)/sum(device_count),
            work_share=sum(work_devices)/sum(device_count))

############# FIGURES 1, 2
ggplot(tmp) + geom_line(aes(x=start_date, y=home_share, color=income_quintile_wtd), size=0.8) + 
  labs(x="", y="Proportion at home\n") +
  scale_color_discrete(name="Income\nquintile") +
  theme_minimal() +
  ylim(0, NA) +
  ggsave(filename="Figure 1.png", width=8, height=6)

ggplot(tmp) + geom_line(aes(x=start_date, y=work_share, color=income_quintile_wtd), size=0.8)+ 
  labs(x="", y="Proportion at work\n") +
  scale_color_discrete(name="Income\nquintile")+
  theme_minimal() +
  ylim(0, NA) +
  ggsave(filename="Figure 2.png", width=8, height=6)

################################### STATE POLICY EFFECTS ################################### 
#### AGGREGATE DATA BY STATE, QUINTILE, AND DATE
sd_income5 <- sd %>%
  group_by(state_code, date, income_quintile_wtd) %>%
  summarise(devices=sum(device_count),
            home_share=sum(completely_home_device_count)/devices,
            work_share=sum(work_devices)/devices,
            pop=sum(populationE, na.rm = T)) %>% ungroup()

sd_income5 <- left_join(sd_income5, fips_codes %>% distinct(state_code, state)) #gets state abbr for join w/ tracker 

## Add closures data
sd_income5 <- left_join(sd_income5, closings)

# Add days pre/post for event studies. 
sd_income5$tx_change <- sd_income5$date-date(mdy(sd_income5$tx)) 
sd_income5$ed_change <- sd_income5$date-date(mdy(sd_income5$emergency)) 

# Create binary treatment indicator
sd_income5$tx <- ifelse(sd_income5$date>=date(mdy(sd_income5$tx)), 1, 0)
sd_income5$tx[is.na(sd_income5$tx)] <- 0 #some states never closed, they're assigned 0

sd_income5$ed <- ifelse(sd_income5$date>=date(mdy(sd_income5$emergency)), 1, 0) 

######## Regressions will be weighted by devices observed in January, February 2020
sd_income5 <- sd_income5 %>% 
  group_by(state, income_quintile_wtd) %>%
  mutate(devices_pre = mean(devices[date<=ymd("2020-02-29")], na.rm = T)) 

################## DID MODEL, FULLY INTERACTED ################## 
###### NOTE: THIS RUNS QUICKLY ON MACBOOK PRO W/ 32 GB RAM, BUT MAY REQUIRE PARALLELIZATION FOR USE ON CLUSTERS
did_model <- lm(home_share~state*income_quintile_wtd + 
                  factor(date)*income_quintile_wtd +
                  tx*income_quintile_wtd, #switch tx to ed for emergency declarations (here and next full line)
                weights = devices_pre, 
                data=subset(sd_income5, date<ymd("2020-04-20")))
did_results <- round(coeftest(did_model, vcov=vcovCL, cluster=~state), 3)[grepl("tx", names(coef(did_model))),] 

colnames(did_results)[1:4] <- c("Estimate", "Std. Error", "t", "P value")
did_results <- did_results %>%as.data.frame() %>% 
  mutate(Estimate= Estimate*100,
         LB = Estimate-1.96*`Std. Error`*100,
         UB = Estimate+1.96*`Std. Error`*100)

################ PLACEBO TESTS ON DID MODEL ####################
niter <- 500

tx_var5 <- sd_income5 %>% distinct(state, date, txb) %>% ungroup() %>% #grab the state, date, and treatment variables
  arrange(state, date) %>% select(date, placebo=txb) #then drop states
states <- unique(sd_income5$state)
placebo_results5 <- vector("list", niter)

set.seed(1234) 

for(j in 1:niter){
  tx_var5$state <- rep(sample(states, 51, replace=F), each=147) #randomly reorder states
  tmp <- suppressMessages(left_join(sd_income5, tx_var5)) %>% ungroup() %>%
    select(home_share, state, income_quintile_wtd, date,
           placebo, devices_pre)
  model <- lm(home_share~state*income_quintile_wtd + 
                factor(date)*income_quintile_wtd +
                placebo*income_quintile_wtd,
              weights = devices_pre, 
              data=subset(tmp, date<ymd("2020-04-20")))
  tmp_results <- round(coeftest(model, vcov=vcovCL, cluster=~state), 3)[grepl("placebo", names(coef(model))),] %>% as_data_frame()
  colnames(tmp_results)[1:4] <- c("Estimate", "Std. Error", "t", "P value")
  tmp_results$var <- 1:5
  tmp_results$iter <- j
  placebo_results5[[j]] <- tmp_results 
  
  if(j %in% seq(0, niter, 10)){print(paste(j, "done"))}
}

placebo_results5 <- bind_rows(placebo_results5)

################ EVENT STUDY MODEL ####################

sd_income5 <- sd_income5 %>% group_by(state) %>% mutate(ever_treated=max(tx)) #identify never-treated states 
sd_income5$event_day <- as.numeric(sd_income5$tx_change) #switch to ed_change for emergency decl

######### SET UP PRE/POST EVENT DAYS
ref_level <- -1 # reference level is day -1
sd_income5$event_day[sd_income5$event_day < -14] <- -14 
sd_income5$event_day[sd_income5$event_day > 14] <- 14
sd_income5$event_day[sd_income5$ever_treated==0] <- ref_level #never-treated units join the reference group
sd_income5$event_day <- factor(sd_income5$event_day)
sd_income5$event_day <- relevel(sd_income5$event_day, ref=as.character(ref_level)) #model will omit dummy for reference level

######### RUN MODELS -- SEPARATE FOR EACH INCOME QUINTILE
myform <- as.formula("home_share~state+factor(date)+event_day+tx") 
event_results5 <- vector("list", 5)

for(i in 1:5){
  condition = sd_income5$income_quintile_wtd==i & sd_income5$date<=ymd("2020-04-20") 
  event5 <- lm(myform,
               weights = devices_pre,
               data=sd_income5[condition,]) 
  tmp_results <- as.data.frame(round(coeftest(event5, vcov=vcovCL, cluster=~state)[(length(coef(event5))-27):length(coef(event5)),], 3)) #POSITIONS OF THE COEFFICIENTS OF INTEREST
  tmp_results$day <- rownames(tmp_results)
  tmp_results$day <- gsub("event_day", "", tmp_results$day)
  tmp_results$day <- as.numeric(tmp_results$day)
  tmp_results <- tmp_results %>% mutate(
    LB = Estimate-1.96*`Std. Error`,
    UB = Estimate+1.96*`Std. Error`)
  tmp_results[nrow(tmp_results)+1,] <- c(0, rep(NA, 3), ref_level, rep(NA, 2))
  tmp_results$income_quintile <- paste("Quintile", str_pad(i, 2, "left", "0"))
  event_results5[[i]] <- tmp_results
  print(paste(i, "done"))}

event_results5 <- bind_rows(event_results5)

######### VISUALIZE RESULTS
ggplot(event_results5) +
  geom_vline(xintercept = 0, linetype=2, color="gray50") +
  geom_hline(yintercept = 0, linetype=2, color="gray50") +
  geom_point(aes(x=day, y=Estimate, color="Point estimate")) +
  geom_line(aes(x=day, y=Estimate, group=factor(day<0), color="Point estimate")) +
  geom_line(aes(x=day, y=LB, group=factor(day<0), color="95% CI")) +
  geom_line(aes(x=day, y=UB, group=factor(day<0), color="95% CI")) +
  scale_color_manual(values = c("gray70", "black"), name="") +
  scale_x_continuous(limits = c(-14, 14), breaks = seq(-14, 14, 2), 
                     labels = c("-14+", seq(-12, 12, 2), "14+"),
                     name="\nDays from treatment") +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(y="Regression coefficient\n") + #, subtitle=paste("Reference level =", ref_level)) +
  facet_wrap(~income_quintile, nrow=2) +
  theme_minimal()



