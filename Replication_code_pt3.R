###### REPRODUCTION CODE V1 -- NOT YET FULLY ANNOTATED
## NEIGHBORHOOD INCOME AND PHYSICAL DISTANCING BEHAVIOR DURING THE COVID-19 PANDEMIC IN THE UNITED STATES
## PREPARED 7/22/20

########################## ########################## ########################## 
########################## PART 3: ANALYSES USING DATA BY STATE/QUINTILE/DATE
########################## ########################## ########################## 
library(tidyverse)
library(data.table)
library(lubridate)
library(sandwich)
library(lmtest)

################ GROUPS DATA BY ACS VARIABLE: POPULATON-WEIGHTED QUINTILES OF MEDIAN HH INCOME
################ THIS CODE CAN PRODUCE SIMILAR OUTPUTS FOR OTHER VARIABLES (E.G. RACIAL COMPOSITION QUINTILES, ETC)
################ RUN PART 1 OF THIS REPLICATION CODE FIRST TO GET THE OTHER DATASETS INTO MEMORY
acsvars <- "income_quintile_wtd"
labels <- "income5"

########################## THIS CODE REQUIRES SOCIAL DISTANCING METRICS V2 FROM SAFEGRAPH, WEEK OF 1/6/20 THROUGH WEEK OF 4/27/20
########################## WE COMPILED THESE AS A SINGLE DATA TABLE (USING ONLY SELECTED COLUMNS) BEFORE ANALYZING HERE

for(i in 1:length(acsvars)){
  sd <- fread( #[FILE NAME GOES HERE], 
              select=c(origin_census_block_group="numeric", completely_home_device_count="integer",
                       device_count="integer", candidate_device_count="integer", date_range_start="datetime", full_time_work_behavior_devices="integer", 
                       part_time_work_behavior_devices="integer", 
                       delivery_behavior_devices="integer")) %>% 
    mutate(GEOID=str_pad(origin_census_block_group, 12, "left", "0"), #otherwise reads as 11 digits, missing states 01-09
           date=date(ymd_hms(date_range_start)),
           state_code=substr(GEOID, 1, 2),
           work_devices=full_time_work_behavior_devices+part_time_work_behavior_devices+delivery_behavior_devices) %>% 
    filter(GEOID %in% acs$GEOID & date!="2020-02-25") #DROPS DATA FROM DATE WITH KNOWN PROBLEMS
  
  ## Add ACS data
  sd <- left_join(sd %>% mutate(GEOID=as.character(GEOID)), acs %>% select(GEOID, grouper=acsvars[i], pop=populationE)) %>% filter(!is.na(grouper)) %>% 
    filter(pop>0 & !is.na(pop)) 
  
  ## Calculate values within strata
  sd <- sd %>% 
    group_by(state_code, date, grouper) %>% 
    summarise(devices=sum(device_count),
              home_share=sum(completely_home_device_count)/devices,
              work_share=sum(work_devices)/devices,
              pop=sum(pop, na.rm = T)) 
  sd[acsvars[i]] <- sd$grouper 
  sd  <- sd %>% select(-grouper)
  
  ## Add closures data by state
  sd <- left_join(sd, fips_codes %>% distinct(state_code, state)) #gets state abbr for join w/ tracker and cases
  sd <- left_join(sd, tracker)
  
  # Add days pre/post for event studies. NA if policy went out of effect.
  sd$txb_change <- sd$date-date(mdy(sd$txb)) 
  sd$txb_change[sd$date>=date(mdy(sd$txb_end))] <- NA

  # for binary treatment indicator
  sd$txb <- ifelse(sd$date>=date(mdy(sd$txb)), 1, 0)
  sd$txb[sd$date>=date(mdy(sd$txb_end))] <- 0 #reverts to 0 after date when policy ended
  sd[is.na(sd)] <- 0
  
  ## Add case data by county and date
  sd <- left_join(sd, cases %>% select(state, date, cases_lag)) #only using cases, not deaths here
  sd$cases_lag[is.na(sd$cases_lag)] <- 0
  
  ## add lagged case categories
  sd$cases_lag_cut <- cut(sd$cases_lag, breaks = c(0, 1, 10, 100, 1000, 10000, Inf), #see manuscript for rationale
                          include.lowest = T, ordered_result = T)
  
  assign(paste0("sd_", labels[i]), sd)
  rm(sd)
}

######### LIMIT DATES RANGE
dates <- seq(as.Date("2020-01-06"), as.Date("2020-05-01"), 1)
sd_income5 <- sd_income5 %>% filter(date %in% dates)

sd_income5$income_quintile_wtd <- factor(as.numeric(sd_income5$income_quintile_wtd), levels = as.character(1:5)) #make sure it's a factor

############################################### DID MODEL, FULLY INTERACTED ############################################### 
did_model <- lm(home_share~state*income_quintile_wtd + 
                  factor(date)*income_quintile_wtd +
                  cases_lag_cut*income_quintile_wtd +
                  txb*income_quintile_wtd,
                weights = devices, #
                data=sd_income5)
did_results <- round(coeftest(did_model, vcov=vcovCL, cluster=~state), 3)[grepl("txb", names(coef(did_model))),]

colnames(did_results)[1:4] <- c("Estimate", "Std. Error", "t", "P value")
did_results <- did_results %>%as.data.frame() %>% 
  mutate(Estimate= Estimate*100,
         LB = Estimate-1.96*`Std. Error`*100,
         UB = Estimate+1.96*`Std. Error`*100)

############################################### EVENT STUDY MODEL ############################################### 
##########################
sd_income5 <- sd_income5 %>% group_by(state) %>% mutate(ever_treated=max(txb), ever_ended=max(txb_end))
sd_income5$event_day <- as.numeric(sd_income5$txb_change) 

######### SET UP PRE/POST EVENT DAYS
ref_level <- -1 # reference level is day -1
sd_income5$event_day[sd_income5$event_day < -14] <- NA #not estimating effects at dates long before/after
sd_income5$event_day[sd_income5$event_day > 14] <- NA 
sd_income5$event_day[sd_income5$ever_treated==0] <- ref_level #never-treated units join the reference group
sd_income5$event_day <- factor(sd_income5$event_day)
sd_income5$event_day <- relevel(sd_income5$event_day, ref=as.character(ref_level))

######### RUN MODELS -- SEPARATE FOR EACH INCOME QUINTILE
myform <- as.formula("home_share~state+factor(date)+cases_lag_cut+event_day") 
event_results5 <- vector("list", 5)

for(i in 1:5){
  condition = sd_income5$income_quintile_wtd==i
  event5 <- lm(myform,
               weights = devices,
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
  labs(y="Regression coefficient\n", subtitle=paste("Reference level =", ref_level)) +
  facet_wrap(~income_quintile, nrow=2) +
  theme_minimal()