#####################################################
####             BIOS 6643 Project               ####
####        Longitudinal Data Analysis           #### 
#### Aim : Explain daily weight data through     ####
####       Functional Data Analysis              ####
#####################################################


#### Load in the data 
wt <- read.csv("D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_with_confounding.csv")


#### Plot data x-axis: study_days, y-axis:  wt_lb, group by: participant_id
library(ggplot2)

ggplot(data = wt, aes(x = study_days, y = wt_lb, group = participant_id)) + 
  geom_line()

#### Transform into wide format - use only the weight variable over time 
library(dplyr)
library(reshape2)
library(tidyr)
wt.wide <- wt %>% 
  select(wt_lb, study_days) %>% 
  spread(study_days, wt_lb)

wt.wide <- reshape(wt[,c(3,13,14)], idvar = "participant_id", timevar = "study_days", direction = "wide")

#### Cohort 1
co1 <- wt %>% 
  filter(cohort == 1) %>% 
  mutate(Date = as.Date(weight_dates)) %>% 
  group_by(participant_id) %>% 
  filter(Date == max(Date, na.rm = T))

#### Cohort 2 
co2 <- wt %>% 
  filter(cohort == 2) %>% 
  mutate(Date = as.Date(weight_dates)) %>% 
  group_by(participant_id) %>% 
  filter(Date == max(Date, na.rm = T))

#### Cohort 3
co3 <- wt %>% 
  filter(cohort == 3) %>% 
  mutate(Date = as.Date(weight_dates)) %>% 
  group_by(participant_id) %>% 
  filter(Date == max(Date, na.rm = T))



#### Summarize the amount of time in study
max.day <- wt %>% 
  group_by(participant_id) %>% 
  summarise(max.day = max(study_days))







#### Clean data 
# Keep only important variables 
wt <- wt[,c(3,4,10,13,14,55:57)]

# Add missing data 
library(dplyr)
library(tidyr)
library(stringr)

wt <- wt %>% 
  group_by(participant_id) %>%  
  mutate(weight_dates = as.Date(weight_dates)) %>%
  complete(weight_dates = seq.Date(min(weight_dates, na.rm = T), 
           max(weight_dates, na.rm = T), by="day")) %>% 
  mutate(cohort = min(cohort, na.rm = T), sex = min(sex, na.rm = T), race = min(race, na.rm = T),
    Day1 = min(as.Date(weight_dates), na.rm = T))

wt$study_days <- wt$weight_dates - wt$Day1 + 1


# Add season variable 
wt$month <- str_split_fixed(wt$weight_dates, "-", 3)[,2]
wt$month <- as.numeric(wt$month)

wt$season <- ifelse(wt$month %in% c(12, 1, 2), "Winter", 
             ifelse(wt$month %in% c(3, 4, 5), "Spring", 
             ifelse(wt$month %in% c(6, 7, 8), "Summer", "Autumn")))

# Truncate cohort times to a year 
wt <- wt[wt$study_days <= 365 ,]
wt <- wt[!(is.na(wt$participant_id)),]
wt <- wt[!duplicated(wt[,c(1,2)]),]


# Remove KBU-082
wt <- wt[!(wt$participant_id == "KBU-082"),]




# Age vs weight plot 
wt.change <- wt %>% 
  group_by(participant_id) %>% 
  filter(study_days %in% c(min(study_days), max(study_days[!(is.na(wt_lb))]))) %>% 
  summarise(age = age, change = wt_lb[study_days > 1] - wt_lb[study_days == 1]) %>% 
  slice(1)

ggplot(wt.change, aes(x = age, y=change)) + 
  geom_point(col = "blue", pch = 16, lwd = 2) + 
  geom_hline(yintercept = mean(wt.change$change), col = "red")

ggplot(wt.change, aes(x = age, y=change)) + 
  geom_point(col = "blue", pch = 16, lwd = 2)

ggplot(wt[wt$study_days == 1,], aes(x = age, y=wt_lb)) + 
  geom_point(col = "blue", pch = 16, lwd = 2)







# change to week scale 
library(lubridate)
wt.week <- wt %>% 
  group_by(participant_id, Week.date = floor_date(weight_dates, unit="week")) %>% 
  summarise(mean.wt = mean(wt_lb, na.rm = T), cohort = min(cohort, na.rm = T), sex = min(sex, na.rm = T), 
    race = min(race, na.rm = T), age = min(age, na.rm = T)) %>% 
  summarise(Week = as.numeric(Week.date - min(Week.date))/7 + 1, cohort = min(cohort, na.rm = T), sex = min(sex, na.rm = T), 
    race = min(race, na.rm = T), age = min(age, na.rm = T), mean.wt = mean.wt, Week.date = Week.date)

wt.week$mean.wt <- ifelse(wt.week$mean.wt == "NaN", NA, wt.week$mean.wt)

wt.week$month <- str_split_fixed(wt.week$Week.date, "-", 3)[,2]
wt.week$month <- as.numeric(wt.week$month)

wt.week$season <- ifelse(wt.week$month %in% c(12, 1, 2), "Winter", 
  ifelse(wt.week$month %in% c(3, 4, 5), "Spring", 
    ifelse(wt.week$month %in% c(6, 7, 8), "Summer", "Autumn")))


# How much missing data is there 
miss <- wt.week %>% 
  group_by(participant_id) %>% 
  summarise(missing = sum(is.na(mean.wt))/n()) %>% 
  filter(missing > 0.8)

# remove people with a lot missing 
wt.week <- wt.week[!(wt.week$participant_id %in% miss$participant_id),]
  

# Adjust age so it changes daily
wt$age <- wt$age + (wt$study_days - 1)*(1/365)
wt.week$age <- wt.week$age + (wt.week$Week - 1)*(7/365)
  
  

wt.week$age <- ifelse(wt.week$age <= 30, "20-30", 
  ifelse(wt.week$age <= 40, "30-40", 
    ifelse(wt.week$age <= 50, "40-50", "50-60")))

wt$study_days <- as.numeric(wt$study_days)
ggplot(data = wt[!(wt$cohort == 3),], aes(x = study_days, y = wt_lb, group = participant_id)) + 
 geom_line(aes(col = age)) + 
  stat_summary(fun.y=mean,geom="line",lwd = 1.5,aes(group=age, col = age))


# Add baseline weight 
weight.bs <- wt %>% group_by(participant_id) %>% 
  filter(study_days == 1)
wt.week$weight.bs <- weight.bs$wt_lb[match(wt.week$participant_id, weight.bs$participant_id)]


write.csv(wt.week, "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean_wk.csv")

