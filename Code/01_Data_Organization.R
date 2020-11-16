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
wt <- wt[,c(3,4,10,12,14,55:57)]

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

write.csv(wt, "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean.csv")

