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
