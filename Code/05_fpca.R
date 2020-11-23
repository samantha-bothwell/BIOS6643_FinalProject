#####################################################
####             BIOS 6643 Project               ####
####        Longitudinal Data Analysis           #### 
#### Aim : Apply FPCA to the analysis            ####
#####################################################

library(refund)
library(reshape2)
library(reshape)
library(tidyr)

wt <- read.csv("D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean.csv")
wt <- wt[!(is.na(wt$participant_id)),]

wt <- wt %>% 
  group_by(participant_id) %>% 
  complete(study_days = seq(1, 365, 1)) %>% 
  mutate(cohort = min(cohort, na.rm = T), sex = min(sex, na.rm = T), race = min(race, na.rm = T),
    Day1 = min(as.Date(weight_dates), na.rm = T))

wt <- wt[!duplicated(wt[,c(1,2)]),]

cohort.list <- wt %>% 
  group_by(participant_id) %>% 
  summarise(cohort = max(cohort))

age.list <- wt %>% 
  group_by(participant_id) %>% 
  slice(1)

age.list$age <- ifelse(age.list$age <= 30, "20-30", 
                ifelse(age.list$age <= 40, "30-40", 
                ifelse(age.list$age <= 50, "40-50", "50-60")))

wt$age <- ifelse(wt$age <= 30, "20-30", 
  ifelse(wt$age <= 40, "30-40", 
    ifelse(wt$age <= 50, "40-50", "50-60")))


# wt$study_days <- as.factor(wt$study_days)

wt.lb <- cast(wt[,c(1,2,6)], participant_id ~ study_days)

wt.lb <- as.matrix(wt.lb[,-1])

wt_fpca = fpca.sc(wt.lb)

# plot(wt_fpca$Y, wt_fpca$Yhat)

# refund.shiny::plot_shiny(wt_fpca)

colnames(wt.lb) = paste0("time_", 1:365)
# How do you plot both predicted and actual
# Use two matrices? 
wt.act <- as_tibble(wt.lb) %>%
  mutate(id = row_number(), cohort = cohort.list$cohort, age = age.list$age) %>%
  gather(time, value.act, contains("time_")) %>%
  mutate(time = str_remove(time, "time_"),
    time = as.numeric(time)) # %>%
 #  filter(id %in% (1:10)) 

wt.pred <- as_tibble(wt_fpca$Yhat) %>%
  mutate(id = row_number(), cohort = cohort.list$cohort, age = age.list$age) %>%
  gather(time, value.pred, contains("V")) %>%
  mutate(time = str_remove(time, "V"),
    time = as.numeric(time)) # %>%
 #  filter(id %in% (1:10)) 

wt.compare <- cbind(wt.act, wt.pred$value.pred)
colnames(wt.compare)[6] <- "value.pred"

ggplot(data = wt.compare[!is.na(wt.compare$value.act),], aes(time, value.act, group = id)) +
  geom_point(alpha = 0.2, color = "#32ad9d") +
  geom_path(alpha = 0.2, color = "#32ad9d") +
  geom_point(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = id), alpha = 0.5, color = "#32ad9d", size = 0.75) + 
  geom_path(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = id), alpha = 0.5, color = "#32ad9d", size = 1) + 
  facet_grid(. ~ cohort) + 
  geom_smooth(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = cohort), lwd = 2, color = 'black')



# remove KBU-082
wt.compare <- wt.compare[!(wt.compare$id == 40),]

ggplot(data = wt.compare[!is.na(wt.compare$value.act),], aes(time, value.act, group = id)) +
  geom_point(alpha = 0.2, color = "#32ad9d") +
  geom_path(alpha = 0.2, color = "#32ad9d") +
  geom_point(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = id), alpha = 0.5, color = "#32ad9d", size = 0.75) + 
  geom_path(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = id), alpha = 0.5, color = "#32ad9d", size = 1) + 
  facet_grid(. ~ age) + 
  geom_smooth(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = age), lwd = 2, color = 'black')


