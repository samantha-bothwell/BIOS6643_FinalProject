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

# wt$study_days <- as.factor(wt$study_days)

wt.lb <- cast(wt[,c(1,2,6)], participant_id ~ study_days)

wt.lb <- as.matrix(wt.lb[,-1])

wt_fpca = fpca.sc(wt.lb)

# plot(wt_fpca$Y, wt_fpca$Yhat)

refund.shiny::plot_shiny(wt_fpca)

colnames(wt.lb) = paste0("time_", 1:365)
# How do you plot both predicted and actual
# Use two matrices? 
wt.act <- as_tibble(wt.lb) %>%
  mutate(id = row_number(), cohort = cohort.list$cohort) %>%
  gather(time, value.act, contains("time_")) %>%
  mutate(time = str_remove(time, "time_"),
    time = as.numeric(time)) # %>%
 #  filter(id %in% (1:10)) 

wt.pred <- as_tibble(wt_fpca$Yhat) %>%
  mutate(id = row_number(), cohort = cohort.list$cohort) %>%
  gather(time, value.pred, contains("V")) %>%
  mutate(time = str_remove(time, "V"),
    time = as.numeric(time)) # %>%
 #  filter(id %in% (1:10)) 

wt.compare <- cbind(wt.act, wt.pred$value.pred)
colnames(wt.compare)[5] <- "value.pred"

ggplot(data = wt.compare[!is.na(wt.compare$value.act),], aes(time, value.act, group = id)) +
  geom_point(alpha = 0.2, color = "#32ad9d") +
  geom_path(alpha = 0.2, color = "#32ad9d") +
  geom_point(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = id), alpha = 0.5, color = "#32ad9d") + 
  geom_path(data = wt.compare[!is.na(wt.compare$value.pred),], aes(time, value.pred, group = id), alpha = 0.5, color = "#32ad9d", lwd = 0.25) + 
  facet_grid(. ~ cohort)


ggplot(data = wt.compare[!is.na(wt.compare$value.act),], aes(time, value.pred, group = id)) +
  geom_point(alpha = 0.2, color = "#32ad9d") +
  geom_path(alpha = 0.2, color = "#32ad9d") +
 #  stat_summary(fun = mean, geom="line",lwd=2,aes(group=1)) + 
  facet_grid(. ~ cohort)


