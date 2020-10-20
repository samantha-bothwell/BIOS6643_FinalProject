#####################################################
####             BIOS 6643 Project               ####
####        Longitudinal Data Analysis           #### 
#### Aim : Visualize daily weight data through   ####
####       Functional Data Analysis              ####
#####################################################


##### Visuals for 
#####    1. Individuals over time in study 
#####    2. Individuals over month in study

#### Load in the data 
wt <- read.csv("D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_with_confounding.csv")


#### Plot data x-axis: study_days, y-axis:  wt_lb, group by: participant_id
library(ggplot2)

ggplot(data = wt, aes(x = study_days, y = wt_lb, group = participant_id, color = as.factor(cohort))) + 
  geom_line()

