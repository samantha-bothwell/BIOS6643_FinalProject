**************************************************
***           BIOS 6643 Project Code           ***
***  Author : Samantha Bothwell                ***
***  Professor : Matt Strand                   ***
**************************************************

** Load in the data **; 
PROC IMPORT DATAFILE = "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean.csv"
	OUT = wt
	DBMS = csv; 
RUN;

PROC IMPORT DATAFILE = "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_cat_clean.csv"
	OUT = wt_cat
	DBMS = csv; 
RUN;

PROC PRINT DATA = wt_cat; 
RUN; 

** Models **;
/* AIC = 60727.9 */
PROC MIXED DATA = wt_cat ; 
	CLASS participant_id cohort sex race(ref = "5") month age; 
	MODEL wt_lb = cohort sex age race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN;



/* AIC = 97794.7 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex age race study_days month / solution;
	RANDOM intercept / SUBJECT = participant_id(cohort) type=ar(1);
RUN;  
 
/* AIC = 61608.7 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = sex age race study_days month / solution ;
	REPEATED / SUBJECT = participant_id(cohort) type=ar(1);
RUN;  

/* AIC = 61590.6 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex age race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN;

/* AIC = 60749.4 */
PROC MIXED DATA = wt; * plots(MAXPOINTS=none)=ALL; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN;   

DATA wt; 
	SET wt; 
	age_sq = age*age; 
RUN; 

PROC PRINT DATA = wt; 
RUN; 

/* AIC = 61595.0 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex age age_sq race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN;
