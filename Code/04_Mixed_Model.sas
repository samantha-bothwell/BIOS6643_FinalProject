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

PROC IMPORT DATAFILE = "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean_blage.csv"
	OUT = wt_bl
	DBMS = csv; 
RUN;

PROC IMPORT DATAFILE = "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_cat_clean.csv"
	OUT = wt_cat
	DBMS = csv; 
RUN;

PROC PRINT DATA = wt; 
RUN; 

DATA wt_cat; 
	SET wt_cat; 
	study_days = study_days/365; 
RUN; 

** Models **;
/* When treating baseline age as categorical we have the lowest model AIC */
/* AIC = 60727.9, OBS Used 15452 */
PROC MIXED DATA = wt_cat; 
	CLASS participant_id cohort sex race(ref = "5") age; 
	MODEL wt_lb = cohort sex age race study_days study_days*study_days / solution outp = pred;
	RANDOM INTERCEPT / SUBJECT = participant_id; 
	RANDOM study_days / SUBJECT = participant_id;
	RANDOM study_days*study_days / SUBJECT = participant_id;  
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN;


/* Best model ignoring age */
/* AIC = 60749.4, OBS Used 15452 */
PROC MIXED DATA = wt_bl; * plots(MAXPOINTS=none)=ALL; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN; 


/* Best model treating age as continuous*/
/* AIC = 61590.6, OBS Used 15452 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_impute = cohort sex age race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN; 

PROC PRINT DATA = wt; 
RUN; 
*********
When we use a daily updated age, 
	AGE ESTIMATE = -0.4817, AGE PVAL = 0.1810
	STUDY DAYS ESTIMATE = 0.008981, STUDY DAYS PVAL = 0.0550
When we use baseline age, 
	AGE ESTIMATE = -0.4817, AGE PVAL = 0.1847
	STUDY DAYS ESTIMATE = 0.007661, STUDY DAYS PVAL = 0.0943
*********; 






/* AIC = 61608.7 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = sex age race study_days month / solution ;
	REPEATED / SUBJECT = participant_id(cohort) type=ar(1);
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


/* Baseline age AIC = 97794.7, Updated age AIC = 97794.7 */
PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex age race study_days month / solution;
	RANDOM intercept / SUBJECT = participant_id(cohort) type=ar(1);
RUN; 


PROC IMPORT DATAFILE = "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean_wk.csv"
	OUT = wt_week
	DBMS = csv; 
RUN;
PROC CONTENTS DATA = wt_week; 
RUN; 

DATA wt_week; 
	SET wt_week; 
	weight_change = mean_wt - weight_bs; 
RUN; 


/* 13162.9 */
PROC MIXED DATA = wt_week ; 
	CLASS participant_id cohort sex race(ref = "5"); 
	MODEL weight_change = cohort sex age race Week Week*Week cohort*Week / solution outp = pred1;
	RANDOM INTERCEPT / subject = participant_id; 
	RANDOM Week / subject = participant_id;  
/*	RANDOM month / subject = participant_id; */
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN; 

/* 13160.2 */
PROC MIXED DATA = wt_week ; 
	CLASS participant_id cohort sex race(ref = "5"); 
	MODEL mean_wt = weight_bs cohort sex age race Week Week*Week / solution outp = pred2;
	RANDOM INTERCEPT / subject = participant_id; 
	RANDOM Week / subject = participant_id;  
/*	RANDOM month / subject = participant_id; */
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN; 

PROC PRINT DATA = pred; 
RUN; 

/* 13162.9 */

PROC SGPLOT DATA = pred1; 
   title 'Predicted Weights over Study Weeks';
   series x=Week y=pred / group=participant_id;
run;



PROC IMPORT DATAFILE = "D:/CU/Fall 2020/BIOS 6643/Project/BIOS6643_FinalProject/DataProcessed/daily_weights_clean_wk_cat.csv"
	OUT = wt_cat_week
	DBMS = csv; 
RUN;

PROC PRINT DATA = wt_cat_week; 
RUN;

PROC MIXED DATA = wt_cat_week ; 
	CLASS participant_id cohort sex race(ref = "5") age; 
	MODEL mean_wt = cohort sex age race Week Week*Week / solution outp = pred;
	RANDOM INTERCEPT / subject = participant_id; 
/*	RANDOM Week / subject = participant_id;  */
/*	RANDOM month / subject = participant_id; */
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN; 
