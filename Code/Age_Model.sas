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

DATA wt; 
	SET wt; 
	study_days = study_days/365; 
RUN; 

PROC MIXED DATA = wt ; 
	CLASS participant_id cohort sex race(ref = "5") month; 
	MODEL wt_lb = cohort sex age race study_days month / solution ;
	REPEATED / SUBJECT = participant_id type=ar(1);
RUN; 
