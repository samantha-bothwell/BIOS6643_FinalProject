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

PROC PRINT DATA = wt; 
RUN; 

** Model **; 
PROC MIXED DATA = wt; 
	CLASS participant_id cohort sex race season month; 
	MODEL wt_lb = cohort sex age race study_days season month / solution;
	REPEATED cohort / subject=participant_id type=ar(1);
RUN;  
