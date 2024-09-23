libname KHANDLE "...";
run;
libname STAR "..."; run;
libname libref '...';
options fmtsearch=(libref.w1fmts); 
OPTIONS nofmterr;

libname TAM "..."; run;
data MHC_KH_ALL; set KHANDLE.khandle_mhc_baseline_kp_20200529;
run;

proc contents data=MHC_KH_ALL;
run;
proc contents data=KHANDLE.khandle_all_waves_kp_20220714;
run;
libname libref '...';
options fmtsearch=(libref.kndlfmts); 

proc freq data=KHANDLE.khandle_all_waves_kp_20220714;
tables W1_MATERNAL_EDUCATION_TEXT W1_MATERNAL_EDUCATION  W1_PATERNAL_EDUCATION W1_PATERNAL_EDUCATION_TEXT;
run;
proc freq data=KHANDLE.khandle_all_waves_kp_20220714;
tables W1_MATERNAL_EDUCATION_TEXT*W1_MATERNAL_EDUCATION  W1_PATERNAL_EDUCATION*W1_PATERNAL_EDUCATION_TEXT;
run;
proc freq data=KHANDLE.khandle_all_waves_kp_20220714;
tables W1_MATERNAL_EDUCATION_TEXT*W1_MATERNAL_EDUCATION;
where W1_MATERNAL_EDUCATION=0;
run;
proc contents data=KHANDLE.khandle_all_waves_kp_20220714; run;
data TEST_KH; set KHANDLE.khandle_all_waves_kp_20220714;
IF  1<=W1_MATERNAL_EDUCATION<=5 OR  1<=W1_PATERNAL_EDUCATION<=5 THEN PARENT_COL=1;
ELSE IF W1_MATERNAL_EDUCATION=0 AND W1_MATERNAL_EDUCATION_TEXT>8 THEN PARENT_COL=1;
ELSE IF W1_PATERNAL_EDUCATION=0 AND W1_PATERNAL_EDUCATION_TEXT>8 THEN PARENT_COL=1;

ELSE IF W1_MATERNAL_EDUCATION=0 AND W1_MATERNAL_EDUCATION_TEXT<=8 THEN PARENT_COL=0;
ELSE IF W1_PATERNAL_EDUCATION=0 AND W1_PATERNAL_EDUCATION_TEXT<=8 THEN PARENT_COL=0;


IF 5<W1_MATERNAL_EDUCATION AND 5<W1_PATERNAL_EDUCATION THEN PARENT_COL=0;
run;

proc print data=TEST_KH (obs=50);
var STUDYID PARENT_COL W1_MATERNAL_EDUCATION W1_MATERNAL_EDUCATION_TEXT W1_PATERNAL_EDUCATION W1_PATERNAL_EDUCATION_TEXT;
run;
data TEST_KH1; set TEST_KH;
drop W1_EMP_RETIRED_AGE;
run;
proc freq data=MHC_KH_ALL;
tables MHC_DIASTOLIC MHC_SYSTOLIC / missing;
run;
data MHC_KH_TEST; set MHC_KH_ALL;
MHC_YR=year(MHC_DATE);

IF MHC_SYSTOLIC>=140 OR  MHC_DIASTOLIC>=90 THEN MHC_BP=3; /*Hypertensive */
ELSE IF 120<=MHC_SYSTOLIC<140 OR  80<=MHC_DIASTOLIC<90 THEN MHC_BP=2;/*Prehypertensive */
ELSE IF MHC_SYSTOLIC<80 OR  MHC_DIASTOLIC<60 THEN MHC_BP=1; /*Hypotensive */
ELSE IF 80<=MHC_SYSTOLIC<120  AND 60<=MHC_DIASTOLIC<80 THEN MHC_BP=0; /*Normotensive */

IF MHC_BP=3 THEN MHC_HYP=1;
IF MHC_BP<3 then MHC_HYP=0;
TIMEFROM425=abs(MHC_EXAM_AGE-42.5);
STUDY="KHANDLE";
where 35<=MHC_EXAM_AGE<=50 AND MHC_DIASTOLIC ne .M AND MHC_DIASTOLIC>0 AND MHC_SYSTOLIC ne .M AND MHC_SYSTOLIC>0;
run;


proc sort data=MHC_KH_TEST; by STUDYID TIMEFROM425;
run;
proc print data=MHC_KH_TEST (obs=10);
var STUDYID TIMEFROM425; run;
proc sort data=MHC_KH_TEST out=MHC_KH_TEST_S nodupkey; by STUDYID;
run;
proc print data=MHC_KH_TEST_S (obs=10);
var STUDYID TIMEFROM425; run;
proc sql;
	create table MHC_KH_WAVE as
	select a.*, b.*
	from  MHC_KH_TEST_S a
	INNER join TEST_KH1 b
		on a.STUDYID=B.STUDYID 
	;
	quit; 

LIBNAME fmt '...';

OPTIONS FMTSEARCH = (fmt.starfmts);


data MHC_STAR_ALL; set STAR.star_mhc_baseline_kp_20220217;
run;

proc contents data=MHC_STAR_ALL;
run;

proc print data=MHC_STAR_ALL (obs=10);
run;

data MHC_STAR_ALL_T; set MHC_STAR_ALL;
keep STUDYID MHC_DIASTOLIC MHC_SYSTOLIC MHC_EXAM_AGE MHC_DATE; 
where MHC_DIASTOLIC ne .M AND MHC_SYSTOLIC ne .M AND MHC_DIASTOLIC >0 AND MHC_SYSTOLIC >0;
run;


data TEST_STAR; set STAR.star_all_waves_kp_20220309;
IF  1<=W1_MATERNAL_EDUCATION<=5 OR  1<=W1_PATERNAL_EDUCATION<=5 THEN PARENT_COL=1;
ELSE IF W1_MATERNAL_EDUCATION=0 AND W1_MATERNAL_EDUCATION_TEXT>8 THEN PARENT_COL=1;
ELSE IF W1_PATERNAL_EDUCATION=0 AND W1_PATERNAL_EDUCATION_TEXT>8 THEN PARENT_COL=1;

ELSE IF W1_MATERNAL_EDUCATION=0 AND W1_MATERNAL_EDUCATION_TEXT<=8 THEN PARENT_COL=0;
ELSE IF W1_PATERNAL_EDUCATION=0 AND W1_PATERNAL_EDUCATION_TEXT<=8 THEN PARENT_COL=0;


IF 5<W1_MATERNAL_EDUCATION AND 5<W1_PATERNAL_EDUCATION THEN PARENT_COL=0;
run;
data TEST_STAR1; set TEST_STAR;
drop W1_EMP_RETIRED_AGE;
run;
proc freq data=TEST_STAR;
tables PARENT_COL;
run;

data MHC_STAR_3550; set MHC_STAR_ALL_T;
MHC_YR=year(MHC_DATE);

IF MHC_SYSTOLIC>=140 OR  MHC_DIASTOLIC>=90 THEN MHC_BP=3; /*Hypertensive */
ELSE IF 120<=MHC_SYSTOLIC<140 OR  80<=MHC_DIASTOLIC<90 THEN MHC_BP=2;/*Prehypertensive */
ELSE IF MHC_SYSTOLIC<80 OR  MHC_DIASTOLIC<60 THEN MHC_BP=1; /*Hypotensive */
ELSE IF 80<=MHC_SYSTOLIC<120  AND 60<=MHC_DIASTOLIC<80 THEN MHC_BP=0; /*Normotensive */

IF MHC_BP=3 THEN MHC_HYP=1;
IF MHC_BP<3 then MHC_HYP=0;
TIMEFROM425=abs(MHC_EXAM_AGE-42.5);
STUDY="STAR";
label MHC_BP="MHC Blood Pressure, 0=Normotensive|1=Hyotensive|2=Prehypertensive|3=Hypertensive";
where 35<=MHC_EXAM_AGE<=50;
run;
proc sort data=MHC_STAR_3550 ; by STUDYID TIMEFROM425;
run;
proc print data=MHC_STAR_3550 (obs=50);
run;
proc sort data=MHC_STAR_3550 out=MHC_STAR_425 nodupkey; by STUDYID;
run;
proc print data=MHC_STAR_425 (obs=50);
run;

proc sql;
	create table MHC_STAR_WAVE as
	select a.*, b.*
	from  MHC_STAR_425 a
	INNER join TEST_STAR1 b
		on a.STUDYID=B.STUDYID 
	;
	quit; 

data MHC_KHSTAR; set MHC_STAR_WAVE MHC_KH_WAVE;
run;
proc means data=MHC_KHSTAR n mean std;
var W1_SENAS_EXEC;
output out=EXEC 
mean=W1_EXEC_mean
std=W1_EXEC_std
;
run;
proc means data=MHC_KHSTAR n mean std;
var W1_SENAS_SEM;
output out=SEM 
mean=W1_SEM_mean
std=W1_SEM_std
;
run;

proc means data=MHC_KHSTAR n mean std;
var W1_SENAS_VRMEM;
output out=VRMEM 
mean=W1_VRMEM_mean
std=W1_VRMEM_std
;
run;

proc sql;
	create table MHC_KHSTAR1 as
	select a.*, b.W1_EXEC_MEAN, b.W1_EXEC_STD, c.W1_SEM_MEAN, c.W1_SEM_STD, d.W1_VRMEM_MEAN, d.W1_VRMEM_STD
	from  MHC_KHSTAR as a, EXEC as b, SEM as c, VRMEM as d
	
	;
	quit;

proc print data=MHC_KHSTAR1 (obs=10); run;
proc contents data=MHC_KHSTAR; run;
data MHC_KHSTAR2; set MHC_KHSTAR1;


W1_SENAS_EXEC_Z=round( ((W1_SENAS_EXEC - W1_EXEC_MEAN)/W1_EXEC_STD), .0001);
W1_SENAS_SEM_Z=round( ((W1_SENAS_SEM - W1_SEM_MEAN)/W1_SEM_STD), .0001);
W1_SENAS_VRMEM_Z=round( ((W1_SENAS_VRMEM - W1_VRMEM_MEAN)/W1_VRMEM_STD), .0001);

W2_SENAS_EXEC_Z=round( ((W2_SENAS_EXEC - W1_EXEC_MEAN)/W1_EXEC_STD), .0001);
W2_SENAS_SEM_Z=round( ((W2_SENAS_SEM - W1_SEM_MEAN)/W1_SEM_STD), .0001);
W2_SENAS_VRMEM_Z=round( ((W2_SENAS_VRMEM - W1_VRMEM_MEAN)/W1_VRMEM_STD), .0001);

W3_SENAS_EXEC_Z=round( ((W3_SENAS_EXEC - W1_EXEC_MEAN)/W1_EXEC_STD), .0001);
W3_SENAS_SEM_Z=round( ((W3_SENAS_SEM - W1_SEM_MEAN)/W1_SEM_STD), .0001);
W3_SENAS_VRMEM_Z=round( ((W3_SENAS_VRMEM - W1_VRMEM_MEAN)/W1_VRMEM_STD), .0001);

IF STUDY="KHANDLE" THEN STUDYIDN=cat("KH_", STUDYID);
IF STUDYID="STAR" THEN STUDYIDN=cat("ST_", STUDYID);
run;
proc freq data=MHC_KHSTAR2;
tables STUDYIDN STUDYID; run;

proc sort data=MHC_KHSTAR2 out=TEST nodupkey; by STUDYID; run;
libname TAMARE "..."; run;

data TAMARE.MHC_KHSTAR; set MHC_KHSTAR2; 
run;

proc freq data=MHC_KHSTAR2;
tables PARENT_COL*MHC_HYP / missing;
run;

proc freq data=MHC_KHSTAR2;
tables W1_INTERVIEW_AGE_PHI; run;

proc contents data=MHC_KHSTAR2; run;
