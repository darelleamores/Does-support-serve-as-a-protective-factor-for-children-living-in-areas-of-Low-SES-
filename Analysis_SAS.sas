*Run Cleaned_combined_all3*;
PROC IMPORT OUT= WORK.master 
            DATAFILE= "C:\Users\Darelle\Dropbox\SCHOOL\LLU\Research\ICP\Data\Cleaned_combined_all3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

*Get the means*; 
proc means; run;

*Correlation*;
proc corr;
var 
Home_support_agg 
Outside_support_agg 
School_support_agg 
;
run;

*******************************frequencies***********************************************************************************************;
*gender and ela*; 
proc freq data = work.master;
tables stu_gender * sbac_ela_level2cat;
run;
*gender and math*; 
proc freq data = work.master;
tables stu_gender * sbac_math_level2cat;
run;

*race and ela*; 
proc freq data = work.master;
tables stu_ethnicity_group * sbac_ela_level2cat;
run;
*race and math*; 
proc freq data = work.master;
tables stu_ethnicity_group * sbac_math_level2cat;
run;

*lowSES*; 
proc freq data = work.master;
tables stu_lowses_status;
run;
*lowSES and ela*; 
proc freq data = work.master;
tables stu_lowses_status * sbac_ela_level2cat;
run;
*lowSES and math*; 
proc freq data = work.master;
tables stu_lowses_status * sbac_math_level2cat;
run;


*grade; 
proc freq data = work.master;
tables stu_grade;
run;
*lowSES and ela*; 
proc freq data = work.master;
tables stu_grade * sbac_ela_level2cat;
run;
*lowSES and math*; 
proc freq data = work.master;
tables stu_grade * sbac_math_level2cat;
run;

*all students*;
proc freq data = work.master;
tables  sbac_ela_level2cat;
run;

proc freq data = work.master;
tables  sbac_math_level2cat;
run;

proc freq;
table stu_zip_code_cmb*stu_lowses_status;
run;

proc freq;
table stu_zip_code_cmb*sbac_ela_level2cat;
run;


*Model 1 with binary support aggregate data for English*; 

*references chosen 
gender = female, females do better at tests according to descriptive data
race   = whites, white do better at test according to research and descriptive data
lowSES = No, individuals who are not in the low SES spectrum do better according to research and descriptive data
grade = 11, grade 11 had the strongest effect in OR when not used as referece, such this is chosen as the reference group*; 


proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_ela_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;

*Model 1 with binary support aggregate data for Math*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;

*Model 1 with binary support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;





*Just a reminder for this week, work on running the regressions again but per grade level, * use "where grade=4;

*grade 4;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where stu_grade = 4;
run;

*grade 5;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where stu_grade = 5;
run;


*grade 6;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where stu_grade = 6;
run;



*grade 7;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where stu_grade = 7;
run;


*grade 8;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where stu_grade = 8;
run;


*grade 11;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_supportCat2_agg2 Outside_supportCat2_agg2 School_supportCat2_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where stu_grade = 11;
run;





**********************************************************************************************************************************************************************************;
**********************************************************************************************************************************************************************************;
**********************************************************************************************************************************************************************************;
****************************************************************SHAVLIK***********************************************************************************************************;
**********************************************************************************************************************************************************************************;

proc print data = work.master (obs=20);
run;

*collapsing continous variables 
*Home support*;
data work.master2;
set work.master;
if 0   <=  home_support_agg     <= 6      then home_supportcl      = 1;
if 7   <=  home_support_agg     <= 8      then home_supportcl      = 2;
if 9   <=  home_support_agg     <= 10     then home_supportcl      = 3;
if 11  <=  home_support_agg     <= 12     then home_supportcl      = 4;

if 0   <=  outside_support_agg  <= 6      then outside_supportcl   = 1;
if 7   <=  outside_support_agg  <= 8      then outside_supportcl   = 2;
if 9   <=  outside_support_agg  <= 10     then  outside_supportcl  = 3;
if 11  <=  outside_support_agg  <= 12     then  outside_supportcl  = 4;

if 4   <=  school_support_agg   <= 8      then school_supportcl    = 1;
if 9   <=  school_support_agg   <= 10     then school_supportcl    = 2;
if 11  <=  school_support_agg   <= 12     then school_supportcl    = 3;
if 13  <=  school_support_agg   <= 14     then school_supportcl    = 4;
if 15  <=  school_support_agg   <= 16     then school_supportcl    = 5;

if 9.7 <=    ageatstudy         <12        then agegroup            = 11;
if 12  <=    ageatstudy         <14        then agegroup            = 13;
if 14  <=    ageatstudy         <16        then agegroup            = 15;
if 16  <=    ageatstudy         <18        then agegroup            = 17;
if 18  <=    ageatstudy         <= 20      then agegroup            = 19;
sqAge = sqrt(ageatstudy);
run; 

proc print data = work.master2 (obs=50);
run;

proc freq;
tables agegroup;
run;




*Model 1 with Continous support aggregate data for English*; 
*collinear behavior*;
*run grades as a seperate model without age*; 
*model 1 with age and model 1 with no grade*;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') stu_lowses_status (ref='N') stu_grade/param=ref ref=first;
model sbac_ela_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status / corrb;
run;


*Model 1b with collapsed continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_ela_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;

*Model 1c Final English: With Agegroup. No grades;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade agegroup/param=ref ref= first;
model sbac_ela_level2cat =  home_supportcl outside_supportcl school_supportcl 
              agegroup stu_gender stu_ethnicity_group stu_lowses_status ;
run;
*Model 1c Final English: With Grades. No Agegroup;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade agegroup/param=ref ref= first;
model sbac_ela_level2cat =  home_supportcl outside_supportcl school_supportcl 
               stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;





proc freq;
table  Home_support_agg Outside_support_agg School_support_agg;
run;

**********************************************MATH******************************************************************************************************************************;
**********************************************MATH*****************************************************************************************************************************;


*model 1c continous with age and model 1 with no grade*;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') stu_lowses_status (ref='N') stu_grade/param=ref ref=first;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status / corrb;
run;

*Model 1c Final Math: With Agegroup. No grades;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade agegroup/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              agegroup stu_gender stu_ethnicity_group stu_lowses_status ;
run;
*Model 1c Final Math: With Grades. No Agegroup;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group (ref='White') home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade agegroup/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
               stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;





*Model 2 with collapsed continous support aggregate data for Math*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;




************MODEL 3 with collapsed age variable - agegroup in the model MATH ******************************************************;
*age group 9.8 - 11.99 AGEGROUP = 1 
*Model 3 with Continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=1;
run;


*Model 3 with collapsed continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=1;
run;


*age group 12 - 13.99 AGEGROUP = 2 
*Model 3 with Continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=2;
run;


*Model 3 with collapsed continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=2;
run;


*age group 14 - 15.99 AGEGROUP = 3 
*Model 3 with Continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=3;
run;


*Model 3 with collapsed continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=3;
run;


*age group 16 - 17.99 AGEGROUP = 4
*Model 3 with Continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=4;
run;


*Model 3 with collapsed continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=4;
run;


*age group 18 and above AGEGROUP = 5
*Model 3 with Continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=5;
run;


*Model 3 with collapsed continous support aggregate data for English*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              ageatstudy stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
where agegroup=5;
run;


*Model 4 with continous variable for English with Squared aged term Math*;
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group stu_lowses_status (ref='N') stu_grade/param=ref;
model sbac_math_level2cat = Home_support_agg Outside_support_agg School_support_agg   
              sqAge stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;


*Model 4 with collapsed continous variable for Math with Squared aged term Math*; 
proc logistic descending;
class stu_gender (ref='F') stu_ethnicity_group home_supportcl outside_supportcl school_supportcl stu_lowses_status (ref='N') stu_grade/param=ref ref= first;
model sbac_math_level2cat =  home_supportcl outside_supportcl school_supportcl 
              sqAge stu_gender stu_ethnicity_group stu_lowses_status stu_grade;
run;
