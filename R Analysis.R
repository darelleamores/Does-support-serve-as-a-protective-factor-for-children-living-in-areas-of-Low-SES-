#packages
{
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
#install.packages('gmodels')
library(gmodels)
library(lubridate)
#install.packages("oddsratio")
}

setwd("C:\\Users\\darelle\\Dropbox\\SCHOOL\\LLU\\Research\\ICP\\Data")
#SBCUSD - CAASP 2018 results
caaspsb <-readxl::read_xlsx("SBCUSD - CAASPP 2018 Results - with zipcode - 20180925.xlsx")

#SBCUSD + student survey 
sbsurvey <-readxl::read_xlsx("sbcusd+San Bernardino Unified Student Survey 2019-01-25 15_08_00 UTC.xlsx")

#COMPARE between two datasets 
#calculating frequency table for CAASP - RACE
caaspsb %>% 
  group_by(stu_ethnicity_group) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

table(caaspsb$stu_ethnicity_group)
prop.table(table(caaspsb$stu_ethnicity_group))*100



#calculating frequency table for sbsurvey - RACE
table(sbsurvey$`Student Race`)
prop.table(table(sbsurvey$`Student Race`))*100

#calculating frequency table for CAASP - GENDER, need to clean
levels_gender = c("m"="M","f" ="F")
caaspsb$stu_gender%<>%revalue(levels_gender)

table(caaspsb$stu_gender)
prop.table(table(caaspsb$stu_gender))*100


#calculating frequency table for sbsurvey - GENDER
table(sbsurvey$`Student Gender`)
prop.table(table(caaspsb$stu_gender))*100


#Freq zipcode - CAASP
caaspsb %>% 
  group_by(zip_code) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

#Freq zipcode - SBSURVEY
sbsurvey %>% 
  group_by(stu_zip_code) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

#Freq lowincome status - CAASP
caaspsb %>% 
  group_by(stu_lowses_status) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))



CrossTable(caaspsb$zip_code, caaspsb$stu_lowses_status)

#Freq lowincome status - SBSURVEY 
caaspsb %>% 
  group_by(stu_lowses_status) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

#homelessstatus - CAASP
table(caaspsb$homeless_status)
prop.table(table(caaspsb$homeless_status))*100

#ELA - SCore met
table(caaspsb$sbac_ela_meets_exceeds_standards)
prop.table(table(caaspsb$sbac_ela_meets_exceeds_standards))*100

CrossTable(caaspsb$sbac_ela_meets_exceeds_standards, caaspsb$stu_lowses_status ,digits=2, 
expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=TRUE, sresid=TRUE)

CrossTable(caaspsb$sbac_ela_meets_exceeds_standards, caaspsb$homeless_status, digits=2, 
expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=TRUE, sresid=TRUE)

CrossTable(caaspsb$sbac_ela_claim_1_description,caaspsb$stu_lowses_status)

#DATA CLEANING  #02/24
#Merge CAASP data with other dataset
#clean ID name in sbsurvey to match caaspsb dataset
names(sbsurvey)[1]  <- "llu_study_id"
combined <-inner_join(caaspsb, sbsurvey, by = "llu_study_id")
#check for duplicates 
#sbsurvey$llu_study_id %>% duplicated %>% table()


#categorize hispanic, african american, white, and other minorities 
table(combined$stu_ethnicity_group) #check races in combined data
levels_racesb = c("Two or More Races"="Other Minorities","Pacific Islander"="Other Minorities", "Asian"="Other Minorities", 
                  "Filipino"="Other Minorities","American Indian"="Other Minorities")
combined$stu_ethnicity_group%<>%revalue(levels_racesb)

#check new categorized variables 
table(combined$stu_ethnicity_group)
prop.table(table(combined$stu_ethnicity_group))*100

#Catorgorized ELA and Math Level - "Met the Standard" (3 and 4) and " Not met the standard" (1 and 2)
#first change description for English and Math 


levels_caaspdesc = c("Standard Exceeded"= "Standard Met", "Standard Nearly Met"= "Standard Not Met")
combined$sbac_ela_level_description2 = revalue(combined$sbac_ela_level_description,levels_caaspdesc)

combined$sbac_math_level_description2 = revalue(combined$sbac_math_level_description,levels_caaspdesc)





#check freq of categorized description
table(combined$sbac_ela_level_description)
table(combined$sbac_math_level_description)

#check freq of new categorized description - binary
table(combined$sbac_ela_level_description2)
table(combined$sbac_math_level_description2)

#######################recoding for binary##################################################################################### 
#changing 3 and 4 = Standard Met -->1 , #now changing 1 and 2 =standard not met -->2 for binary

#check freq of originals 
table(combined$sbac_ela_level)
prop.table(table(combined$sbac_ela_level))*100 #english
table(combined$sbac_math_level)
prop.table(table(combined$sbac_math_level))*100 #math

combined %<>% mutate(sbac_ela_level2cat = case_when(
  sbac_ela_level == 1 ~ 0,
  sbac_ela_level == 2 ~ 0,
  sbac_ela_level == 3 ~ 1,
  sbac_ela_level == 4 ~ 1,
  TRUE ~ NA_real_
))

table(combined$sbac_ela_level2cat)
prop.table(table(combined$sbac_ela_level2cat))*100

combined %<>% mutate(sbac_math_level2cat = case_when(
  sbac_math_level == 1 ~ 0,
  sbac_math_level == 2 ~ 0,
  sbac_math_level == 3 ~ 1,
  sbac_math_level == 4 ~ 1,
  TRUE ~ NA_real_
))

table(combined$sbac_math_level2cat)
prop.table(table(combined$sbac_math_level2cat))*100



#another way to do it (my preference)
#combined %<>% mutate(sbac_ela_level_met  = if_else(sbac_ela_level  >= 3, 1, 0, NA_real_))
#combined %<>% mutate(sbac_math_level_met = if_else(sbac_math_level >= 3, 1, 0, NA_real_))

# compare old with new column
#combined %$% table(sbac_ela_level, sbac_ela_level_met)
#combined %$% table(sbac_math_level, sbac_math_level_met)

#combined %>% select(sbac_ela_level, sbac_ela_level2cat) %>% View


#levels_caaspcat = c("3" = "1", "4" = "1", 
#                    "1"="2","2"="2")

#combined$sbac_ela_level%<>%as.numeric(revalue(levels_caaspcat))


#combined$sbac_math_level%<>%as.numeric(revalue(levels_caaspcat))

#Age calculation using lubridate package 
combined %<>% mutate(dob = lubridate::as_date(stu_dob %>% as.character, tz="UTC", format="%Y%m%d" ))
combined %<>% mutate(studydate = `Response Start Time` %>% as_date)
combined %<>% mutate(ageatstudy = ((studydate - dob)/365.25) %>% round(1) %>% as.numeric)

mean(combined$ageatstudy)
min(combined$ageatstudy)
max(combined$ageatstudy)



#run frequencies on merged data
table(caaspsb$homeless_status)
prop.table(table(caaspsb$homeless_status))*100


#gender combined data
levels_gender = c("m"="M","f" ="F")
combined$stu_gender%<>%revalue(levels_gender)

table(combined$stu_gender)
prop.table(table(combined$stu_gender))*100

#SES combined data
table(combined$stu_lowses_status)
prop.table(table(combined$stu_lowses_status))*100

#####################DESCRIPTIVES############################################################################################
#all students
summarize(combined)

#age

min(combined$ageatstudy)
max(combined$ageatstudy)
sd(combined$ageatstudy)
mean(combined$ageatstudy)

#Gender
table(combined$stu_gender)
prop.table(table(combined$stu_gender))*100

#Binary
 
CrossTable(combined$sbac_ela_level2cat, combined$stu_gender,digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE,sresid=TRUE) #english #REMEMBER 4th row is percent


CrossTable(combined$sbac_math_level2cat, combined$stu_gender, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math #REMEMBER 4th row is percent




#Race/Ethnicity
CrossTable(combined$sbac_ela_level2cat, combined$stu_ethnicity_group, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=TRUE, sresid=TRUE) #english

#MATh and ethnicity
CrossTable(combined$sbac_math_level2cat, combined$stu_ethnicity_group,digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math



#SES and math/english


table(combined$stu_lowses_status)
prop.table(table(combined$stu_lowses_status))*100
 
CrossTable(combined$sbac_ela_level2cat, combined$stu_lowses_status, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english


CrossTable(combined$sbac_math_level2cat, combined$stu_lowses_status, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math


#Grade
CrossTable(combined$sbac_ela_level2cat, combined$stu_grade, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english


CrossTable(combined$sbac_math_level2cat, combined$stu_grade, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math

#frquency om san bernardino data - WHO is interested in my school work 
table(combined$"who is interested in my school work. TEXT" )
prop.table(table(combined$"who is interested in my school work. TEXT" ))*100

#################################prepare new combined datasets#########################################################

#read Student Culture and Climate Survey elementary Student survey
climateELE <- readr::read_csv("sbcusd+CORE 2018-2019 Student Culture and Climate Survey Elementary Student Survey.csv")
climateSEC <- readr::read_csv("sbcusd+CORE 2018-2019 Student Culture and Climate Survey Secondary Student Survey.csv")

#clean ID name in sbsurvey to match caaspsb dataset
names(climateELE)[1]  <- "llu_study_id"
names(climateSEC)[1]  <- "llu_study_id"

#now combined 
#(caaspsb, sbsurvey, by = "llu_study_id")
  
cbind(names(climateSEC), names(climateELE))

newnames <- c(
  "Do adults at school encourage you to work hard so you can be successful? TEXT"="School_support1",
  "Adults at this school encourage me to work hard so I can be successful in college or at the job I choose. TEXT"="School_support1",
  "Teachers give students a chance to take part in classroom discussions or activities. TEXT" = "School_support2",
  "Do teachers give students a chance to take part in classroom discussions or activities? TEXT" = "School_support2",
  "My teachers work hard to help me with my schoolwork when I need it. TEXT" = "School_support3",
  "Do your teachers work hard to help you with your schoolwork when you need it? TEXT" = "School_support3",
  "Teachers go out of their way to help students. TEXT" = "School_support4",
  "Do teachers go out of their way to help students? TEXT" = "School_support4"
)

new_climateELE <- climateELE
names(new_climateELE) <- ifelse(names(new_climateELE) %in% names(newnames), newnames[names(new_climateELE)], names(new_climateELE))
new_climateELE$source = "ELE"


new_climateSEC <-climateSEC
names(new_climateSEC) <- ifelse(names(new_climateSEC) %in% names(newnames), newnames[names(new_climateSEC)], names(new_climateSEC))
new_climateSEC$source = "SEC"


combined_all <- combined %>%
  left_join(plyr::rbind.fill(new_climateELE, new_climateSEC), by="llu_study_id", suffix = c(".cmb", ".es"))

#renmame the variables to Home_support and Outside_support
newnames2 <- c(
  "who is interested in my school work. TEXT" = "Home_support1",
  "who talks with me about my problems. TEXT" = "Home_support2",
  "who listens to me when I have something to say. TEXT" = "Home_support3",
  "who really cares about me. TEXT" = "Outside_support1",
  "who notices when I am upset about something. TEXT"= "Outside_support2",
  "whom I trust. TEXT" = "Outside_support3"
)

names(combined_all) <- ifelse(names(combined_all) %in% names(newnames2), newnames2[names(combined_all)], names(combined_all))

###########################################################recoding########################################################################
#levels_caaspdesc = c("Standard Exceeded"= "Standard Met", "Standard Nearly Met"= "Standard Not Met")
#combined$sbac_ela_level_description2 = revalue(combined$sbac_ela_level_description,levels_caaspdesc)

#CAT2
#homesupport1 
combined_all %<>% mutate(Home_support1_cat2 = case_when(
  Home_support1 == "Not at all true" ~ 0,
  Home_support1 == "A little true" ~ 0,
  Home_support1 == "Pretty much true" ~ 1,
  Home_support1 == "Very much true" ~ 1,
  TRUE ~ NA_real_
))

#homesupport2
combined_all %<>% mutate(Home_support2_cat2 = case_when(
  Home_support2 == "Not at all true" ~ 0,
  Home_support2 == "A little true" ~ 0,
  Home_support2 == "Pretty much true" ~ 1,
  Home_support2 == "Very much true" ~ 1,
  TRUE ~ NA_real_
))

#homesupport3
combined_all %<>% mutate(Home_support3_cat2 = case_when(
  Home_support3 == "Not at all true" ~ 0,
  Home_support3 == "A little true" ~ 0,
  Home_support3 == "Pretty much true" ~ 1,
  Home_support3 == "Very much true" ~ 1,
  TRUE ~ NA_real_
))

#Outsupport1 
combined_all %<>% mutate(Outside_support1_cat2 = case_when(
  Outside_support1 == "Not at all true" ~ 0,
  Outside_support1 == "A little true" ~ 0,
  Outside_support1 == "Pretty much true" ~ 1,
  Outside_support1 == "Very much true" ~ 1,
  TRUE ~ NA_real_
))

#Outsupport2 
combined_all %<>% mutate(Outside_support2_cat2 = case_when(
  Outside_support2 == "Not at all true" ~ 0,
  Outside_support2 == "A little true" ~ 0,
  Outside_support2 == "Pretty much true" ~ 1,
  Outside_support2 == "Very much true" ~ 1,
  TRUE ~ NA_real_
))

#Outsupport3
combined_all %<>% mutate(Outside_support3_cat2 = case_when(
  Outside_support3 == "Not at all true" ~ 0,
  Outside_support3 == "A little true" ~ 0,
  Outside_support3 == "Pretty much true" ~ 1,
  Outside_support3 == "Very much true" ~ 1,
  TRUE ~ NA_real_
))

#Schoolsupport1 
combined_all %<>% mutate(School_support1_cat2 = case_when(
  School_support1 == "No, never" ~ 0,
  School_support1 == "Disagree" ~ 0,
  School_support1 == "Strongly Disagree" ~ 0,
  School_support1 == "Neither Disagree Nor Agree" ~ 0,
  School_support1 == "Agree" ~ 1,
  School_support1 == "Strongly Agree" ~ 1,
  School_support1 == "Yes, some of the time" ~ 1,
  School_support1 == "Yes, most of the time" ~ 1,
  School_support1 == "Yes, all of the time" ~ 1,
  TRUE ~ NA_real_
))

#Schoolsupport2
combined_all %<>% mutate(School_support2_cat2 = case_when(
  School_support2 == "No, never" ~ 0,
  School_support2 == "Disagree" ~ 0,
  School_support2 == "Strongly Disagree" ~ 0,
  School_support2 == "Neither Disagree Nor Agree" ~ 0,
  School_support2 == "Agree" ~ 1,
  School_support2 == "Strongly Agree" ~ 1,
  School_support2 == "Yes, some of the time" ~ 1,
  School_support2 == "Yes, most of the time" ~ 1,
  School_support2 == "Yes, all of the time" ~ 1,
  TRUE ~ NA_real_
))


#Schoolsupport3
combined_all %<>% mutate(School_support3_cat2 = case_when(
  School_support3 == "No, never" ~ 0,
  School_support3 == "Disagree" ~ 0,
  School_support3 == "Strongly Disagree" ~ 0,
  School_support3 == "Neither Disagree Nor Agree" ~ 0,
  School_support3 == "Agree" ~ 1,
  School_support3 == "Strongly Agree" ~ 1,
  School_support3 == "Yes, some of the time" ~ 1,
  School_support3 == "Yes, most of the time" ~ 1,
  School_support3 == "Yes, all of the time" ~ 1,
  TRUE ~ NA_real_
))

#Schoolsupport4
combined_all %<>% mutate(School_support4_cat2 = case_when(
  School_support4 == "No, never" ~ 0,
  School_support4 == "Disagree" ~ 0,
  School_support4 == "Strongly Disagree" ~ 0,
  School_support4 == "Neither Disagree Nor Agree" ~ 0,
  School_support4 == "Agree" ~ 1,
  School_support4 == "Strongly Agree" ~ 1,
  School_support4 == "Yes, some of the time" ~ 1,
  School_support4 == "Yes, most of the time" ~ 1,
  School_support4 == "Yes, all of the time" ~ 1,
  TRUE ~ NA_real_
))

#now recode Homesupport -> schoolsupport ordingal variables 

combined_all %<>% mutate(Home_support1 = case_when(
  Home_support1 == "Not at all true" ~ 1,
  Home_support1 == "A little true" ~ 2,
  Home_support1 == "Pretty much true" ~ 3,
  Home_support1 == "Very much true" ~ 4,
  TRUE ~ NA_real_
))

#homesupport2
combined_all %<>% mutate(Home_support2 = case_when(
  Home_support2 == "Not at all true" ~ 1,
  Home_support2 == "A little true" ~ 2,
  Home_support2 == "Pretty much true" ~ 3,
  Home_support2 == "Very much true" ~ 4,
  TRUE ~ NA_real_
))

#homesupport3
combined_all %<>% mutate(Home_support3 = case_when(
  Home_support3 == "Not at all true" ~ 1,
  Home_support3 == "A little true" ~ 2,
  Home_support3 == "Pretty much true" ~ 3,
  Home_support3 == "Very much true" ~ 4,
  TRUE ~ NA_real_
))

#Outsupport1 
combined_all %<>% mutate(Outside_support1 = case_when(
  Outside_support1 == "Not at all true" ~ 1,
  Outside_support1 == "A little true" ~ 2,
  Outside_support1 == "Pretty much true" ~ 3,
  Outside_support1 == "Very much true" ~ 4,
  TRUE ~ NA_real_
))

#Outsupport2 
combined_all %<>% mutate(Outside_support2 = case_when(
  Outside_support2 == "Not at all true" ~ 1,
  Outside_support2 == "A little true" ~ 2,
  Outside_support2 == "Pretty much true" ~ 3,
  Outside_support2 == "Very much true" ~ 4,
  TRUE ~ NA_real_
))

#Outsupport3
combined_all %<>% mutate(Outside_support3 = case_when(
  Outside_support3 == "Not at all true" ~ 1,
  Outside_support3 == "A little true" ~ 2,
  Outside_support3 == "Pretty much true" ~ 3,
  Outside_support3 == "Very much true" ~ 4,
  TRUE ~ NA_real_
))

#Schoolsupport1 
combined_all %<>% mutate(School_support1 = case_when(
  #Secondary school responses
  School_support1 == "Strongly Disagree" ~ 1,
  School_support1 == "Disagree" ~ 1,
  School_support1 == "Neither Disagree Nor Agree" ~ 2,
  School_support1 == "Agree" ~ 3,
  School_support1 == "Strongly Agree" ~ 4, 
  #elementary school reponses
  School_support1 == "No, never" ~ 1,
  School_support1 == "Yes, some of the time" ~ 2,
  School_support1 == "Yes, most of the time" ~ 3,
  School_support1 == "Yes, all of the time" ~ 4,
  TRUE ~ NA_real_
))

#Schoolsupport2
combined_all %<>% mutate(School_support2 = case_when(
  #Secondary school responses
  School_support2 == "Strongly Disagree" ~ 1,
  School_support2 == "Disagree" ~ 1,
  School_support2 == "Neither Disagree Nor Agree" ~ 2,
  School_support2 == "Agree" ~ 3,
  School_support2 == "Strongly Agree" ~ 4, 
  #elementary school reponses
  School_support2 == "No, never" ~ 1,
  School_support2 == "Yes, some of the time" ~ 2,
  School_support2 == "Yes, most of the time" ~ 3,
  School_support2 == "Yes, all of the time" ~ 4,
  TRUE ~ NA_real_
))


#Schoolsupport3
combined_all %<>% mutate(School_support3 = case_when(
  #Secondary school responses
  School_support3 == "Strongly Disagree" ~ 1,
  School_support3 == "Disagree" ~ 1,
  School_support3 == "Neither Disagree Nor Agree" ~ 2,
  School_support3 == "Agree" ~ 3,
  School_support3 == "Strongly Agree" ~ 4, 
  #elementary school reponses
  School_support3 == "No, never" ~ 1,
  School_support3 == "Yes, some of the time" ~ 2,
  School_support3 == "Yes, most of the time" ~ 3,
  School_support3 == "Yes, all of the time" ~ 4,
  TRUE ~ NA_real_
))

#Schoolsupport4
combined_all %<>% mutate(School_support4 = case_when(
  #Secondary school responses
  School_support4 == "Strongly Disagree" ~ 1,
  School_support4 == "Disagree" ~ 1,
  School_support4 == "Neither Disagree Nor Agree" ~ 2,
  School_support4 == "Agree" ~ 3,
  School_support4 == "Strongly Agree" ~ 4, 
  #elementary school reponses
  School_support4 == "No, never" ~ 1,
  School_support4 == "Yes, some of the time" ~ 2,
  School_support4 == "Yes, most of the time" ~ 3,
  School_support4 == "Yes, all of the time" ~ 4,
  TRUE ~ NA_real_
))


############################################AGGREGATE_DATA##############################################################################

#AGGREGATE ORDINAL 
combined_all %<>% mutate(School_support_agg = School_support1 + School_support2 + School_support3 + School_support4,
                                  Outside_support_agg = Outside_support1 + Outside_support2 + Outside_support3,
                                  Home_support_agg = Home_support1 + Home_support2 + Home_support3)

#AGGREGATE BINARY 
combined_all %<>% mutate(School_supportCat2_agg = School_support1_cat2 + School_support2_cat2 + School_support3_cat2 + School_support4_cat2,
                                  Outside_supportCat2_agg2 = Outside_support1_cat2 + Outside_support2_cat2 + Outside_support3_cat2,
                                  Home_supportCat2_agg2 = Home_support1_cat2 + Home_support2_cat2 + Home_support3_cat2)


########################################CHECKING DATA AND ASSIGNING CORRECTED AS NUMERIC,CHARACTER,FACTOR, ETC#########################

#LLU STUDENT ID
is.numeric(cleaned_combined_all3$llu_study_id)

#ZIPCODE
is.numeric(cleaned_combined_all3$stu_zip_code.cmb)

#GRADE LEVEL
is.factor(cleaned_combined_all3) #->false 
head(as.factor(cleaned_combined_all3$stu_grade))
cleaned_combined_all3$stu_grade <- factor(cleaned_combined_all3$stu_grade, labels = c("4", "5", "6", "7", "8", "11"))
is.factor(cleaned_combined_all3$stu_grade) #-->true 

#AGE at Study
is.numeric(cleaned_combined_all3$ageatstudy)

#GENDER
is.factor(cleaned_combined_all3$stu_gender)
head(as.factor(cleaned_combined_all3$stu_gender))
cleaned_combined_all3$stu_gender <- factor(cleaned_combined_all3$stu_gender, labels = c("F","M"))

#ETHNICITY 
is.factor(cleaned_combined_all3$stu_ethnicity_group) #-->false 
head(as.factor(cleaned_combined_all3$stu_ethnicity_group))
cleaned_combined_all3$stu_ethnicity_group <- factor(cleaned_combined_all3$stu_ethnicity_group, labels = c("African American", "Hispanic", "Other Minorities", "White"))
is.factor(cleaned_combined_all3$stu_ethnicity_group) #-->TRUE

#LOW SES STATUS
is.factor(cleaned_combined_all3$stu_lowses_status) #-->false 
head(as.factor(cleaned_combined_all3$stu_lowses_status))
cleaned_combined_all3$stu_lowses_status <- factor(cleaned_combined_all3$stu_lowses_status, labels = c("N", "Y"))
is.factor(cleaned_combined_all3$stu_lowses_status) #-->TRUE

#binary vars
#SBAC_ELA_LEVEL
is.factor(combined_all$stu_lowses_status) #-->false 
head(as.factor(combined_all$stu_lowses_status))
combined_all$stu_lowses_status <- factor(combined_all$stu_lowses_status, labels = c("N", "Y"))
is.factor(combined_all$stu_lowses_status) #-->TRUE




#cleaned_combined_all3 <- combined_all %>% dplyr::select(llu_study_id,stu_zip_code.cmb,stu_grade,ageatstudy,stu_gender,stu_ethnicity_group,stu_lowses_status,
                                                sbac_ela_level, 
                                                sbac_ela_level_description,
                                                sbac_math_level,
                                                sbac_math_level_description,
                                                Home_support1,
                                                Home_support2,
                                                Home_support3,
                                                Home_support_agg,
                                                Outside_support1,
                                                Outside_support2,
                                                Outside_support3,
                                                Outside_support_agg,
                                                School_support1,
                                                School_support2,
                                                School_support3,
                                                School_support4,
                                                School_support_agg,
                                                sbac_ela_level2cat, 
                                                sbac_ela_level_description2,
                                                sbac_math_level2cat,
                                                sbac_math_level_description2,
                                                Home_support1_cat2,
                                                Home_support2_cat2,
                                                Home_support3_cat2,
                                                Home_supportCat2_agg2,
                                                Outside_support1_cat2,
                                                Outside_support2_cat2,
                                                Outside_support3_cat2,
                                                Outside_supportCat2_agg2,
                                                School_support1_cat2,
                                                School_support2_cat2,
                                                School_support3_cat2,
                                                School_support4_cat2,
                                                School_supportCat2_agg)  




#Export table for analysis and future reference 
#readr::write_csv(cleaned_combined_all3,path = "C:/Users/Darelle/Dropbox/SCHOOL/LLU/Research/ICP/Data/Cleaned_combined_all3.csv")
  
##########################################GIS##################################################################################################################################
GIS_combined_all <-readr::read_csv("GIS_combined_all.csv")
  

GIS_combined_all %<>% mutate(Latitude = case_when(
  #Latitude
  stu_zip_code.cmb == 90255 ~ 33.983550,
  stu_zip_code.cmb == 91701 ~ 34.126314,
  stu_zip_code.cmb == 91706 ~ 34.088310,
  stu_zip_code.cmb == 91737 ~ 34.146567,
  stu_zip_code.cmb == 91739 ~ 34.127612, 
  stu_zip_code.cmb == 91750 ~ 34.120445,
  stu_zip_code.cmb == 91762 ~ 34.057350,
  stu_zip_code.cmb == 91766 ~ 34.050780,
  stu_zip_code.cmb == 91786 ~ 34.094592,
  stu_zip_code.cmb == 91790 ~ 34.062306,
  stu_zip_code.cmb == 92407 ~ 34.192524,
  stu_zip_code.cmb == 92220 ~ 33.964320,
  stu_zip_code.cmb == 92223 ~ 33.937090,
  stu_zip_code.cmb == 92301 ~ 34.583868,
  stu_zip_code.cmb == 92308 ~ 34.418937,
  stu_zip_code.cmb == 92313 ~ 34.027500,
  stu_zip_code.cmb == 92316 ~ 34.058214,
  stu_zip_code.cmb == 92317 ~ 34.245330,
  stu_zip_code.cmb == 92324 ~ 34.062706,
  stu_zip_code.cmb == 92334 ~ 34.062706,
  stu_zip_code.cmb == 92335 ~ 34.083068,
  stu_zip_code.cmb == 92336 ~ 34.142472,
  stu_zip_code.cmb == 92345 ~ 34.398411,
  stu_zip_code.cmb == 92346 ~ 34.127297,
  stu_zip_code.cmb == 92354 ~ 34.054281,
  stu_zip_code.cmb == 92359 ~ 34.087193,
  stu_zip_code.cmb == 92369 ~ 34.087193,
  stu_zip_code.cmb == 92373 ~ 34.034926,
  stu_zip_code.cmb == 92374 ~ 34.082763,
  stu_zip_code.cmb == 92376 ~ 34.117155,
  stu_zip_code.cmb == 92377 ~ 34.165239,
  stu_zip_code.cmb == 92382 ~ 34.221147,
  stu_zip_code.cmb == 92392 ~ 34.493634,
  stu_zip_code.cmb == 92394 ~ 34.566779,
  stu_zip_code.cmb == 92395 ~ 34.514563,
  stu_zip_code.cmb == 92399 ~ 34.058348,
  stu_zip_code.cmb == 92401 ~ 34.108330,
  stu_zip_code.cmb == 92402 ~ 34.108305,
  stu_zip_code.cmb == 92404 ~ 34.191322,
  stu_zip_code.cmb == 92405 ~ 34.145847,
  stu_zip_code.cmb == 92406 ~ 34.135025,
  stu_zip_code.cmb == 92408 ~ 34.087877,
  stu_zip_code.cmb == 92410 ~ 34.098228,
  stu_zip_code.cmb == 92411 ~ 34.123566,
  stu_zip_code.cmb == 92413 ~ 34.141258,
  stu_zip_code.cmb == 92423 ~ 34.028721,
  stu_zip_code.cmb == 92427 ~ 34.171154,
  stu_zip_code.cmb == 92428 ~ 34.171154,
  stu_zip_code.cmb == 92444 ~ 34.171154,
  stu_zip_code.cmb == 92501 ~ 33.993497,
  stu_zip_code.cmb == 92503 ~ 33.897429,
  stu_zip_code.cmb == 92507 ~ 33.981463,
  stu_zip_code.cmb == 92509 ~ 34.010957,
  stu_zip_code.cmb == 92557 ~ 33.979721,
  stu_zip_code.cmb == 92571 ~ 33.837351,
  stu_zip_code.cmb == 92591 ~ 33.541886,
  stu_zip_code.cmb == 92747 ~ 33.541886,
  TRUE ~ NA_real_
))
  

GIS_combined_all %<>% mutate(Longitude = case_when(
  #Latitude
  stu_zip_code.cmb == 90255 ~ -118.223500,
  stu_zip_code.cmb == 91701 ~ -117.565876,
  stu_zip_code.cmb == 91706 ~ -117.960450,
  stu_zip_code.cmb == 91737 ~ -117.578587,
  stu_zip_code.cmb == 91739 ~ -117.532475, 
  stu_zip_code.cmb == 91750 ~ -117.773075,
  stu_zip_code.cmb == 91762 ~ -117.695115,
  stu_zip_code.cmb == 91766 ~ -117.751851,
  stu_zip_code.cmb == 91786 ~ -117.683298,
  stu_zip_code.cmb == 91790 ~ -117.930879,
  stu_zip_code.cmb == 92407 ~ -117.351006,
  stu_zip_code.cmb == 92220 ~ -116.838516,
  stu_zip_code.cmb == 92223 ~ -116.981498,
  stu_zip_code.cmb == 92301 ~ -117.507840,
  stu_zip_code.cmb == 92308 ~ -117.159883,
  stu_zip_code.cmb == 92313 ~ -117.313092,
  stu_zip_code.cmb == 92316 ~ -117.389446,
  stu_zip_code.cmb == 92317 ~ -117.211600,
  stu_zip_code.cmb == 92324 ~ -117.329246,
  stu_zip_code.cmb == 92334 ~ -117.329246,
  stu_zip_code.cmb == 92335 ~ -117.461783,
  stu_zip_code.cmb == 92336 ~ -117.462300,
  stu_zip_code.cmb == 92345 ~ -117.302190,
  stu_zip_code.cmb == 92346 ~ -117.179593,
  stu_zip_code.cmb == 92354 ~ -117.252331,
  stu_zip_code.cmb == 92359 ~ -117.110177,
  stu_zip_code.cmb == 92369 ~ -117.110177,
  stu_zip_code.cmb == 92373 ~ -117.207488,
  stu_zip_code.cmb == 92374 ~ -117.197609,
  stu_zip_code.cmb == 92376 ~ -117.375624,
  stu_zip_code.cmb == 92377 ~ -117.405504,
  stu_zip_code.cmb == 92382 ~ -117.123263,
  stu_zip_code.cmb == 92392 ~ -117.409814,
  stu_zip_code.cmb == 92394 ~ -117.350309,
  stu_zip_code.cmb == 92395 ~ -117.293673,
  stu_zip_code.cmb == 92401 ~ -117.011464,
  stu_zip_code.cmb == 92402 ~ -117.286595,
  stu_zip_code.cmb == 92404 ~ -117.251345,
  stu_zip_code.cmb == 92405 ~ -117.299709,
  stu_zip_code.cmb == 92406 ~ -117.289655,
  stu_zip_code.cmb == 92408 ~ -117.267554,
  stu_zip_code.cmb == 92410 ~ -117.304717,
  stu_zip_code.cmb == 92411 ~ -117.319148,
  stu_zip_code.cmb == 92413 ~ -117.250924,
  stu_zip_code.cmb == 92423 ~ -117.186790,
  stu_zip_code.cmb == 92427 ~ -117.341328,
  stu_zip_code.cmb == 92428 ~ -117.341328,
  stu_zip_code.cmb == 92444 ~ -117.341328,
  stu_zip_code.cmb == 92501 ~ -117.372435,
  stu_zip_code.cmb == 92503 ~ -117.446509,
  stu_zip_code.cmb == 92507 ~ -117.321083,
  stu_zip_code.cmb == 92509 ~ -117.416175,
  stu_zip_code.cmb == 92557 ~ -117.259801,
  stu_zip_code.cmb == 92571 ~ -117.208751,
  stu_zip_code.cmb == 92591 ~ -117.114013,
  stu_zip_code.cmb == 92747 ~ -117.114013,
  TRUE ~ NA_real_
))

GIS_combined_all2 <- GIS_combined_all %>% select(llu_study_id,stu_zip_code.cmb,Longitude,Latitude,stu_lowses_status,sbac_ela_level2cat,sbac_math_level2cat, School_supportCat2_agg, Outside_supportCat2_agg2, Home_supportCat2_agg2)

#write gis file 
#readr::write_csv(GIS_combined_all2,path = "C:/Users/Darelle/Dropbox/SCHOOL/LLU/Research/ICP/Data/GIS_combined_all12.csv")
  
  
##########################################ANALYSIS ON CLEANED_COMBINED########################################################################################################

#Read made dataset
cleaned_combined_all3 <-readr::read_csv("cleaned_combined_all3.csv")











######factor the variables
#LLU STUDENT ID
is.numeric(GIS_combined_all$stu_zip_code.cmb)

is.numeric(cleaned_combined_all3$llu_study_id)

#ZIPCODE
is.numeric(cleaned_combined_all3$stu_zip_code.cmb)

#GRADE LEVEL
is.factor(cleaned_combined_all3) #->false 
head(as.factor(cleaned_combined_all3$stu_grade))
cleaned_combined_all3$stu_grade <- factor(cleaned_combined_all3$stu_grade, labels = c("4", "5", "6", "7", "8", "11"))
is.factor(cleaned_combined_all3$stu_grade) #-->true 

#AGE at Study
is.numeric(cleaned_combined_all3$ageatstudy)

#GENDER
is.factor(cleaned_combined_all3$stu_gender)
head(as.factor(cleaned_combined_all3$stu_gender))
cleaned_combined_all3$stu_gender <- factor(cleaned_combined_all3$stu_gender, labels = c("F","M"))

#ETHNICITY 
is.factor(cleaned_combined_all3$stu_ethnicity_group) #-->false 
head(as.factor(cleaned_combined_all3$stu_ethnicity_group))
cleaned_combined_all3$stu_ethnicity_group <- factor(cleaned_combined_all3$stu_ethnicity_group, labels = c("African American", "Hispanic", "Other Minorities", "White"))
is.factor(cleaned_combined_all3$stu_ethnicity_group) #-->TRUE

#LOW SES STATUS
is.factor(cleaned_combined_all3$stu_lowses_status) #-->false 
head(as.factor(cleaned_combined_all3$stu_lowses_status))
cleaned_combined_all3$stu_lowses_status <- factor(cleaned_combined_all3$stu_lowses_status, labels = c("N", "Y"))
is.factor(cleaned_combined_all3$stu_lowses_status) #-->TRUE




########################################Creating table for Cleaned_combined_all3#########################################################################################

#ALL STUDENTS
summarize(cleaned_combined_all3)

#AGE
min(cleaned_combined_all3$ageatstudy)
max(cleaned_combined_all3$ageatstudy)
sd(cleaned_combined_all3$ageatstudy)
mean(cleaned_combined_all3$ageatstudy)

#GENDER
table(cleaned_combined_all3$sbac_ela_level)



prop.table(table(cleaned_combined_all3$sbac_ela_level))*100






table(cleaned_combined_all3$sbac_ela_level)
prop.table(table(cleaned_combined_all3$s))*100


CrossTable(cleaned_combined_all3$sbac_ela_level2cat, c$stu_gender,digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE,sresid=TRUE) #english #REMEMBER 4th row is percent


CrossTable(cleaned_combined_all3$sbac_math_level2cat, combined$stu_gender, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math #REMEMBER 4th row is percent




#CROSSTABLE 
#HomeSUpport
table(cleaned_combined_all3$Home_support1)
prop.table(table(cleaned_combined_all3$Home_support1))*100

CrossTable(cleaned_combined_all$Home_support1, cleaned_combined_all3$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english1

table(cleaned_combined_all$Home_support2)
prop.table(table(cleaned_combined_all$Home_support2))*100

CrossTable(cleaned_combined_all$Home_support2, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english2

table(cleaned_combined_all$Home_support3)
prop.table(table(cleaned_combined_all$Home_support3))*100

CrossTable(cleaned_combined_all$Home_support3, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english3

CrossTable(cleaned_combined_all$Home_support1, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math1

CrossTable(cleaned_combined_all$Home_support2, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math2

CrossTable(cleaned_combined_all$Home_support3, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math3

#Outside Support
table(cleaned_combined_all$Outside_support1)
prop.table(table(cleaned_combined_all$Outside_support1))*100

table(cleaned_combined_all$Outside_support2)
prop.table(table(cleaned_combined_all$Outside_support2))*100

table(cleaned_combined_all$Outside_support3)
prop.table(table(cleaned_combined_all$Outside_support3))*100

CrossTable(cleaned_combined_all$Outside_support1, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english1

CrossTable(cleaned_combined_all$Outside_support2, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english2

CrossTable(cleaned_combined_all$Outside_support3, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english3

CrossTable(cleaned_combined_all$Outside_support1, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math1

CrossTable(cleaned_combined_all$Outside_support2, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math2

CrossTable(cleaned_combined_all$Outside_support3, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math3

#School Support
table(cleaned_combined_all$School_support1)
prop.table(table(cleaned_combined_all$School_support1))*100

table(cleaned_combined_all$School_support2)
prop.table(table(cleaned_combined_all$School_support2))*100

table(cleaned_combined_all$School_support3)
prop.table(table(cleaned_combined_all$School_support3))*100

table(cleaned_combined_all$School_support4)
prop.table(table(cleaned_combined_all$School_support4))*100

CrossTable(cleaned_combined_all$School_support1, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english1

CrossTable(cleaned_combined_all$School_support2, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english2

CrossTable(cleaned_combined_all$School_support3, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english3

CrossTable(cleaned_combined_all$School_support4, cleaned_combined_all$sbac_ela_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #english4


CrossTable(cleaned_combined_all$School_support1, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math1

CrossTable(cleaned_combined_all$School_support2, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math2

CrossTable(cleaned_combined_all$School_support3, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math3

CrossTable(cleaned_combined_all$School_support4, cleaned_combined_all$sbac_math_level2cat, digits=4, 
           expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE) #math4




#######################################################################################################################################
#########################################################ANALYSIS######################################################################
#######################################################################################################################################



#cleaned_combined_all2 %<>% mutate(School_support_cat4 = case_when(
  #School_support_agg < 8 ~ "1st",
  #School_support_agg < 12 ~"2nd",
  #School_support_agg < 
#))



#ANALYSIS FOR FOR BINARY VARIABLES 
summary(cleaned_combined_all3)
sapply(cleaned_combined_all3,sd)

#references
#gender
cleaned_combined_all3$stu_gender <- relevel(cleaned_combined_all3$stu_gender, ref = "F")

#ethnicity
cleaned_combined_all3$stu_ethnicity_group <- relevel(cleaned_combined_all3$stu_ethnicity_group, ref = "White")

#LOW SES STATUS
cleaned_combined_all3$stu_lowses_status<- relevel(cleaned_combined_all3$stu_lowses_status, ref = "N")

#GRADE --> most people did well on tests on balance both for 7th grade
cleaned_combined_all3$stu_grade<- relevel(cleaned_combined_all3$stu_grade, ref = "11")


                                         #ENGLISH
glm(sbac_ela_level2cat ~ Home_supportCat2_agg2 + Outside_supportCat2_agg2 + School_supportCat2_agg +  
              ageatstudy + stu_gender + stu_ethnicity_group + stu_lowses_status + stu_grade, 
              family = binomial(link = "logit"),data = cleaned_combined_all3) %>% summary

binarylogitE <- glm(sbac_ela_level2cat ~ Home_supportCat2_agg2 + Outside_supportCat2_agg2 + School_supportCat2_agg +  
                     ageatstudy + stu_gender + stu_ethnicity_group + stu_lowses_status + stu_grade, 
                   family = binomial(link = "logit"),data = cleaned_combined_all3)



CIbetaEnglish <- confint.default((binarylogitE))   #CI 
OR.ci <- exp(CIbetaEnglish[-1,])


OR <- exp(coef(binarylogitE)[-1]) #OR

cbind(OR, OR.ci)

#High CI indicates that underage or overage might have an effect on grade performance 
#so create new variable offsetage then check model

#new variable ageoffset
cleaned_combined_all3 %<>% group_by(stu_grade) %>%  mutate(ageoffset = mean(ageatstudy, na.rm = T)-ageatstudy)

binarylogitE <- glm(sbac_ela_level2cat ~ Home_supportCat2_agg2 + Outside_supportCat2_agg2 + School_supportCat2_agg +  
                     ageoffset + stu_grade + stu_gender + stu_ethnicity_group + stu_lowses_status + stu_grade, 
                   family = binomial(link = "logit"),data = cleaned_combined_all3) #AGEOFFSET


CIbetaEnglish <- confint((binarylogitE))   #CI 
OR.ciE <- exp(CIbetaEnglish[-1,])


ORE <- exp(coef(binarylogitE)[-1]) #OR

cbind(ORE, OR.ciE)


                                         #MATH
glm(sbac_math_level2cat ~ Home_supportCat2_agg2 + Outside_supportCat2_agg2 + School_supportCat2_agg +  
                ageatstudy + stu_gender + stu_ethnicity_group + stu_lowses_status + stu_grade, 
                    family = binomial(link = "logit"),data = cleaned_combined_all3) %>% summary

binarylogitM<- glm(sbac_math_level2cat ~ Home_supportCat2_agg2 + Outside_supportCat2_agg2 + School_supportCat2_agg +  
                     ageatstudy + stu_gender + stu_ethnicity_group + stu_lowses_status + stu_grade, 
                   family = binomial(link = "logit"),data = cleaned_combined_all3)  #AGEATSTUDY




CIbetaMath <- confint((binarylogitM))   #CI 
OR.ciM <- exp(CIbetaMath[-1,])


ORM <- exp(coef(binarylogitM)[-1]) #OR

cbind(ORM, OR.ciM)


#MATH MODEL WITH AGEOFFSET
binarylogitM<- glm(sbac_math_level2cat ~ Home_supportCat2_agg2 + Outside_supportCat2_agg2 + School_supportCat2_agg +  
                     ageoffset + stu_gender + stu_ethnicity_group + stu_lowses_status + stu_grade, 
                   family = binomial(link = "logit"),data = cleaned_combined_all3)  #AGEOFFSET


ORM <- exp(coef(binarylogitM)[-1]) #OR

cbind(ORM, OR.ciM)



#HOMESUPPORT                                - ENGLISH
table(cleaned_combined_all3$sbac_ela_level2cat , cleaned_combined_all3$Home_supportCat2_agg2)
glm(sbac_ela_level2cat ~ Home_supportCat2_agg2 +ageatstudy, family = binomial, data=cleaned_combined_all3) %>% summary

#OUTSIDESUPPORT                             - ENGLISH
table(cleaned_combined_all3$sbac_ela_level2cat , cleaned_combined_all3$Outside_supportCat2_agg2)
glm(sbac_ela_level2cat ~ Outside_supportCat2_agg2, family = binomial, data=cleaned_combined_all3) %>% summary

#SCHOOLSUPPORT                              - ENGLISH
table(cleaned_combined_all3$sbac_ela_level2cat , cleaned_combined_all3$School_supportCat2_agg)
glm(sbac_ela_level2cat ~ School_supportCat2_agg, family = binomial, data=cleaned_combined_all3) %>% summary


#notes 
#Intercept is left side variable,such as sbac_ela_level2cat































#STUFF?
cleaned_combined_all3 %$% hist(School_support_agg)
cleaned_combined_all3 %$% table(stu_grade, School_support_agg) %>% prop.table(1) %>% multiply_by(100) %>% round(1) 
cleaned_combined_all2 %<>% mutate(School_support_agg_pct = percent_rank(cleaned_combined_all2$School_support_agg),
                                  School_support_agg_q4  = cut(School_support_agg_pct, breaks =c(-Inf, 0.25, 0.50, 0.75, Inf), labels = c("1st", "2nd", "3rd", "4th")))
table(cleaned_combined_all2$School_support_agg_q4)
#STUFF?


polr
fit <- polr(sbac_math_level ~ School_support_agg_q4, 
            data=cleaned_combined_all2 %>% mutate(
              sbac_math_level = sbac_math_level %>% factor,
              School_support_agg_q4 = School_support_agg_q4 %>% factor))
(sfit <- summary(fit))
(ctable <- coef(sfit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p %>% round(4)))

#Porpotional odds 



