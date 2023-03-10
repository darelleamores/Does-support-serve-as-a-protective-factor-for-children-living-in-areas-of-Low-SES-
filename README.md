# Does support serve as a protective factor for children living in areas of Low SES?

# Abstract 
Background: Various research have shown significance results that an individual living environment plays a significant role in the outcome of academic performance. Individuals who live in areas of low poverty or SES, do worse than individuals who come from better living conditions in terms of health, living conditions, and even academic performance. However, research has shown that students who have a support system at home or school do better than their peers who do not have a support system. The purpose of this study is to understand the significance of support in a student’s academic performance even when they are living in areas that are filled with adverse environmental factors such as high crime rate, food desert, and poverty. 

Method: Students(N=16,422) in grades 5,6, 7,8, and 11 participated to take the 2018-2019 California Assessment of Student Performance and Progress (CAASPP) test which is consist of English and Math proportion. The students were later surveyed a year later after taking the test. A binary logistic regression was used to determine the effects of demographics, SES, grade, and support towards CAASP english and math test results using RStudio and SAS. GIS was also used to determine the spatial analysis of student’s living conditions and the problematic factors that come with it by zipcode. 

 Results: Students who had support at Home (OR = 1.19, P<.001) and School(OR = 1.04, P<.001) had positive effects towards passing the CAASP English portion, as well as the CAASP Math portion (OR = 1.16, P<.001, OR 1.06, P<.001) respectively. In terms of Race/Ethnicity compared to whites, African Americans had the lowest odds in doing well in the CAASP tests. 

 Conclusion: Home and school support is a significant indicator for increase academic achievement and standardized test scores for students who come from areas of high risks such as high crime rates, poverty, and food deserts. Create resources, programs and policies that help at-risk populations for academic achievement.

# Introduction
Education for one’s well-being is a powerful predictor for health behaviors and health risks. Individuals who have higher achievement performance in education showed higher rates in positive health outcomes and decrease participation risky behaviors such as smoking and alcohol abuse, and violent crimes (Hahn & Truman, 2015). According to reports from the CDC, individuals who lived in households with a bachelor’s degree or higher, had decrease obesity prevalence compared to households with less than a high school education(CDC, 2020).  Inversely, individuals who had the lowest academic performance in education showed higher rates of obesity, alcohol & drug abuse, smoking, injury and even crime leading to prison detainment (Bayless et al., 2018).  An adolescent’s academic performance and attainment can be affected by their own living environment and conditions (Crede, Wirthwein, McElvany, & Steinmayr, 2015). Individuals who live in poverty are adversely affected by the social and environmental factors such as food deserts and high crime rates, and are part of the lower economic status spectrum (Meade, 2014) (Neff, Palmer, McKenzie, & Lawrence, 2009). Low income areas also lack clean streets, public areas, and even walkable sidewalks. Thus, these areas have decreased traffic safety and physical activity thus will lead to higher rates of diseases (Neckerman et al., 2009). There is a high percentage of minority groups such as Hispanics and African Americans living in these low-income areas.
 Low socioeconomic status (SES) areas are plagued with food deserts, or areas where it is hard to buy fresh, affordable, good quality food (Hilmers, Hilmers, & Dave, 2012). These food deserts are characterized by a few supermarkets, lots of convenient stores, and fast food joints that sell saturated, fatty processed foods that is affordable for the population(Widener, Metcalf, & Bar-Yam, 2012).  The affordable and convenient compensation for the poverty area stricken with food deserts has shown there is significant decrease in academic performance, as well as showing increased obesity in children. There is a correlation between a child’s test scores and if he/she lives in a highly dense food desert area (Frndak, 2014). Children living in food deserts do poorly than children who do not live in a food desert. Increased consumption of high saturated fats and carbs can lead to detrimental cognitive effects that lead to poor academic performance. However, if a child is living in a food desert area but comes from a higher income family, it can negate the correlated effects of poor academic performance by food desert prevalence thus high SES can be a protective factor against the negative effects of a food desert areas (Frndak, 2014). To make matters worse, food deserts have shown a significant correspondence with a higher crime rate which is evident in overlapping characteristics of poverty areas(Crowe, Lacy, & Columbus, 2018).  Low SES have a higher rate of being involved or witnessing crime and violence. Prolonged exposure to crime and violence creates risks in academic learning by affecting the cognitive function of the brain that affects memory attention which helps in academic performance (Moffitt & Klaus-Grawe Think, 2013). Violence through an individual’s stress response decreases cognitive function thus leading to lower academic performance; it can even cause developmental issues in a child (Cybele Raver & Blair, 2016). Heightened exposure to violence also have shown to increase maladaptive behaviors such as increase aggressiveness and even promoting violence themselves while creating consequences towards academic performance(Haynie, Silver, & Teasdale, 2006). Students who participated in violent behaviors such as fights at school lead to increased school dropouts and lower grades which are common  in lower SES areas (Jensen, 2009). However, when rates of violence is decreased in neighborhoods and schools, there is an increase in academic performance (Casillas et al., 2012).  
Academic performance is an important inference for societal foundation and learning skills such as if individuals who have poor grades are more likely to drop out of school, increased societal exclusion, and even decreased health outcomes (Hughes, Cao, West, Allee Smith, & Cerda, 2017). Poor academic performance is also positively associated with substance abuse, unsafe sexual activity, and poor mental health (Cox, Zhang, Johnson, & Bender, 2007). Minority groups such as African Americans and Hispanics are reported to have increased risk to participate in these adverse health behaviors due to having poor academic performance compared to their White counterparts such indicating achievement disparities in the education system (APA, 2017). This gap in the education system between minority groups and whites can be due to higher reports of discrimination, unfair treatment, and marginalization from not only from school faculty but from their living conditions as well which are derived from variables such as stress, tension, lack of communication for needs and resources, and fear(APA, 2012). 
Research has indicated that school and family support shown protective effects against low academic performance due to poverty factors(Kim, Mazza, Zwanziger, & Henry, 2014). Family support towards a child’s education in low SES areas have shown to do better than individuals who did not have family support while living in the same environment(Conger, Conger, & Martin, 2010). They also found that if the parent(s) had full-time employment and was not able to support the child in school, lead to negative impacts in learning in math and science  (Mayo & Siraj, 2015). On the other hand, when a child is neglected by family, it seems to increase maladaptive behavior in the child which can lead to lower academic performance regardless of poverty level even though there is high correlation between neglect and poverty level(Herruzo, Raya Trenas, Pino, & Herruzo, 2020).  Studies have shown that supportive relationships from family, friends and teachers from school have acted as a buffer for better academic performance even when a child comes from a low socioeconomic setting that has high rates of socioeconomic stressors that lead to poor cognitive learning (Rezaei-Dehaghani, Keshvari, & Paki, 2018). 
The objective of this study is to examine the environmental and social factors that affect academic achievement such as crime rates, food deserts, and social support to model the Pass/Did Not Pass outcome of the California Assessment of Student Performance and Progress (CAASP) tests for San Bernardino Unified School District (SBCUSD) students from grade levels 4,5, 6, 7,8 and 11. Demographic and descriptive variables such as students’ date of birth, gender, race/ethnicity, low SES status and environmental living conditions by zip code were also accounted for in the analysis. The first hypothesis is that SBUSD students will have better test scores while controlling for age, gender, grade level, race/ ethnicity, and low SES status if given the different areas of support which are support from school, support from home, and outside support other than school and home. The second hypothesis indicates that SBCUSD students living in lower SES areas that are characterized by higher high crime rate, lower average median income, and increased density of food deserts will have lower test scores than students who have better living conditions.

# Methods 

Participants were students from the SBCUSD in Southern California. The data collected was during the school year of 2018-2019 which comprised of 4th, 5th, 6th, 7th , 8th , and 11th graders. Students each year are required to take the CAASP which is comprised of an English and Math portion. The CAASP test was designed to help teachers in the State of California to help monitor the needs and success of individual students in their classrooms. After completing the CAASP test, a survey instrument measured school climate, social emotional learning, risky behaviors of the students, school support, home support outside support, physical and mental health, food and nutrition, and discrimination. Post surveys were administered in English, in a single class period and to minimize impact on instructional time, the number of questions asked were limited. Surveys that were given which were based on the California Healthy Kids Survey Resiliency Module which assessed school support, home support and home support. These responses were linked to the student’s record (grade level, home zip code, free or reduced lunch (low SES status, gender, and grade level.), de-identified, and given a LLU student ID marker to keep consistency in responses for the CAASP and survey response data. These responses were made available for researchers for analysis. A total of 16,422 students aged 9 to 19 years old participated in both the CAASP test and the survey. As shown in Table 1, majority of the students were Male and Hispanic. More than half of the students qualified for free lunch (low SES status). 


Measures

Outcome Variables: CAASP English and Math, pass or fail 
The binary outcome of pass or fail for both the English and Math section of the CAASP test was used to access academic performance. Passing was referred as “Met the Standard” and coded as 1. Failing was referred as “Not met the standard” was coded as 0. 
	
Home Support were based on questions that were modeled after the California Healthy Resiliency Module. Questions such as “Who is interested in my schoolwork”, “Who talks with me about my problems”, and “Who really cares about me” were categorized Home Support. The Responses “Not at all True” and “A little true” were coded as 0 while “Pretty much true” and “Very much true” were coded as 0.  Questions such as “Who really cares about me”, “ Who notices when I am upset about something” and “Who I trust” were categorized as Outside Support. The Outside support responses and coding were the same as Home Support. Questions such as “Do adults at school encourage you to work hard so you can be successful?”, Adults at this school encourage me to work hard to I can be successful in college or at the job I choose”, Do teachers give students a chance to take part in classroom discussions or activities, and “ My teachers work hard to help me with my schoolwork when I need it” were categorized as School support. The responses “No,never’, “ Disagree”, “Strongly Disagree”, Neither Disagree Nor Agree” were coded as 0 while “Agree”, “Strongly Agree”, “Yes, some of the time”, “Yes , most of the time”, “Yes, all of the time” were coded as 1. 

School independent variables: Low SES status and Grade level

Low SES status was based on if the student received free or reduced lunch. Responses were either “Yes” or “No” and coded as 1 and 0, respectively.  Students grade levels consisted of 4 referred to their respective grade level number (4,5,6,7,8,11) in the data. 

Socio-demographic controls: Age at survey, Gender, and Race/Ethnicity
Characteristics such as calculated age at survey, gender (M= Male and F = Female) and Race/Ethnicity (African Americans, Hispanics, and Other Minorities. Age was calculated using the students’ date of birth and the response start time for the survey which included both the month and the year. Students identifying as American Indian, Pacific Islander, Filipino, and Two or More Races were categorized into Other Minorities due to the very low percentage (less than 5%) for their individual categorical level.

# Data Analysis
Logistic Regression
Rstudio was used to clean the data and to combine the CAASP and survey data for the analyses. Variables such as age at survey and Home support, School support, and Outside support were calculated and computed accordingly.  SAS, version 9.4 was then used for analyses. First, descriptive statistics were computed for the continuous variables such as age at study. Frequencies and percentages were computed for the independent variables such as gender, race/ethnicity, low socioeconomic status, and grade level. 
A logit logistic binary model procedure was used to estimate the effects of support and other independent variables such ass gender, race/ethnicity, and low SES on the effect on the binary outcome CAASP test performance. There were two models for our outcomes which was one for CAASP English test and the CAASP Math test. Proc Logistic in SAS was used for logistic regression analysis. Step 1 of our analysis was choosing the references for our independent variables for the class statement in our models. Female was chosen as the reference for gender due to females doing better in the CAASP test compared to males. In terms of race, Whites was used as the references which according to research and the descriptive data, do better in academic performance. The answer “No” was chosen as the reference for the variable Low SES status due to data and past research indicating that that individuals who are higher SES do significantly better than their lower SES counterparts at school. In terms of grade level, 11th grades do better in both the English and Math respectively compared to their younger classmates. Step 2 was creating the models in our analysis. In our English model statement, English CAASP outcome was chosen as the outcome, while home support, outside support, school support, age at survey, gender, race/ethnicity, low SES status, and grade level. For the Math model, it contained the Math CAASP outcome instead of the English CAASP outcome. 

GIS 
GIS, version 2.4 was used for spatial analysis and the variables used for this analysis was zip code, CAASP English and Math test performance, School support, Home Support, Outside Support and low SES status. Using a zip code layer from Arc GIS online, students zip code locations were plotted. Ratios from pass or fail according to zip code of students living there, was symbolized as circles. Bigger circles indicated more passing while smaller circles indicated more students failing according to that zip code. These test scores were plotted on 11 maps according in order to find any patterns between environment and test score performance. Layers that were created using ARC GIS Pro were, Food Environment, Poverty, Crime, and Support (Outside, School, and Home). 

# Results 
Descriptive Statistics 
As shown on Table 1, Females (55.52%) did better than Males (44.48%) in the CAASP English portion(P<.0001). However, in the CAASP Math portion, Males did a little better (50.41%) than Females (49.59%). In terms of race/ethnicity, Whites did better in both the English (56.64%) and Math (39.32%) portion of the CAASP test. Other Minorities (CAASP English 47.80%, CAASP Math 34.74%) was second behind Whites, followed by Hispanics (CAASP English 43.53, CAASP Math 25.69), and then lastly African Americans who did the worst in both tests compared to the other race/ethnic groups (CAASP English 34.81%, CAASP Math 15.82%). Low SES status students did worst in both the English (41.59%, P<.0001) and Math (24.69%) CAASP test than their higher SES status counterparts (CAASP English 55.81%, CAASP Math 34.15%) In terms of grade level, 11th graders did the best in the English portion of the CAASP test but did the worst in the Math portion. 4th graders did the best in the Math portion of the CAASP but did the worst in the English portion.  

Spatial Analysis 
	According to our data, some students from the SBCUSD lived outside the school district boundary as far at Los Angeles and Riverside county (Map 1). In terms of poverty, that resided in SBCUSD boundary, lived in high percentage of poverty which is about 33-44.5% of the individuals in those zip codes. When plotting the test scores on top of the poverty, we see that students do better in areas with less poverty than areas with high poverty (Map 2-3). When looking at food environment, we can see that areas with less access to food are places in SBCUSD district. This is indicated by the amount of red dots in the area which more red dots indicates less access for people in poverty. The area in shades of green indicates the 1 mile walkable buffer to the grocery store or market place, and the yellow dots indicates the location of these resources (Map 4). When test scores are plotted, there’s better access to foods and test scores. in areas such near Rancho Cucamonga, Los Angeles County and Riverside County, indicated by the less number of red dots compared to the SBCUSD area. When looking at 2019 USA Crime Index, higher rates of crimes by zip code is indicated by the darker shades of red in that area. However, all students lived in areas that had higher crime than the US average, so it is difficult to distinguish a pattern between test scores and crime (Map 5-6). When looking at the support maps, we can see that more support is indicated by darker shades of the color that represents support. We can see that there are more support outside the SBCUSD boundary for those students and this is for outside, school and home support (Maps 7-12)


Logistic Regression 
As shown on Table 2, we see that for Home Support it is significant towards passing both English and Math and has increase odds. We also see similar results for school support. In terms of age, gender, race/ethnicity, and grade was significant but did not have odds for passing both the CAASPP math and english tests. 

# Discussion 
The purpose of this study was to investigate test score performance in relation with environmental and social factors in relation to supporting cultivating factors of children living in areas of poverty. In the logistic regression analysis, test score performance based on Math and English was based on students’ age during the survey, gender, race/ethnicity, grade, low SES status and home, school, and outside support. Children who are low SES and live in areas that are filled with food deserts and high crime rates have low test scores (Frndak, 2014).   
	As we see on Table 2, we see that odds increase in english test scores and decrease in math test scores as the age range and grade level increase. As we see on Table 2, we see that odds increase in english test scores and decrease in math test scores as the age range and grade level increase. The reason for two models was due to avoid collinearity due to a strong correlation between age range and grade level. Generally, students do better on the english test than the math test and the individuals who had the least odds of passing were younger, early grade level, male, and African American.According to a study by PLOS, students who are older and mature do better academically than their young student counterparts due grasping educational concepts effectively than their young counter parts(Navarro, García-Rubio, & Olivares, 2015). However, poor mathematical performance in increasing age and grade level is an issue, this might be caused by anxiety that is based on stress caused by decreased efficacy, beliefs, and conceptual based on past mathematical experiences in their academic career (Luttenberger, Wimmer, & Paechter, 2018). Males do better than females in math typically till 10 to 12 years old, but due to changes to admissions to getting into colleges, increase requirement of taking a math course, and the recent increase percentage of females entering higher education and STEM jobs, female mathematical performance have increased through the years and closing the gap between their male counterparts (Hyde & Mertz, 2009).  
Children who come from higher-income families had contrasting attitudes and higher self-confidence such there is significant odds for higher-income children to attend and complete college compared to students living in poverty(Jensen, 2009). thus higher-income children perform better in academia than low-income children. Top schools in America spend about 10 times more than poorer schools thus poorer schools have lower-quality resources such as less experienced teachers and outdated books which can lead to poorer education quality (Jensen, 2009) From several studies, girls do better than boys because of more family involvement in academics and a set standard set compared to boys (Johnson, McGue, & Iacono, 2007). In terms of race/ethnicity, poor African Americans do the worst in academic performance, this is due to having higher rates of less access to  healthy foods, increases likelihood of homicidal death due to firearms (57%) and living in poverty, less access to healthcare and decrease lack of education which indicates decrease healthy outcomes(Noonan, Velasco-Mondragon, & Wagner, 2016). Decrease healthy outcomes which is also the byproduct of low SES indicates low academic achievement in school such low SES can lead to increase household family problems due to increase economic pressure affect household support which can create parenting problems that decreases the child’s well-being in that household(Li, Xu, & Xia, 2020). The importance of high SES status, and support from home such as high income in areas of low poverty that support that it can serve as a protective factor against food desert such as students who can get healthy food from home did better even when living in areas of food deserts (Frndak, 2014).Parental involvement or parental support also has strong evidence to children’s dietary habit regardless of food environment, such guardians who promote healthy eating such as increase intake of fruits and vegetables while lower intake of junk food, as children imitate their parents eating habits (Scaglioni et al., 2018). Students who are have healthy diets show a positive association towards academic performance because of decrease intake of high fats and carbs that lead to decrease in cognitive function that is detrimental in academic performance (Gómez-Pinilla, 2008).
	As seen in table 2, there was increase odds for students in passing both the English and Math CAASP tests if there was support at home and school. Studies have shown that if there is adult involvement with a child’s academia life, children have increased positive relationship with teachers and academic performance (Topor, Keane, Shelton, & Calkins, 2010). This can be due to adult guardians of the students becoming aware of potential barriers to learning that can be addressed and be replaced with positive reinforcing behaviors to increase positive behavior outcomes for academic performance(Topor et al., 2010). Students who live in vulnerable populations with high food deserts and crime rates but had close connectiveness at home, community and school reported decrease “…levels of depressive symptoms, suicidal ideation, social anxiety, and sexual activity, as well as higher levels of higher levels of self-esteem and more adaptive use of free time (Foster et al., 2017).”  Children who reported high self-esteem and confidence showed positive correlation with academic achievement, and it’s even suggested that it also helps with self-efficacy towards complex world problems (Moradi Sheykhjan & Jabari, 2014). So low SES children who have close support and relationships from school and home, are more self-confident and efficient to achieve better standardized test results even when living in a vulnerable population compared to low SES children who do not have support. Children who are neglected or do not have support home, reported lower grades and less engagement in the classroom for academic success(Manly, Lynch, Oshri, Herzog, & Wortel, 2013). Low SES children who experienced neglect had increased prolonged stress due to poverty factors such as high crimes, adversely effected cognitive development and led to poor academic performance(Pechtel & Pizzagalli, 2011). 

# Limitations
For zip codes, we did not know the exact physical addresses of the students. So hard to indicates specific environment factors in their census block that might not reflect the whole zip code area.
For support variables, these were self-report, and might indicate reporter biases due to being based on the student’s own perception. These tests are 1-2 hours long and is computer based. Students in school normally take test on paper. This might interfere with effective test taking strategies. For the responses, we can no validate if the students understood the responses such as we see here, it is hard to tell the difference between No, Never and Strongly Disagree.

# Conclusion 
Students who come from high concentrated poverty areas have lower test scores. However, home and school support is a significant indicator for increase academic achievement and standardized test scores for students who come from areas of high risks such as high crime rates, poverty, and food deserts. Create resources, programs and policies that help at-risk populations for academic achievement. This also shows the importance of being involved with adolescent's social and academic life to help increase positive health outcomes and behaviors.

# Appendixes 
Map 1. Students living outside and inside SBCSUD boundary 
![Map 1  Students living outside and inside SBCUSD boundary](https://user-images.githubusercontent.com/50031745/216805655-bb79ae53-555c-42a7-950d-ed2656e28346.png)

Map 2. Poverty and English Test scores

![Map 2  Poverty and English Test scores](https://user-images.githubusercontent.com/50031745/216805665-14e7a7f6-34a7-4d1a-84e3-0224fc1ab490.png)

Map 3. Poverty and Math Test scores

![Map 3  Poverty and Math Test scores](https://user-images.githubusercontent.com/50031745/216805672-4d0c24bc-d344-48ad-a759-4466c4e0c14c.png)

Map 4. Food Environment

![Map 4  Food Environment](https://user-images.githubusercontent.com/50031745/216805681-5c42aeb5-7105-4886-96ca-c69b56b0e87b.png)

Map 5. Food Environment and English Test Scores

![Map 5  Food Environment and English Test Scores](https://user-images.githubusercontent.com/50031745/216805692-de8083e1-bc6c-4ede-8b40-2f51a68f53ba.png)

Map 6. Food Environment and Math Test Scores

![Map 6  Food Environment and Math Test Scores](https://user-images.githubusercontent.com/50031745/216805700-ba9cb4ac-fb4f-4a88-9262-ff1c87840129.png)

Map 7. English Scores by 2019 USA Average Crime Index

![Map 7  English Scores by 2019 USA Average Crime Index](https://user-images.githubusercontent.com/50031745/216805727-d2316ad9-9793-49b2-ad9e-4bf73afaa611.png)

Map 8. Math Scores by 2019 USA Average Crime Index

![Map 8  Math Scores by 2019 USA Average Crime Index](https://user-images.githubusercontent.com/50031745/216805730-f05c6ca0-8b9f-4a6b-b9f5-c1de797d38f6.png)

Map 9. Outside support and English Test Scores

![Map 9  Outside support and English Test Scores](https://user-images.githubusercontent.com/50031745/216805734-c31239cd-a86d-4c7d-98de-24426538770c.png)

Map 10. Outside support and English Test Scores

![Map 10  Outside support and English Test Scores](https://user-images.githubusercontent.com/50031745/216805743-128ccaeb-cd71-47d6-87af-59f73d3eabbd.png)

Map 11. School support and English Test Scores

![Map 11  School support and English Test Scores](https://user-images.githubusercontent.com/50031745/216805748-69ff2a4e-23d8-4e63-b19f-33bd6b21aa2f.png)

Map 12. School Support and Math Test Scores

![Map 12  School Support and Math Test Scores](https://user-images.githubusercontent.com/50031745/216805762-108e20d2-ab37-4d3e-b884-79b253c410ad.png)

Map 13. Home Support and English Test Scores

![Map 13  Home Support and English Test Scores](https://user-images.githubusercontent.com/50031745/216805776-7efccbcf-4945-4211-a6fa-2d08691324fd.png)

Map 13. Home Support and English Test Scores

![Map 13  Home Support and English Test Scores](https://user-images.githubusercontent.com/50031745/216805784-659b6297-36e7-4f9b-81e9-2238ed3b3f03.png)

Map 14. Home Support and Math Test Scores

![Map 14  Home Support and Math Test Scores](https://user-images.githubusercontent.com/50031745/216805794-5bd38629-9301-4914-8f1e-c4eb889ac326.png)

Table 1. Descriptive Statistics 

![Table 1  Descripriptive Statistics](https://user-images.githubusercontent.com/50031745/216805809-e085de2e-a750-4c83-af23-b3806b146dac.png)

Table 2. Logistic Regression 

![Table 2  Logistic Regression](https://user-images.githubusercontent.com/50031745/216805815-6a593717-018c-425d-84d8-ac61097804ac.png)

# References 
APA. (2012). Presidential Task Force on Educational Disparities. Ethnic and racial disparities in education: Psychology’s contributions to understanding and reducing disparities. Retrieved from http://www.apa.org/ed/resources/racial-disparities.aspx

APA. (2017). Ethnic and Racial Minorities & Socioeconomic Status. American Psychological Association. Retrieved from https://www.apa.org/pi/ses/resources/publications/minorities

Bayless, S. D., Jenson, J. M., Richmond, M. K., Pampel, F. C., Cook, M., & Calhoun, M. (2018). Effects of an Afterschool Early Literacy Intervention on the Reading Skills of Children in Public Housing Communities. Child & Youth Care Forum, 47(4), 537-561. doi:10.1007/s10566-018-9442-5

Casillas, A., Robbins, S., Allen, J., Kuo, Y.-L., Hanson, M. A., & Schmeiser, C. (2012). Predicting early academic failure in high school from prior academic achievement, psychosocial characteristics, and behavior. Journal of Educational Psychology, 104(2), 407. 

CDC. (2020, February 27, 2020). Adult Obesity Facts. Retrieved from https://www.cdc.gov/obesity/data/adult.html

Conger, R. D., Conger, K. J., & Martin, M. J. (2010). Socioeconomic Status, Family Processes, and Individual Development. Journal of marriage and the family, 72(3), 685-704. doi:10.1111/j.1741-3737.2010.00725.x

Cox, R. G., Zhang, L., Johnson, W. D., & Bender, D. R. (2007). Academic Performance and Substance Use: Findings From a State Survey of Public High School Students. Journal of School Health, 77(3), 109-115. doi:10.1111/j.1746-1561.2007.00179.x

Crede, J., Wirthwein, L., McElvany, N., & Steinmayr, R. (2015). Adolescents' academic achievement and life satisfaction: the role of parents' education. Frontiers in psychology, 6, 52-52. doi:10.3389/fpsyg.2015.00052

Crowe, J., Lacy, C., & Columbus, Y. (2018). Barriers to Food Security and Community Stress in an Urban Food Desert. Urban Science, 2(2). doi:10.3390/urbansci2020046

Cybele Raver, C., & Blair, C. (2016). Neuroscientific Insights: Attention, Working Memory, and Inhibitory Control. Future of Children, 26(2), 95-118. doi:10.1353/foc.2016.0014

Foster, C. E., Horwitz, A., Thomas, A., Opperman, K., Gipson, P., Burnside, A., . . . King, C. A. (2017). Connectedness to family, school, peers, and community in socially vulnerable adolescents. Children and youth services review, 81, 321-331. doi:10.1016/j.childyouth.2017.08.011

Frndak, S. E. (2014). An ecological study of food desert prevalence and 4th grade academic achievement in New York State school districts. Journal of Public Health Research, 3(3), 130-137. doi:10.4081/jphr.2014.319

Gómez-Pinilla, F. (2008). Brain foods: the effects of nutrients on brain function. Nature reviews. Neuroscience, 9(7), 568-578. doi:10.1038/nrn2421

Hahn, R. A., & Truman, B. I. (2015). Education Improves Public Health and Promotes Health Equity. International journal of health services : planning, administration, evaluation, 45(4), 657-678. doi:10.1177/0020731415585986

Haynie, D. L., Silver, E., & Teasdale, B. (2006). Neighborhood Characteristics, Peer Networks, and Adolescent Violence. Journal of Quantitative Criminology, 22(2), 147-169. doi:10.1007/s10940-006-9006-y

Herruzo, C., Raya Trenas, A., Pino, M. J., & Herruzo, J. (2020). Study of the Differential Consequences of Neglect and Poverty on Adaptive and Maladaptive Behavior in Children. Int J Environ Res Public Health, 17(3). doi:10.3390/ijerph17030739

Hilmers, A., Hilmers, D. C., & Dave, J. (2012). Neighborhood disparities in access to healthy foods and their effects on environmental justice. American journal of public health, 102(9), 1644-1654. doi:10.2105/AJPH.2012.300865

Hughes, J. N., Cao, Q., West, S. G., Allee Smith, P., & Cerda, C. (2017). Effect of retention in elementary grades on dropping out of school early. Journal of school psychology, 65, 11-27. doi:10.1016/j.jsp.2017.06.003

Hyde, J. S., & Mertz, J. E. (2009). Gender, culture, and mathematics performance. Proceedings of the National Academy of Sciences of the United States of America, 106(22), 8801-8807. doi:10.1073/pnas.0901265106

Jensen, E. (2009). Teaching with poverty in mind : what being poor does to kids&#39; brains and what schools can do about it: Alexandria, Va. : ASCD, [2009] ©2009.

Johnson, W., McGue, M., & Iacono, W. G. (2007). Socioeconomic Status and School Grades: Placing their Association in Broader Context in a Sample of Biological and Adoptive Families. Intelligence, 35(6), 526-541. doi:10.1016/j.intell.2006.09.006

Kim, S., Mazza, J., Zwanziger, J., & Henry, D. (2014). School and Behavioral Outcomes Among Inner City Children: Five-Year Follow-Up. Urban education, 49(7), 835-856. doi:10.1177/0042085913501895

Li, S., Xu, Q., & Xia, R. (2020). Relationship Between SES and Academic Achievement of Junior High School Students in China: The Mediating Effect of Self-Concept. Frontiers in psychology, 10, 2513-2513. doi:10.3389/fpsyg.2019.02513

Luttenberger, S., Wimmer, S., & Paechter, M. (2018). Spotlight on math anxiety. Psychology research and behavior management, 11, 311-322. doi:10.2147/PRBM.S141421

Manly, J. T., Lynch, M., Oshri, A., Herzog, M., & Wortel, S. N. (2013). The impact of neglect on initial adaptation to school. Child maltreatment, 18(3), 155-170. doi:10.1177/1077559513496144

Mayo, A., & Siraj, I. (2015). Parenting practices and children’s academic success in low-SES families. Oxford Review of Education, 41(1), 47-63. doi:10.1080/03054985.2014.995160

Meade, E. E. (2014). Overview of Community Characteristics in Areas with Concentrated Poverty Department of Health and Human Services. 

Moffitt, T. E., & Klaus-Grawe Think, T. (2013). Childhood exposure to violence and lifelong health: clinical intervention science and stress-biology research join forces. Development and psychopathology, 25(4 Pt 2), 1619-1634. doi:10.1017/S0954579413000801

Moradi Sheykhjan, T., & Jabari, K. (2014). Self-Esteem and Academic Achievement of High School Students. 2, 38. 

Navarro, J.-J., García-Rubio, J., & Olivares, P. R. (2015). The Relative Age Effect and Its Influence on Academic Performance. PloS one, 10(10), e0141895-e0141895. doi:10.1371/journal.pone.0141895

Neckerman, K. M., Lovasi, G. S., Davies, S., Purciel, M., Quinn, J., Feder, E., . . . Rundle, A. (2009). Disparities in urban neighborhood conditions: evidence from GIS measures and field observation in New York City. J Public Health Policy, 30 Suppl 1, S264-285. doi:10.1057/jphp.2008.47

Neff, R. A., Palmer, A. M., McKenzie, S. E., & Lawrence, R. S. (2009). Food Systems and Public Health Disparities. Journal of hunger & environmental nutrition, 4(3-4), 282-314. doi:10.1080/19320240903337041

Noonan, A. S., Velasco-Mondragon, H. E., & Wagner, F. A. (2016). Improving the health of African Americans in the USA: an overdue opportunity for social justice. Public health reviews, 37, 12-12. doi:10.1186/s40985-016-0025-4

Pechtel, P., & Pizzagalli, D. A. (2011). Effects of early life stress on cognitive and affective function: an integrated review of human literature. Psychopharmacology, 214(1), 55-70. doi:10.1007/s00213-010-2009-2

Rezaei-Dehaghani, A., Keshvari, M., & Paki, S. (2018). The Relationship between Family Functioning and Academic Achievement in Female High School Students of Isfahan, Iran, in 2013-2014. Iranian journal of nursing and midwifery research, 23(3), 183-187. doi:10.4103/ijnmr.IJNMR_87_17

Scaglioni, S., De Cosmi, V., Ciappolino, V., Parazzini, F., Brambilla, P., & Agostoni, C. (2018). Factors Influencing Children's Eating Behaviours. Nutrients, 10(6), 706. doi:10.3390/nu10060706

Topor, D. R., Keane, S. P., Shelton, T. L., & Calkins, S. D. (2010). Parent involvement and student academic performance: a multiple mediational analysis. Journal of prevention & intervention in the community, 38(3), 183-197. doi:10.1080/10852352.2010.486297

Widener, M. J., Metcalf, S. S., & Bar-Yam, Y. (2012). Developing a mobile produce distribution system for low-income urban residents in food deserts. J Urban Health, 89(5), 733-745. doi:10.1007/s11524-012-9677-7

