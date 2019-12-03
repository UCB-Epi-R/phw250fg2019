#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Solutions: Epidemiologic data analysis
# homework #3: analysis 
#####################################
# Load okR autograder
# devtools::source_gist(id='70cb79937ffd5067444c5d364230fc93', filename='hw_epi_analysis_2.ok.R', quiet = TRUE)
# AutograderInit()

# ADD T TEST

#-----------------------------------------------
# Problem 1: Load the packages you will need for
# this analysis
#-----------------------------------------------
library(dplyr)
library(ggplot2)

# Check your answer
# CheckProblem1()

#-----------------------------------------------
# Load data generated in the prior assignment
#-----------------------------------------------
# Load each dataset 
treatment = read.csv("data/washb-data/washb-bangladesh-tr-public.csv")
enroll = read.csv("data/washb-data/washb-bangladesh-enrol-public.csv")
anthro = read.csv("data/washb-data/washb-bangladesh-anthro-public.csv")

enroll_tr = full_join(enroll, treatment, by=c("block","clusterid"))
all = inner_join(enroll_tr, anthro, by=c("block","clusterid","dataid"))
all = all %>% filter(tr == "Control")
all = all %>% filter(svy == 2)
all = all %>% mutate(implatrine = factor(ifelse(latown==1 & latseal==1, 
              "Has improved latrine", "Does not have improved latrine")))
all = all %>% 
  filter(!is.na(laz)) %>%
  filter(!is.na(implatrine))


#-----------------------------------------------
# Problem 2: What measure of association do you 
# plan to estimate to explore this hypothesis? 
# Save your result in an object called p2.

# a) Mean difference
# b) Risk difference
# c) Relative risk
# d) Odds ratio
#-----------------------------------------------
p2 = "a"
  
# Check your answer
# CheckProblem2()

#-----------------------------------------------
# Problem 3: For a bivariable analysis of the 
# association between improved sanitation and 
# length-for-age Z-score, what is the appropriate 
# statistical approach?

# Save your result in an object called p3. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p3 = "c"

# Check your answer
# CheckProblem3()

#-----------------------------------------------
# Problem 4: For a multivariable analysis of the 
# association between improved sanitation and 
# length-for-age Z-score, what is the appropriate 
# statistical approach?

# Save your result in an object called p4. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p4 = "g"

# Check your answer
# CheckProblem4()

#-----------------------------------------------
# Problem 5: Fit a model to estimate the *unadjusted*
# measure of association for length-for-age Z-score  
# and improved latrines. 

# Fit the appropriate model based on 
# your response to Problem 4a to estimate the
# measure of association you selected in Problem 2. 

# For the purpose of this assignment, assume that 
# all assumptions required to correctly obtain 
# the appropriate measure of association have been met. 

# Name the model fit laz_unadj_model
#-----------------------------------------------
laz_unadj_model = glm(laz ~ implatrine, data = all)
summary(laz_unadj_model)

# Check your answer
# CheckProblem5()

#-----------------------------------------------
# Problem 6: Using your model fit from Problem 5, 
# obtain the unadjusted measure of association. Save
# it as an object named laz_unadj_moa
#-----------------------------------------------
laz_unadj_moa = laz_unadj_model$coefficients[2]
laz_unadj_moa

# Check your answer
# CheckProblem6()

#-----------------------------------------------
# Problem 7: Using your model fit from Problem 5, 
# obtain the 95% confidence interval for the
# measure of association. Save it as a vector of 
# length two named laz_unadj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
# TO DO
# laz_unadj_moa_ci
# laz_unadj_moa_ci

# Check your answer
# CheckProblem7()

#-----------------------------------------------
# Problem 7b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------


#-----------------------------------------------
# Problem 8: Fit a model to estimate the *adjusted*
# measure of association for length-for-age Z-score  
# and improved latrines. Adjust for the following
# variables: age in days, sex, month of data collection, 
# ownership of a refrigerator, mother's education level, 
# mother's height. 

# Fit the appropriate model based on 
# your response to Problem 4a to estimate the
# measure of association you selected in Problem 2. 

# Again, assume that all assumptions required to correctly  
# obtain the appropriate measure of association have been met. 

# Name the model fit laz_adj_model
#-----------------------------------------------
laz_adj_model = glm(laz ~ implatrine + aged + sex + month + asset_refrig + momedu + momheight, data = all)

# Check your answer
# CheckProblem8()

#-----------------------------------------------
# Problem 9: Using your model fit from Problem 8, 
# obtain the adjusted measure of association. Save 
# it as an object named laz_adj_moa
#-----------------------------------------------
laz_adj_moa = laz_adj_model$coefficients[2]
laz_adj_moa

#-----------------------------------------------
# Problem 10: Using your model fit from Problem 8, 
# obtain the 95% confidence interval for the *adjusted* 
# measure of association. Save it as a vector of 
# length two named laz_adj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
# TO DO
# laz_adj_moa_ci
# laz_adj_moa_ci

# Check your answer
# CheckProblem10()

#-----------------------------------------------
# Problem 10b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

#-----------------------------------------------
# Problem 11: Include an interaction term between 
# household refrigerator ownership and improved 
# latrines. Save the model fit as an object called
# laz_unadj_int_model
#-----------------------------------------------
all = all %>% mutate(mom_edu_2 = as.factor(ifelse(momedu == "Secondary (>5y)", 1, 0)))

laz_unadj_int_model = glm(laz ~ implatrine*mom_edu_2, data = all)


# Check your answer
# CheckProblem11()

#-----------------------------------------------
# Problem 12: For a multivariable analysis of the 
# association between improved sanitation and 
# stunting (LAZ < -2), which of the following 
# approaches can be used to obtain the risk ratio? 

# Save your result in an object called p12. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p12 = "f"

# Check your answer
# CheckProblem12()

#-----------------------------------------------
# Problem 12b: What are the key assumptions of the 
# method mentioned in problem 12 that we learned in 
# this class? 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------
"No auto-correlation, no dependency in the data"


#-----------------------------------------------
# Problem 13: Fit a model to estimate the *unadjusted*
# measure of association for stunting (LAZ < -2)
# and improved latrines. 

# Fit the appropriate model based on 
# your response to Problem 12.

# Again, assume that all assumptions required to correctly  
# obtain the appropriate measure of association have been met. 

# Name the model fit stunt_unadj_model
#-----------------------------------------------
stunt_unadj_model = glm(lazminus2 ~ implatrine, data = all, family=poisson(link="log"))

# Check your answer
# CheckProblem13()

#-----------------------------------------------
# Problem 14: Using your model fit from Problem 13, 
# obtain the unadjusted risk ratio. Save 
# it as an object named stunt_unadj_moa
#-----------------------------------------------
stunt_unadj_moa = exp(summary(stunt_unadj_model)$coefficients[2,"Estimate"])

#-----------------------------------------------
# Problem 15: Using your model fit from Problem 12, 
# obtain the 95% confidence interval for the *unadjusted* 
# measure of association. Save it as a vector of 
# length two named stunt_unadj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
# TO DO 
# stunt_unadj_moa_ci
# stunt_unadj_moa_ci

#-----------------------------------------------
# Problem 15b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

#-----------------------------------------------
# Problem 15c: Reflect on the results of the 
# analyses in this assignment. What do you conclude
# about the relationship between improved latrines
# (as defined here) and linear growth (as measured
# by height-for-age Z-score and stunting)?
#-----------------------------------------------

#-----------------------------------------------
# Problem 15d: What are the threats to validity 
# in the analyses carried out in this assignment?
#-----------------------------------------------

#-----------------------------------------------
# Problem 15e: How do the results of this analysis
# compare to what the WASH Benefits Bangladesh
# trial found in the sanitation arm? If results
# are different, what do you think accounts for
# the discrepancy? 
#-----------------------------------------------


# --------------------------------------------
# Check your total score (excludes short answer
# questions, which will be graded in okpy)
# MyTotalScore()
# --------------------------------------------


