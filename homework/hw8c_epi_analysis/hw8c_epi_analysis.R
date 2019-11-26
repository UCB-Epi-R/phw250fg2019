#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 8c: Epidemiologic data analysis
# homework #3: analysis 
#####################################
# Don't change these lines, just run them!
# Load okR autograder
tryCatch({source('setup/autograder-setup/hw8c_epi_analysis/hw8c_epi_analysis.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

library(dplyr)

#-----------------------------------------------
# Load data generated in the prior assignment
#-----------------------------------------------
# Load each dataset 
all = read.csv(paste0(here::here(),"/data/washb-data/washb-bangladesh-anthro-combined.csv"))

#-----------------------------------------------
# Problem 1: What measure of association do you 
# plan to estimate to explore this hypothesis? 
# Save your answer choice in an object called p1. 
# For example, you could type p1 = "a".

# a) Mean difference
# b) Risk difference
# c) Relative risk
# d) Odds ratio
#-----------------------------------------------
p1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
  
# Check your answer
CheckProblem1()

#-----------------------------------------------
# Problem 2: For a bivariable analysis of the 
# association between improved sanitation and 
# length-for-age Z-score, what is the appropriate 
# statistical approach?

# Save your result in an object called p2. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p2 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem2()

#-----------------------------------------------
# Problem 3: Perform the bivariable analysis 
# that you selected in Problem 2 to test whether
# the mean length-for-age Z-score (LAZ) among those
# with an improved latrine is statistically
# different from the mean among those without
# an improved latrine. 

# Your hypothesis does not specify whether one
# mean is larger than the other. 

# Name the t-test output ttest_laz
# Name the t-test test statistic ttest_laz_stat
# Name the t-test p-value ttest_laz_pvalue
#-----------------------------------------------
ttest_laz = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
ttest_laz_stat = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
ttest_laz_pvalue = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem3()

#-----------------------------------------------
# Problem 4: Based on the result of Problem 3,
# which of the following conclusions is correct
# at the alpha  = 0.05 level? 

# a) The difference in mean LAZ among those with
# and without improved latrines is statistically
# significant.

# b) The difference in mean LAZ among those with
# and without improved latrines is not statistically
# significant.

# Save your result in an object called p4.
#-----------------------------------------------
p4 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem4()

#-----------------------------------------------
# Problem 5: For a multivariable analysis of the 
# association between improved sanitation and 
# length-for-age Z-score, what is the appropriate 
# statistical approach?

# Save your result in an object called p5. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p5 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem5()

#-----------------------------------------------
# Problem 6: Fit a model to estimate the *unadjusted*
# measure of association for length-for-age Z-score  
# and improved latrines. 

# Fit the appropriate model based on 
# your response to Problem 5 to estimate the
# measure of association you selected in Problem 1. 

# For the purpose of this assignment, assume that 
# all assumptions required to correctly obtain 
# the appropriate measure of association have been met. 

# Name the model fit laz_unadj_model
#-----------------------------------------------
laz_unadj_model = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
summary(laz_unadj_model)

# Check your answer
CheckProblem6()

#-----------------------------------------------
# Problem 7: Using your model fit from Problem 6, 
# obtain the unadjusted measure of association. Save
# it as an object named laz_unadj_moa
#-----------------------------------------------
laz_unadj_moa = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
laz_unadj_moa

# Check your answer
CheckProblem7()

#-----------------------------------------------
# Problem 8: Using your model fit from Problem 7, 
# obtain the 95% confidence interval for the
# measure of association. Save it as a vector of 
# length two named laz_unadj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
laz_unadj_moa_ci = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
laz_unadj_moa_ci

# Check your answer
CheckProblem8()

#-----------------------------------------------
# Problem 8b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>

#-----------------------------------------------
# Problem 9: Fit a model to estimate the *adjusted*
# measure of association for length-for-age Z-score  
# and improved latrines. Adjust for the following
# variables: age in days, sex, month of data collection, 
# ownership of a refrigerator, mother's education level, 
# mother's height. 

# Fit the appropriate model based on 
# your response to Problem 5 to estimate the
# measure of association you selected in Problem 1. 

# Again, assume that all assumptions required to correctly  
# obtain the appropriate measure of association have been met. 

# Name the model fit laz_adj_model
#-----------------------------------------------
laz_adj_model = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem9()

#-----------------------------------------------
# Problem 10: Using your model fit from Problem 8, 
# obtain the adjusted measure of association. Save 
# it as an object named laz_adj_moa
#-----------------------------------------------
laz_adj_moa = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
laz_adj_moa

# Check your answer
CheckProblem10()

#-----------------------------------------------
# Problem 11: Using your model fit from Problem 9, 
# obtain the 95% confidence interval for the *adjusted* 
# measure of association. Save it as a vector of 
# length two named laz_adj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
laz_adj_moa_ci = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
laz_adj_moa_ci

# Check your answer
CheckProblem11()

#-----------------------------------------------
# Problem 11b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 
# Comment on how the adjusted measure compares
# to the unadjusted measure you obtained in 
# Problems 6-8.

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


#-----------------------------------------------
# Problem 12: Include an interaction term between 
# household refrigerator ownership and improved 
# latrines. Save the model fit as an object called
# laz_unadj_int_model
#-----------------------------------------------
laz_unadj_int_model = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"


# Check your answer
CheckProblem12()

#-----------------------------------------------
# Problem 13: Using your model fit from Problem 12, 
# obtain the coefficient from the interaction term.
# Save it as an object named laz_interaction
#-----------------------------------------------
laz_interaction = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"


# Check your answer
CheckProblem13()

#-----------------------------------------------
# Problem 13b: Interpret the coefficient you saved
# in problem 13. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


#-----------------------------------------------
# Problem 14: For a multivariable analysis of the 
# association between improved sanitation and 
# stunting (LAZ < -2), which of the following 
# approaches can be used to obtain the risk ratio? 

# Save your result in an object called p14. 

# a) Correlation test
# b) Chi-square test
# c) T-test
# d) ANOVA
# e) Logistic regression
# f) Log-linear regression
# g) Linear regression
#-----------------------------------------------
p14 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem14()

#-----------------------------------------------
# Problem 14b: What are the key assumptions of the 
# method selected in problem 14 that we learned in 
# this class? 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


#-----------------------------------------------
# Problem 15: Fit a model to estimate the *unadjusted*
# measure of association for stunting (LAZ < -2)
# and improved latrines. 

# Fit the appropriate model based on 
# your response to Problem 14.

# Again, assume that all assumptions required to correctly  
# obtain the appropriate measure of association have been met. 

# Name the model fit stunt_unadj_model
#-----------------------------------------------
stunt_unadj_model = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem15()

#-----------------------------------------------
# Problem 16: Using your model fit from Problem 15, 
# obtain the unadjusted risk ratio. Save 
# it as an object named stunt_unadj_moa
#-----------------------------------------------
stunt_unadj_moa = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem16()

#-----------------------------------------------
# Problem 17: Using your model fit from Problem 15, 
# obtain the 95% confidence interval for the *unadjusted* 
# measure of association. Save it as a vector of 
# length two named stunt_unadj_moa_ci, where the first
# element of the vector is the lower bound, and 
# the second element is the upper bound.
#-----------------------------------------------
stunt_unadj_moa_ci = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem17()

#-----------------------------------------------
# Problem 17b: Interpret this unadjusted measure of 
# association, assessing the magnitude of the unadjusted
# measure of association as well as the 95% CI for it. 

# This is a short answer question. Enter your
# response using a hash (#) at the beginning of 
# each line. Your response will be graded upon 
# submission to okpy. 
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


#-----------------------------------------------
# Problem 17c: Reflect on the results of the 
# analyses in this assignment. What do you conclude
# about the relationship between improved latrines
# (as defined here) and linear growth (as measured
# by height-for-age Z-score and stunting)?
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


#-----------------------------------------------
# Problem 17d: What are the threats to validity 
# in the analyses carried out in this assignment?
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


#-----------------------------------------------
# Problem 17e: How do the results of this analysis
# compare to what the WASH Benefits Bangladesh
# trial found in the sanitation arm? If results
# are different, what do you think accounts for
# the discrepancy? 
#-----------------------------------------------

# <<<<<<<<<<<<< YOUR ANSWER HERE >>>>>>>>>>>>>>>


# --------------------------------------------
# Check your total score (excludes short answer
# questions, which will be graded in gradescope)
MyTotalScore()
# --------------------------------------------

# Follow the instructions on bCourses to submit your work.

######################################
# ALTERNATE SUBMISSION INSTRUCTIONS
# 1. Click on the "Terminal" tab in the panel below
# 2. Copy and paste the following line of code without the # and press "enter":
# cd; cd phw250fg2019/homework/hw8c_epi_analysis; python3 ok --submit;
# 3. Follow the prompts in the terminal
# Video tutorial: https://www.youtube.com/watch?v=NYNDi_zJRGE
######################################

