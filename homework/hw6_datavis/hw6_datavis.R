#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 6: Data visualization
#####################################
# Don't change these lines, just run them!
# Load okR autograder
tryCatch({source('setup/autograder-setup/hw6_datavis/hw6_datavis.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

# Note: since this homework assignment focuses on plotting,
# we can't rely on the autograder in the same way 
# as in other assignments because the autograder
# doesn't "look" at your plot, but rather just checks
# your R syntax. You can look at the correct plot output
# by visiting this URL, where the plots produced by the 
# solutions code are available: https://goo.gl/bPUkBo

# Read in the water quality dataset from the Surfer Health Study:
wq = read.csv(paste0(here::here(),"/data/Surfer-data/shs-wq-public.csv"))

# In this problem set we will investigate the relationship
# between precipitation and fecal coliform. We will 
# compare the relationship to the one we observed 
# between precipitation and total coliform in the 
# tutorial. Both are types of bacteria that are commonly
# used to measure water quality in epidemiologic studies. 

# Fecal coliforms are indicators of coliforms 
# that live in the intestinal track of humans 
# and other animals. Total coliforms include 
# fecal coliforms and other types of coliforms
# and can occur naturally in the environment 
# (ie, they don't necessarily come from human
# or animal waste).

# --------------------------------------------
# Load the ggplot2 package
# Hint: go back to the tutorial to review how to do this. 
# It's only one line of code. 
# --------------------------------------------
#<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>#


# --------------------------------------------
# Problem 1: Evaluate the distribution of fecal coliform

# Make a histogram with 50 bins, gray fill, 
# and black outline around each bar.
# Label the y-axis: "Number of observations"
# Label the x-axis: "Mean log 10 fecal coliform concentration in the past 3 days"
# --------------------------------------------
p1 = ggplot("#<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>#")
p1

# Check your answer
CheckProblem1()

# --------------------------------------------
# Problem 2: What does the distribution look like? 
# Answer by assigning q1 to "a", "b", or "c"
# a) Many more observations with small values 
# b) Close to normally distributed (like a bell curve) 
# c) Many more observations with large values 
# --------------------------------------------
p2 = #<< YOUR ANSWER HERE >>#

# Check your answer
CheckProblem2()

# --------------------------------------------
# Problem 3: Evaluate the relationship between precipitation and 
# fecal coliform concentration? 

# Make a scatter plot comparing precipitation on the x-axis
# to fecal coliform concentration on the y-axis. 

# Label the y-axis: "Mean log 10 fecal coliform concentration in the past 3 days"
# Label the x-axis: "Precipitation in the past 3 days (inches)"
# Set alpha = 0.3
# Set y-axis limits between 0 and 4.5
# Set x-axis limits between 0 and 2.7
# --------------------------------------------
p3 = ggplot("<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>")
p3 

# Check your answer
CheckProblem3()

# --------------------------------------------
# Problem 4: What does the relationship look like? 
# Answer by assigning q2 to "a", "b", or "c"
# a) As precipitation increases, fecal coliform concentration decreases somewhat
# b) As precipitation increases, fecal coliform concentration stays the same
# c) As precipitation increases, fecal coliform concentration increases somewhat
# --------------------------------------------
p4 = #<< YOUR ANSWER HERE >>#
  
# Check your answer
CheckProblem4()

# --------------------------------------------
# Problem 5: Review the plot of precipitation by 
# total coliform from the tutorial (or remake the plot 
# here). Which bacterium has a stronger relationship 
# wiht precipitation? 
# Answer by assigning q3 to "a", "b", or "c"
# a) Fecal coliform has a much stronger relationship with precipitation 
# b) The relationship is similar overall 
# c) Fecal coliform has a much weaker relationship with precipitation 
# --------------------------------------------
p5 = #<< YOUR ANSWER HERE >>#

# Check your answer
CheckProblem5()

# --------------------------------------------
# Problem 6: Evaluate whether fecal coliform concentration
# differs between Ocean Beach vs. other beaches


# Make a box plot comparing fecal coliform
# at Ocean Beach vs. other beaches.
# Label the y-axis title: "Mean log 10 fecal coliform concentration in the past 3 days"
# Label the x-axis values: "Ocean Beach" and "Other beaches"
# Label the x-axis title: "Beach"
# --------------------------------------------
# Create x-axis label variable and remove missing values
library(dplyr)
wq = wq %>% 
  mutate(ob.plot = ifelse(ob == 1, "Ocean Beach", "Other beaches")) %>%
  filter(!is.na(ob.plot))

# Make a box plot
p6 = ggplot("#<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>#")
p6

# Check your answer
CheckProblem6()

# --------------------------------------------
# Problem 7: Review the box plot you created in 
# the previous task. Was the median concentration
# of fecal coliform higher at Ocean Beach or 
# other beaches? 
# Answer by assigning q4 to "a", "b", or "c"
# a) Ocean Beach
# b) Other beaches
# --------------------------------------------
p7 = #<< YOUR ANSWER HERE >>#

# Check your answer
CheckProblem7()

# --------------------------------------------
# Check your total score
MyTotalScore()
# --------------------------------------------

# Follow the instructions on bCourses to submit your work.

######################################
# ALTERNATE SUBMISSION INSTRUCTIONS
# 1. Click on the "Terminal" tab in the panel below
# 2. Copy and paste the following line of code without the # and press "enter":
# cd; cd phw250fg2019/homework/hw6_datavis; python3 ok --submit;
# 3. Follow the prompts in the terminal
# Video tutorial: https://www.youtube.com/watch?v=NYNDi_zJRGE
######################################


