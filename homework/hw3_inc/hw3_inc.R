#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 3: Incidence
#################################################
# Don't change these lines, just run them!
# Load okR autograder
tryCatch({source('setup/autograder-setup/hw3_inc/hw3_inc.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

#################################################
# Read in the data and view the data
#################################################
# Load the dplyr package
library(dplyr)

# In this problem set we will calculate incidence
# using different formulas. 

# Read in the data for this assignment. 
# The data is saved as a .RData file, so you need 
# to use the load command to load the data. 
# Fill the R data file name in the quotes below 
# without the data directory information.
load(paste0("data/hw3_incidence.RData"))

#################################################
# Section 1: Use the Kaplan-Meier method 
# to calculate the incidence in the data
# frame called "km"
#################################################
# First, display the data.frame "km"
km

#-----------------------------------------------
# Problem 1: calculate conditional risk as a new 
# column called "cond_risk" in the km dataframe
#-----------------------------------------------
km = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem1()

#-----------------------------------------------
# Problem 2: calculate conditional survival as a new 
# column in the km dataframe called cond_surv
#-----------------------------------------------
km = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem2()

#-----------------------------------------------
# Problem 3: calculate cumulative risk and save it  
# as a scalar named km_cum_risk
#-----------------------------------------------
km_cum_risk = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem3()

#-----------------------------------------------
# Problem 4: calculate cumulative survival and save it
# as a scalar named km_cum_surv
#-----------------------------------------------
km_cum_surv = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem4()

#################################################
# Section 2: Use the Density method 
# to calculate the incidence in the data
# frame called "dm"
#################################################
# display the data.frame "dm"
dm

#-----------------------------------------------
# Problem 5: calculate person-time using a formula that 
# assumes withdrawals occur halfway through each
# interval. Withdrawals are indicated in the column "W".
#-----------------------------------------------
delta_t=2
dm = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem5()

#-----------------------------------------------
# Problem 6: calculate the conditional risk as a new 
# column in the dm dataframe
#-----------------------------------------------
dm = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem6()

#-----------------------------------------------
# Problem 7: calculate cumulative risk  and save it  
# as a scalar named dm_cum_risk
#-----------------------------------------------
dm_cum_risk = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem7()

#-----------------------------------------------
# Problem 8: calculate cumulative survival and save it  
# as a scalar named dm_cum_surv
#-----------------------------------------------
dm_cum_surv = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem8()


# --------------------------------------------
# Check your total score
MyTotalScore()
# --------------------------------------------

# Follow the instructions on bCourses to submit your work.

######################################
# ALTERNATE SUBMISSION INSTRUCTIONS
# 1. Click on the "Terminal" tab in the panel below
# 2. Copy and paste the following line of code and press "enter":
cd; cd phw250fg2019/homework/hw3_inc; python3 ok --submit;
# 3. Follow the prompts in the terminal
# Video tutorial: https://www.youtube.com/watch?v=NYNDi_zJRGE
######################################

