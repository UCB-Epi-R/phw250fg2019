#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 8b: Epidemiologic data analysis
# homework #2: analysis preparation
#####################################
# Don't change these lines, just run them!
# Load okR autograder
tryCatch({source('setup/autograder-setup/hw8b_epi_analysis/hw8b_epi_analysis.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

# NOTE: In this homework, in order for the autograder
# to run correctly, you must run the code for each
# problem in order. If you have trouble with the 
# autograder, try running your code again starting 
# from line 10.

#-----------------------------------------------
# Problem 1: Load the packages you will need for
# this analysis
#-----------------------------------------------


# Check your answer
CheckProblem1()

#-----------------------------------------------
# Problem 2: Read in the three required datasets 
# for this analysis and name them as follows: 

# treatment: washb-bangladesh-tr-public.csv
# enroll: washb-bangladesh-enrol-public.csv
# anthro: washb-bangladesh-anthro-public.csv

# Hint: use code from previous homework assignments. 
# If you have trouble, make sure your code is
# pointing to the correct directory, and make sure you
# opened the phw250fgfall2019 R Project before 
# getting started. 
#-----------------------------------------------
# Load each dataset 
treatment = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
enroll = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
anthro = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem2()

#-----------------------------------------------
# Problem 3: Merge together the treatment and 
# enroll data frames. Name the merged data frame
# enroll_tr
#-----------------------------------------------
enroll_tr = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
  
# Check your answer
CheckProblem3()

#-----------------------------------------------
# Problem 4: Merge together the enroll_tr and 
# anthro data frames. Name the merged data frame
# all.  
#-----------------------------------------------
all = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem4()

#-----------------------------------------------
# Problem 5: Subset the data frame all to only 
# the control arm
#-----------------------------------------------
all = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem5()

#-----------------------------------------------
# Problem 6: Subset the data frame all to only
# observations collected at the 2 year measurement
# time point (endline)
#-----------------------------------------------
all = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem6()


#-----------------------------------------------
# Problem 7: Make a variable called “implatrine” 
# in the all data frame that is a factor variable
# equal to "Has improved latrine" if the household owns 
# a latrine with a functional water seal and equal
# to "Does not have improved latrine" if they do not. 
# If the value for latseal is missing, code the 
# implatrine variable as "Does not have improved latrine".
#-----------------------------------------------
all = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem7()


#-----------------------------------------------
# Problem 8: Subset the data frame all by removing
# any rows for people who had missing laz or missing
# implatrine
#-----------------------------------------------
all = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem8()


#-----------------------------------------------
# Problem 9: Create a histogram showing the 
# distribution of the length-for-age
# Z-score. Label the x-axis "Length-for-age Z-score".
# Name the plot object "laz_histogram". 
# Make the bars have a gray fill and a black outline. 
#-----------------------------------------------
laz_histogram = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

laz_histogram 

# Check your answer
CheckProblem9()


#-----------------------------------------------
# Problem 9b: Examine the distribution in the histogram
# you created. What do you conclude about the 
# nutritional status of children in this study 
# at endline? Recall that children who have LAZ< -2
# are considered to be "stunted" (ie, shorter than
# is considered normal for their age and sex). 

# Write 1-2 sentences including line breaks, and use
# the "hash" symbol (#) at the beginning of each line
# to comment out your answer. 
#-----------------------------------------------


#-----------------------------------------------
# Problem 10: Create a box plot for the length-for-age 
# Z-score within categories of implatrine. Label
# the x-axis "Type of latrine" and the y-axis 
# "Length-for-age Z-score". Name the plot object
# "laz_lat_boxplot". 
#-----------------------------------------------
laz_lat_boxplot = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

laz_lat_boxplot

# Check your answer
CheckProblem10()

#-----------------------------------------------
# Problem 10b: Examine the boxplot you created. 
# What do you conclude about the distribution of 
# LAZ among those with vs. without improved latrines?

# Write 1-2 sentences including line breaks, and use
# the "hash" symbol (#) at the beginning of each line
# to comment out your answer. 
#-----------------------------------------------


#-----------------------------------------------
# Problem 11: 
# a) Calculate the mean LAZ among those
# with improved latrines and name it mean_laz_implat1.

# b) Calculate the mean LAZ among those without
# improved latrines and name it mean_laz_implat0.

# c) Calculate the mean difference in LAZ among 
# those with and without improved latrines. Name 
# the results object diff_mean_laz_implat. 

# The autograder will only check diff_mean_laz_implat.
#-----------------------------------------------
mean_laz_implat1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
mean_laz_implat0 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

diff_mean_laz_implat = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
diff_mean_laz_implat

# Check your answer
CheckProblem11()

#-----------------------------------------------
# Problem 11b: Examining the mean in each category
# of improved latrine, what do you conclude 
# about the crude association between improved
# latrine ownership and length-for-age? 

# Write 1-2 sentences including line breaks, and use
# the "hash" symbol (#) at the beginning of each line
# to comment out your answer. 
#-----------------------------------------------



#-----------------------------------------------
# Problem 12: Assess confounding by a variable
# that serves as a proxy for household wealth:
# refrigerator ownership. This will allow you to 
# assess confounding of the mean difference by 
# household refrigerator ownership. 

# a) Calculate the mean LAZ among those whose 
# household owns an improved latrine and a refrigerator.
# Name it mean_laz_implat1_refrig1.

# b) Calculate the mean LAZ among those whose 
# household does not owns an improved latrine
# and does own a refrigerator.
# Name it mean_laz_implat0_refrig1.

# c) Calculate the mean difference in LAZ among those
# with and without latrines among those whose
# household owns a refrigerator.
# Hint: use your answers from a) and b).
# Name it diff_mean_laz_implat_refrig1.

# d) Calculate the mean LAZ among those whose 
# household owns an improved latrine and does not own a 
# refrigerator.
# Name it mean_laz_implat1_refrig0.

# e) Calculate the mean LAZ among those whose 
# household does not owns an improved latrine
# and does not own a refrigerator.
# Name it mean_laz_implat0_refrig0.

# f) Calculate the mean difference in LAZ among those
# with and without latrines among those whose
# household does not own a refrigerator.
# Hint: use your answers from d) and e).
# Name it diff_mean_laz_implat_refrig0.

# The autograder will only check diff_mean_laz_implat_refrig1
# and diff_mean_laz_implat_refrig0
#-----------------------------------------------
mean_laz_implat1_refrig1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
mean_laz_implat0_refrig1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

mean_laz_implat1_refrig0 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
mean_laz_implat0_refrig0 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

diff_mean_laz_implat_refrig1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
diff_mean_laz_implat_refrig0 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

diff_mean_laz_implat_refrig1
diff_mean_laz_implat_refrig0

# Check your answer
CheckProblem12()

#-----------------------------------------------
# Problem 12b: Examining the mean in each category
# of improved latrine stratified by household 
# refrigerator ownership, what do you conclude 
# about confounding of the association between 
# improved latrine ownership and length-for-age 
# by household refrigerator ownership? 
#-----------------------------------------------



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
# cd; cd phw250fg2019/homework/hw8b_epi_analysis; python3 ok --submit;
# 3. Follow the prompts in the terminal
# Video tutorial: https://www.youtube.com/watch?v=NYNDi_zJRGE
######################################

