#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 4: Functions
#####################################
# Don't change these lines, just run them!
# Load okR autograder
tryCatch({source('setup/autograder-setup/hw4_functions/hw4_functions.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

# In this assignment, we'll use the same 2x2 table
# from the risk difference tutorial to calculate
# other measures of association.
a = 5
b = 10
c = 20
d = 10

#------------------------------------
# Problem 1: Calculate the relative risk
# In the risk difference tutorial, we defined
# a function that calculated the risk difference.
# Now you need to modify the risk difference
# function code below to calculate the relative
# risk instead. The function arguments will be the same --
# cells of the 2x2 table (a, b, c, and d). 
# The output will be the RR. Call the function 
# estimate_rr
#------------------------------------
estimate_rr="<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem1()

#------------------------------------
# Problem 2: Apply the function estimate_rr to the values
# a, b, c, and d defined above. Save the
# results in an object called p2. 
#------------------------------------
p2 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem2()

#------------------------------------
# Problem 3: Estimate a 95% confidence interval
# for the relative risk

# Now modify the function we wrote for the 
# confidence interval of the risk difference
# to calculate the CI for the relative risk. 
# The function will take 2x2 cells as arguments
# and return the lower and upper bound of 
# the interval.
# Call the function estimate_rr_ci 

# Hint: the SE equation is: sqrt((1/a) + (1/c))

# Be sure to report the CI on the correct scale
# (i.e., not on the ln scale but on the RR scale)
# In R, the natural log can be calculated using
# the "log" function (NOT "ln")

# Note: the autograder for this problem is 
# only checking that the function is set up
# correctly - not that the calculation is right!
# If the function is set up correctly, but your
# math is wrong, Problem 3 will show up as 
# correct, but Problem 4 will show up as incorrect. 
#------------------------------------
estimate_rr_ci = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem3()

#------------------------------------
# Problem 4: Apply the function estimate_rr_ci to the 
# values a, b, c, and d defined above. Save the
# lower bound of the results in an object
# called p4_lb and the upper bound in an object
# called p4_ub
#------------------------------------
p4_lb = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
p4_ub = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem4()

#------------------------------------
# Problem 5: Calculate the odds ratio

# Now write a new function to calculate the
# odds ratio. Use the 2x2 table cells as agruments.
# Call the function estimate_or
#------------------------------------
estimate_or = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>" 

# Check your answer
CheckProblem5()

#------------------------------------
# Problem 6: Apply the function estimate_rr_ci to the values
# a, b, c, and d defined above. Save the
# results in an object called p4. 
#------------------------------------
p6 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem6()

#------------------------------------
# Problem 7: Estimate the 95% confidence interval
# for the odds ratio

# Now write a new function to calculate the
# 95% CI for the OR. Use the 2x2 table cells as arguments.
# Call the function estimate_or_ci
# Be sure to report the CI on the correct scale
# (i.e., not on the ln scale but on the OR scale)

# Hint: the SE equation is: sqrt((1/a)+(1/b)+(1/c)+(1/d))
#------------------------------------
estimate_or_ci = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem7()

#------------------------------------
# Problem 8: Apply the function estimate_or_ci to the values
# a, b, c, and d defined above. Save the
# lower bound of the results in an object
# called p8_lb and the upper bound in an object
# called p8_ub
#------------------------------------
p8_lb = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
p8_ub = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem8()

#------------------------------------
# Problem 9: Calculate the population attributable risk
# (R_total - R_unexposed)

# Write a function that uses 2x2 table cells
# as arguments and returns the PAR.
# Call the function par.fn
#------------------------------------
estimate_par = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem9()

#------------------------------------
# Problem 10: Apply the function estimate_par to the values
# a, b, c, and d defined above. Save the
# results in an object called p10. 
#------------------------------------
# apply function to 2x2 table
p10 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Check your answer
CheckProblem10()

# --------------------------------------------
# Check your total score
MyTotalScore()
# --------------------------------------------

# Follow the instructions on bCourses to submit your work.

######################################
# ALTERNATE SUBMISSION INSTRUCTIONS
# 1. Click on the "Terminal" tab in the panel below
# 2. Copy and paste the following line of code without the # and press "enter":
# cd; cd phw250fg2019/homework/hw4_functions; python3 ok --submit;
# 3. Follow the prompts in the terminal
# Video tutorial: https://www.youtube.com/watch?v=NYNDi_zJRGE
######################################


