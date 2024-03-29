#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 0, R Setup
#################################################

# In this course, you'll be completing weekly R assignments that 
# will teach you how to use modern computing tools to complete
# epidemiological analysis. 

# For each assignment, we will be providing a series of checkpoints 
# that will allow you to check your work and receive automatic 
# feedback on your answers. In this short set-up assignment, you'll 
# get some practice interacting with the autograder and submitting
# your work.

# Load okR autograder - you'll see these two lines of code at the 
# top of each assignment. It sets up the checkpoints for you to use
# throughout the rest of the assignment.

# Don't change these lines, just run them!
# Load okR autograder
tryCatch({source('setup/autograder-setup/hw0_setup/hw0_setup.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

#-----------------------------------------------
# Problem 1: Set x to a mathematical equation that 
# evalutes to the number 5. 
# Replace "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
# with your answer. Run your newly written code.

# To run your code, click on the line you want to run 
# and press ctrl + enter on your keyboard
#-----------------------------------------------
x = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"

# Now try running the autograder!
CheckProblem1()

# Let's see what happens when you pass a wrong answer
# through the autograder. Try running all the cells below
# in order.

x = "5" 
CheckProblem1()


x = 2 + 2
CheckProblem1()


# Congratulations! You just finished your first R assignment.
# Follow the instructions on bCourses to submit your work.

######################################
# ALTERNATE SUBMISSION INSTRUCTIONS
# 1. Click on the "Terminal" tab in the panel below
# 2. Copy and paste the following line of code and press "enter":
cd; cd phw250fg2019/homework/hw0_setup; python3 ok --submit;
# 3. Follow the prompts in the terminal
# Video tutorial: https://www.youtube.com/watch?v=NYNDi_zJRGE
######################################

