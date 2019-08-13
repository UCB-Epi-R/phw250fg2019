######################################################
# Welcome to Public Health 250F! 
# In this course, you'll learn how to use R to complete 
# data analysis in epidemiology. In your first assignment, 
# you'll learn the basics of the language. 

# You can receive credit for this assignment by completing 
# one of the following:
#   (1) Complete the datacamp tutorials for both R and 
#       tidyverse and upload a certifcate of completion 
#       on bCourses.

#   (2) Completing this quiz

# For each question below, replace 
# "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
# with your answer. For example, if your answer to Question 1
# is (a), replace q_1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
# with q_1 = "a"
######################################################
tryCatch({source('setup/autograder-setup/hw1_r_quiz/hw1_r_quiz.ok.R')},
         warning = function(e){print("Error: did you remember to load the phw250fg2019.Rproj file?")})
AutograderInit()

######################################################
# *Question 1*
# How do we load the "dplyr" package into our R programming environment? 
# Assign `q_1` to the character corresponding your answer choice.
#  a.	load(dplyr)
#  b.	launch(dplyr)
#  c.	package(dplyr)
#  d.	library(dplyr)
######################################################

q_1 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem1()

######################################################
# *Question 2*
# How do we load the dataset "my_data.csv" into our environment? 
# Assign `q_2` to the character corresponding your answer choice.
#  a. load(my_data.csv)
#  b. read.csv("my_data.csv")
#  c. launch(my_data.csv)
#  d. load("my_data.csv")
######################################################
  
q_2 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem2()

######################################################
# *Question 3*
# How do we load the dataset "my_data.RData" into our environment? 
# Assign `q_3` to the character corresponding your answer choice.
#  a. load(my_data.RData)
#  b. launch(my_data.RData)
#  c. load("my_data.RData")
#  d. read.csv("my_data.RData")
######################################################

q_3 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem3()

######################################################
# *Question 4*
# Assign `q_4` to a vector with the following numerics: 
# 2, 2 times 5, 3 divided by 4, and 4 to the 5th power. 
######################################################

q_4 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem4()

######################################################
# *Question 5*
# Assign `q_5` to a vector containing the second, third, 
# and fourth item of the object list_of_numbers, without using c().
######################################################
list_of_numbers = c(0, 2, 10, 12, 4, 43, 6)
q_5 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>" 
CheckProblem5()

######################################################
# *Question *
# Assign the column names of a data frame called df 
# to q_6.
######################################################

df = readRDS("df.RDS")
q_6 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem6()

######################################################
# For the following question, assume that we have the 
# table `q_7`, describing a store's inventory with the columns:
#  - `item_name`: the name of the item
#  - `amount_remaining`: number of items remaining
#  - `price_per_item`: cost of one item
# Using base R methods, add a column to `q_7` called 
# `total_cost`, which contains the total price of all 
# the remaining items for each `item_name`.
######################################################

q_7 = readRDS("q_7.RDS")
q_7 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>" 
CheckProblem7()

######################################################
# *Question 8*
# What function can we use to add columns to our data frame? 
# Assign `q_8` to the character corresponding your answer choice.
#  a. mutate(...)
#  b. add_col(...)
#  c. summarize(...)
#  d. combine(...)
######################################################

q_8 = "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem8()

######################################################
# For the following questions, use the data table 
# `student_info`, which contains the columns:
#  - `studentID`: integers with student ID numbers
#  - `name`: strings with student names
#  - `major`: strings of student majors. Assume that 
# students only have one major
#  - `age`: integers with student ages
#  - `terms`: integers with the number of semesters 
# students have been in attendance
#  - `gpa`: integers with student GPAs
######################################################

student_info = readRDS("student_info.RDS")

######################################################
# *Question 9*
# Assign `q_6` to a data table containing rows from 
# `student_info` for students who are Public Health majors.
######################################################

q_9 = student_info %>% "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem9()

######################################################
# *Question 10*
# Assign `q_10` to a copy of `student_info` sorted on 
# age in descending order.
######################################################

q_10 = student_info %>% "<<<<<<<<<<<<< YOUR CODE HERE >>>>>>>>>>>>>>>"
CheckProblem10()

# --------------------------------------------
# Check your total score
MyTotalScore()
# --------------------------------------------

