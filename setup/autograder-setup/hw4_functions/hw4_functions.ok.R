#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Homework 4, Functions
###############################################

source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(10)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = (exists("estimate_rr") & is.function(estimate_rr)), 
             correct_message = "estimate_rr has been set to a function",
             error_message = "Did you create a function called estimate_rr?")
  
  CheckPoint(checkpoint_number = 2,
             test = has_args(estimate_rr, c("a","b","c","d"), exact=TRUE), 
             correct_message = "The correct arguments have been passed through estimate_rr",
             error_message = "Did you enter the correct arguments to the function (a, b, c, d)?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem2 = function() {
  problemNumber = 2
  num_tests <<- 1
  tests_failed <<- num_tests

  CheckPoint(checkpoint_number = 1,
             test = p2 == 0.5, 
             correct_message = "Final answer calls the estimate_rr function correctly",
             error_message = "Check that your estimate_rr function uses the correct formula and that
                              your function call entered the correct arguments.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem3 = function() {
  problemNumber = 3
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = (exists("estimate_rr_ci") & is.function(estimate_rr_ci)), 
             correct_message = "estimate_rr_ci has been set to a function",
             error_message = "Did you create a function called estimate_rr_ci?")
  
  CheckPoint(checkpoint_number = 2,
             test = has_args(estimate_rr_ci, c("a","b","c","d"), exact=TRUE), 
             correct_message = "The correct arguments have been passed through estimate_rr_ci",
             error_message = "Did you enter the correct arguments to the function (a, b, c, d)?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem4 = function() {
  problemNumber = 4
  num_tests <<- 5
  tests_failed <<- num_tests
  
  estimate_rr_ci_correct=readRDS("setup/autograder-setup/hw4_functions/estimate_rr_ci_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = length(p4_lb) == 1, 
             correct_message = "p4_lb only contains one number",
             error_message = "p4_lb should only contain one number.")
  
  CheckPoint(checkpoint_number = 2,
             test = length(p4_ub) == 1, 
             correct_message = "p4_ub only contains one number",
             error_message = "p4_ub should only contain one number.")

  CheckPoint(checkpoint_number = 3,
             test = p4_lb < p4_ub & length(p4_lb) == 1 & length(p4_ub) == 1, 
             correct_message = "p4_lb is less than p4_ub",
             error_message = "p4_lb should be less than p4_ub. You may have saved your lower bound as p4_up and/or upper bound as p4_lb")
  
  CheckPoint(checkpoint_number = 4,
             test = round(p4_lb,3) == round(estimate_rr_ci_correct(5, 10, 20, 10)[1],3) & length(p4_lb) == 1, 
             correct_message = "The value of p4_lb is correct",
             error_message = "Your value for p4_lb is incorrect. Check that your function uses the correct formula and that
                  your function call entered the correct arguments.")
  
  CheckPoint(checkpoint_number = 5,
             test = round(p4_ub,3) == round(estimate_rr_ci_correct(5, 10, 20, 10)[2],3) & length(p4_ub) == 1,
             correct_message = "The value of p4_ub is correct",
             error_message = "Your value for p4_ub is incorrect. Check that your function uses the correct formula and that
                  your function call entered the correct arguments.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem5 = function() {
  problemNumber = 5
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = (exists("estimate_or") & is.function(estimate_or)), 
             correct_message = "estimate_or has been set to a function",
             error_message = "Did you create a function called estimate_or?")
  
  CheckPoint(checkpoint_number = 2,
             test = has_args(estimate_or, c("a","b","c","d"), exact=TRUE), 
             correct_message = "The correct arguments have been passed through estimate_or",
             error_message = "Did you enter the correct arguments to the function (a, b, c, d)?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem6 = function() {
  problemNumber = 6  
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p6 == 0.25, 
             correct_message = "Final answer calls the estimate_or function correctly",
             error_message = "Check that your estimate_or function uses the correct formula and that your function call entered the correct arguments.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem7 = function() {
  problemNumber = 7
  num_tests <<- 2
  tests_failed <<- num_tests

  CheckPoint(checkpoint_number = 1,
             test = (exists("estimate_or_ci") & is.function(estimate_or_ci)), 
             correct_message = "estimate_or_ci has been set to a function",
             error_message = "Did you create a function called estimate_or_ci?")
  
  CheckPoint(checkpoint_number = 2,
             test = has_args(estimate_or_ci, c("a","b","c","d"), exact=TRUE), 
             correct_message = "The correct arguments have been passed through estimate_or_ci",
             error_message = "Did you enter the correct arguments to the function (a, b, c, d)?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem8 = function() {
  problemNumber = 8  
  num_tests <<- 5
  tests_failed <<- num_tests
  
  estimate_or_ci_correct=readRDS("setup/autograder-setup/hw4_functions/estimate_or_ci_correct.RDS")

  CheckPoint(checkpoint_number = 1,
             test = length(p8_lb) == 1, 
             correct_message = "p8_lb only contains one number",
             error_message = "p8_lb should only contain one number.")
  
  CheckPoint(checkpoint_number = 2,
             test = length(p8_ub) == 1, 
             correct_message = "p8_ub only contains one number",
             error_message = "p8_ub should only contain one number.")
  
  CheckPoint(checkpoint_number = 3,
             test = p8_lb < p8_ub & length(p8_lb) == 1 & length(p8_ub) == 1, 
             correct_message = "p8_lb is less than p8_ub",
             error_message = "p8_lb should be less than p8_ub. You may have saved your lower bound as p8_ub and/or upper bound as p8_lb")
  
  CheckPoint(checkpoint_number = 4,
             test = round(p8_lb,3) == round(estimate_or_ci_correct(5, 10, 20, 10)[1],3) & length(p8_lb) == 1, 
             correct_message = "The value of p8_lb is correct",
             error_message = "Your value for p8_lb is incorrect. Check that your function uses the correct formula and that
             your function call entered the correct arguments.")
  
  CheckPoint(checkpoint_number = 5,
             test = round(p8_ub,3) == round(estimate_or_ci_correct(5, 10, 20, 10)[2],3) & length(p8_ub) == 1,
             correct_message = "The value of p8_ub is correct",
             error_message = "Your value for p8_ub is incorrect. Check that your function uses the correct formula and that
             your function call entered the correct arguments.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem9 = function() {
  problemNumber = 9
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = (exists("estimate_par") & is.function(estimate_par)), 
             correct_message = "estimate_par has been set to a function",
             error_message = "Did you create a function called estimate_par?")
  
  CheckPoint(checkpoint_number = 2,
             test = has_args(estimate_par, c("a","b","c","d"), exact=TRUE), 
             correct_message = "The correct arguments have been passed through estimate_par",
             error_message = "Did you enter the correct arguments to the function (a, b, c, d)?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


# --------------------------------------------
CheckProblem10 = function() {
  problemNumber = 10
  num_tests <<- 1
  tests_failed <<- num_tests
  
  estimate_par_correct = readRDS("setup/autograder-setup/hw4_functions/estimate_par_correct.RDS")

  CheckPoint(checkpoint_number = 1,
             test = round(p10,3) == round(estimate_par_correct(5, 10, 20, 10),3), 
             correct_message = "Final answer calls the estimate_par function correctly",
             error_message = "Check that your estimate_par function uses the correct formula and that your function call entered the correct arguments.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


