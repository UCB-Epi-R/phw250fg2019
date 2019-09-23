#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Homework 5, Iteration
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(7)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  num_tests <<- 3
  tests_failed <<- num_tests
  
  prevalence_tr_correct = readRDS("setup/autograder-setup/hw5_iteration/prevalence_tr_correct.RDS")


  CheckPoint(checkpoint_number = 1,
             test = nrow(prevalence_tr) ==7,
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
             correct_message = "Final answer is a data frame with the correct treatment column",
             error_message = "Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(prevalence_tr$prevalence, prevalence_tr_correct$prevalence),
             correct_message = "Final answer is a data frame with the correct prevalence column",
             error_message = "Did you correctly group by the treatment variable and then take the mean of diar7d in each category? There is an error in the 'prevalence' column.")
  
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
  num_tests <<- 2
  tests_failed <<- num_tests
  
  calculate_inc_rare_correct=readRDS("setup/autograder-setup/hw5_iteration/calculate_inc_rare_correct.RDS")
  
  
  CheckPoint(checkpoint_number = 1,
             test = exists("calculate_inc_rare") & is.function(calculate_inc_rare),
             correct_message = "calculate_inc_rare has been set to a function",
             error_message = "Did you create a function called calculate_inc_rare?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(list(args(calculate_inc_rare)), list(args(calculate_inc_rare_correct))),
             correct_message = "The correct arguments have been passed through calculate_inc_rare",
             error_message = "Did you enter the correct arguments to the function (id and d)?")
  
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
  num_tests <<- 3
  tests_failed <<- num_tests

  prevalence_tr_correct = readRDS("setup/autograder-setup/hw5_iteration/prevalence_tr_correct1.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prevalence_tr) ==7, 
             correct_message = "prevalence_tr is a data frame with the correct number of rows",
             error_message = "Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
             correct_message = "prevalence_tr has the correct treatment column",
             error_message = "Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(prevalence_tr$incidence_density, prevalence_tr_correct$incidence_density),
             correct_message = "prevalence_tr has the correct incidence column",
             error_message = "There is an error in the incidence column. Check the formula in the function you used to calculate incidence.")

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
  num_tests <<- 3
  tests_failed <<- num_tests
  
  prevalence_tr_correct = readRDS("setup/autograder-setup/hw5_iteration/prevalence_tr_correct2.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prevalence_tr) ==7, 
             correct_message = "prevalence_tr is a data frame with the correct number of rows",
             error_message = "Did you correctly group by the treatment variable? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(prevalence_tr$tr, prevalence_tr_correct$tr),
             correct_message = "prevalence_tr has the correct treatment column",
             error_message = "Did you correctly group by the treatment variable? There is an error in the column with the treatment label.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(prevalence_tr$inc_diff, prevalence_tr_correct$inc_diff),
             correct_message = "prevalence_tr has the correct inc_diff column",
             error_message = "There is an error in the inc_diff column. Check the formula in the function you used to calculate incidence.")
  
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
  num_tests <<- 3
  tests_failed <<- num_tests
  
  inc_diff_loop_correct = readRDS("setup/autograder-setup/hw5_iteration/inc_diff_loop_correct.RDS")

  CheckPoint(checkpoint_number = 1,
             test = is.vector(inc_diff_loop),
             correct_message = "inc_diff_loop has been set to a vector",
             error_message = "You did not define inc_diff_loop as a vector. Try again.")
  
  CheckPoint(checkpoint_number = 2,
             test = length(inc_diff_loop)==6,
             correct_message = "The length of inc_diff_loop is correct",
             error_message = "The length of inc_diff_loop is incorrect. Did you accidentally include the control arm?")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(inc_diff_loop, inc_diff_loop_correct),
             correct_message = "inc_diff_loop contains the correct values",
             error_message = "There is an error in the way you calculated the inc_diff_loop vector.")
  
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
  num_tests <<- 3
  tests_failed <<- num_tests
  
  symptoms_correct = readRDS("setup/autograder-setup/hw5_iteration/symptoms_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = is.vector(symptoms),
             correct_message = "symptoms has been set to a vector",
             error_message = "You did not define symptoms as a vector. Try again.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(length(symptoms), length(symptoms_correct)),
             correct_message = "The length of symptoms is correct",
             error_message = "The length of symptoms is incorrect. Did you accidentally include the control arm?")
  
  CheckPoint(checkpoint_number = 3,
             test = round(sum(symptoms - symptoms_correct), 16) == 0,
             correct_message = "symptoms contains the correct values",
             error_message = "There is an error in the way you calculated the symptoms vector.")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(p7, "d3plus7d"),
             correct_message = "Correct!",
             error_message = "Incorrect, try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

