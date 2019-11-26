#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Epidemiologic data analysis
# homework #3: analysis 
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(17)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p1 == "a",
             error_message = "Incorrect. Try again.")
  
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
             test = p2 == "c",
             error_message = "Incorrect. Try again.")
  
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
  num_tests <<- 4
  tests_failed <<- num_tests

  CheckPoint(checkpoint_number = 1,
             test = setequal(class(ttest_laz), "htest"),
             correct_message = "ttest_laz is the result of a t-test?",
             error_message = "Is ttest_laz the result of a t-test?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(ttest_laz[1]$statistic, 3), -6.328 ),
             correct_message = "t-test has been correctly set up",
             error_message = "Did you correctly set up the t-test?")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(round(ttest_laz_stat,3), -6.328 ),
             correct_message = "The t-statistic has been saved in ttest_laz_stat",
             error_message = "Did you save the t statistic in ttest_laz_stat?")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(round(ttest_laz_pvalue,3), 0 ),
             correct_message = "The p-values has been saved in ttest_laz_stat",
             error_message = "Did you save the p-value in ttest_laz_stat?")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p4 == "a",
             error_message = "Incorrect. Try again.")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p5 == "g",
             error_message = "Incorrect. Try again.")
  
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
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(class(laz_unadj_model)[1], "glm"),
             correct_message = "laz_unadj_model is the output of a glm model",
             error_message = "Is laz_unadj_model the output of a glm model?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(laz_unadj_model$coefficients[1], 3), -1.875) & setequal(round(laz_unadj_model$coefficients[2], 3), 0.501),
             correct_message = "The model has the correct exposure, outcome, and data",
             error_message = "Does your model have the correct exposure, outcome, and data?")
  
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
  
  names_imp = c("Does not have improved latrine", "Has improved latrine")
  values_imp = c(857, 204)
  
  CheckPoint(checkpoint_number = 1,
             test = round(laz_unadj_moa, 3) != -1.875,
             correct_message = "The correct coefficient has been saved",
             error_message = "Did you save the correct coefficient?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(laz_unadj_moa, 3), 0.501),
             error_message = "Incorrect. Try again.")
  
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
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(round(laz_unadj_moa_ci[1], 3), 0.348),
             error_message = "Incorrect. Try again.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(laz_unadj_moa_ci[2], 3), 0.655),
             error_message = "Incorrect. Try again.")
  
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
             test = setequal(class(laz_adj_model)[1], "glm"),
             correct_message = "laz_adj_model is the output of a glm model",
             error_message = "Is laz_adj_model the output of a glm model?")
  
  CheckPoint(checkpoint_number = 2,
             test =setequal(round(laz_adj_model$coefficients[1], 3), -12.524) & setequal(round(laz_adj_model$coefficients[2], 3), 0.349),
             correct_message = "The model has the correct exposure, outcome, and data",
             error_message = "Does your model have the correct exposure, outcome, and data?")
  
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
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = round(laz_adj_model$coefficients[1], 3) != -1.252,
             correct_message = "The correct coefficient has been saved",
             error_message = "Did you save the correct coefficient?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(laz_adj_moa, 3), 0.349),
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem11 = function() {
  problemNumber = 11  
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(round(laz_adj_moa_ci[1], 3), 0.198),
             error_message = "Incorrect. Try again.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(laz_adj_moa_ci[2], 3), 0.500),
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem12 = function() {
  problemNumber = 12
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(class(laz_unadj_int_model)[1], "glm"),
             correct_message = "laz_unadj_int_model is the output of a glm model",
             error_message = "Is laz_unadj_int_model the output of a glm model?")
  
  CheckPoint(checkpoint_number = 2,
             test =setequal(round(laz_unadj_int_model$coefficients[1], 3), -1.898) & setequal(round(laz_unadj_int_model$coefficients[2], 3), 0.382),
             correct_message = "The model has the correct exposure, outcome, and data",
             error_message = "Does your model have the correct exposure, outcome, and data?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem13 = function() {
  problemNumber = 13
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(round(laz_interaction, 3), -0.035),
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem14 = function() {
  problemNumber = 14
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p14 == "f",
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem15 = function() {
  problemNumber = 15
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(class(stunt_unadj_model)[1], "glm"),
             correct_message = "stunt_unadj_model is the output of a glm model",
             error_message = "Is stunt_unadj_model the output of a glm model?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(stunt_unadj_model$coefficients[1], 3), -0.802) & setequal(round(stunt_unadj_model$coefficients[2], 3), -0.620),
             correct_message = "The model has the correct exposure, outcome, and data",
             error_message = "Does your model have the correct exposure, outcome, and data?")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem16 = function() {
  problemNumber = 16
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(round(stunt_unadj_moa, 3), 0.538),
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

# --------------------------------------------
CheckProblem17 = function() {
  problemNumber = 17
  num_tests <<- 2
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(round(stunt_unadj_moa_ci[1], 3), 0.393),
             error_message = "Incorrect. Try again.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(round(stunt_unadj_moa_ci[2], 3), 0.718),
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

