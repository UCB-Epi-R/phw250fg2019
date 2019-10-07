#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Relational data
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(16)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  num_tests <<- 5
  tests_failed <<- num_tests
  
  prev_time_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_time_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prev_time) == 1, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly summarize the data? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(prev_time) == 3, 
             correct_message = "Final answer is a data frame with the correct number of columns",
             error_message = "Did you correctly summarize the data? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(prev_time$crypto_prev1, prev_time_correct$crypto_prev1),
             correct_message = "The crypto_prev1 column contains the correct values",
             error_message = "There is an error in the 'crypto_prev1' column.")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(prev_time$crypto_prev2, prev_time_correct$crypto_prev2),
             correct_message = "The crypto_prev2 column contains the correct values",
             error_message = "There is an error in the 'crypto_prev2' column.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(prev_time$crypto_prev3, prev_time_correct$crypto_prev3),
             correct_message = "The crypto_prev3 column contains the correct values",
             error_message = "There is an error in the 'crypto_prev3' column.")
  
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
  num_tests <<- 4
  tests_failed <<- num_tests
  
  lab_data_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_data_long_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(lab_data_long)==30, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly convert to long format? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(prev_time) == 3, 
             correct_message = "Final answer is a data frame with the correct number of columns",
             error_message = "Did you correctly convert to long format? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(lab_data_long$crypto, lab_data_long_correct$crypto),
             correct_message = "The crypto column contains the correct values",
             error_message = "Did you correctly convert to long format? There is an error in the crypto column.")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(lab_data_long$time, lab_data_long_correct$time),
             correct_message = "The time column contains the correct values",
             error_message = "Did you correctly convert to long format? There is an error in the time column.")
  
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
  
  prev_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prev) == 1, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly summarize the data? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(prev) == 1, 
             correct_message = "Final answer is a data frame with the correct number of columns",
             error_message = "Did you correctly summarize the data? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = !is.na(prev$crypto_prev),
             correct_message = "The prevalence column contains the correct values",
             error_message = "Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(prev$crypto_prev, prev_correct$crypto_prev),
             correct_message = "The prevalence column contains the correct values",
             error_message = "Did you correctly summarize the data? The prevalence result is incorrect.")
  
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
             test = p4 =="d", 
             correct_message = "Correct!",
             error_message = "Incorrect. Try again!")
  
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
  
  baseline_lab_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/baseline_lab_long_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(baseline_lab_long)==30, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly convert to long format? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(baseline_lab_long$crypto, baseline_lab_long_correct$crypto),
             correct_message = "The crypto column contains the correct values",
             error_message = "Did you correctly convert to long format? There is an error in the crypto column.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(baseline_lab_long$time, baseline_lab_long_correct$time),
             correct_message = "The time column contains the correct values",
             error_message = "Did you correctly convert to long format? There is an error in the time column.")

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
  
  baseline_lab_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/baseline_lab_long_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = "age_u5" %in% colnames(baseline_lab_long),
             correct_message = "Final answer is a data frame with the column age_u5",
             error_message = "Did you add a new column named age_u5?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(baseline_lab_long$age_u5, baseline_lab_long_correct$age_u5),
             correct_message = "The age_u5 column contains the correct values",
             error_message = "There is an error in the age_u5 column. Try again.")
    
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
  num_tests <<- 5
  tests_failed <<- num_tests
  
  prev_age_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_age_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prev_age) ==2, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly summarize the data? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(prev_age) ==2, 
             correct_message = "Final answer is a data frame with the correct number of columns",
             error_message = "Did you correctly summarize the data? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = "age_u5" %in% colnames(prev_age),
             correct_message = "Final answer is a data frame with the age_u5 column",
             error_message = "Did you calculate prevalence within each level of age_u5?")
  
  CheckPoint(checkpoint_number = 4,
             test = all(!is.na(prev_age$crypto_prev)),
             correct_message = "The crypto_prev column contains no missing values",
             error_message = "Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(prev_age$crypto_prev, prev_age_correct$crypto_prev),
             correct_message = "The crypto_prev column contains the correct values",
             error_message = "Did you correctly summarize the data? The prevalence result is incorrect.")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p8 =="a", 
             correct_message = "Correct!",
             error_message = "Incorrect. Try again!")
  
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
  num_tests <<- 5
  tests_failed <<- num_tests
  
  prev_sex_correct = readRDS("setup/autograder-setup/hw7_relational_data/prev_sex_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(prev_sex)==2, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly convert to long format? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(prev_sex) == 2, 
             correct_message = "Final answer is a data frame with the correct number of columns",
             error_message = "Did you correctly convert to long format? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = "female" %in% colnames(prev_sex),
             correct_message = "Final answer is a data frame with a female column",
             error_message = "Did you calculate prevalence within each level of female?")
  
  CheckPoint(checkpoint_number = 4,
             test = all(!is.na(prev_sex$crypto_prev)),
             correct_message = "The crypto_prev column does not containmissing values",
             error_message = "Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(prev_sex$crypto_prev, prev_sex_correct$crypto_prev),
             correct_message = "The crypto_prev column contains the correct values",
             error_message = "Did you correctly summarize the data? The prevalence result is incorrect.")
  
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
  
  CheckPoint(checkpoint_number = 1,
             test = p10 =="c", 
             correct_message = "Correct!",
             error_message = "Incorrect. Try again!")
  
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
  
  fu_survey_long_correct = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = nrow(fu_survey_long)==30,
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly convert to long format? The number of rows is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(fu_survey_long$diarrhea, fu_survey_long_correct$diarrhea),
             correct_message = "The diarrhea column has the correct values",
             error_message = "Did you correctly convert to long format? There is an error in the diarrhea column.")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests
  
  lab_data_long_correct_time = readRDS("setup/autograder-setup/hw7_relational_data/lab_data_long_correct_time.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(lab_data_long$time, lab_data_long_correct_time$time),
             correct_message = "The time column has the correct values",
             error_message = "There is an error in the time column. Try again.")
  
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
  
  fu_survey_long_correct_time = readRDS("setup/autograder-setup/hw7_relational_data/fu_survey_long_correct_time.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = setequal(fu_survey_long$time, fu_survey_long_correct_time$time),
             correct_message = "The time column has the correct values",
             error_message = "There is an error in the time column. Try again.")
  
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
  num_tests <<- 4
  tests_failed <<- num_tests

  lab_fu_survey_correct = readRDS("setup/autograder-setup/hw7_relational_data/lab_fu_survey_correct.RDS")

  
  CheckPoint(checkpoint_number = 1,
             test = nrow(lab_fu_survey)==30,
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did did you join by the correct colunes? The number of rows is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(lab_fu_survey$diarrhea, lab_fu_survey_correct$diarrhea),
             correct_message = "The diarrhea column has the correct values",
             error_message = "There is an error in the diarrhea column. Try again.")
  
  CheckPoint(checkpoint_number = 3,
             test = setequal(lab_fu_survey$crypto, lab_fu_survey_correct$crypto),
             correct_message = "The crypto column has the correct values",
             error_message = "There is an error in the crypto column. Try again.")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(lab_fu_survey$time, lab_fu_survey_correct$time),
             correct_message = "The time column has the correct values",
             error_message = "There is an error in the time column. Try again.")
  
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
  num_tests <<- 5
  tests_failed <<- num_tests
  
  diarrhea_crypto_prev_correct = readRDS("setup/autograder-setup/hw7_relational_data/diarrhea_crypto_prev_correct.RDS")

  CheckPoint(checkpoint_number = 1,
             test = nrow(diarrhea_crypto_prev) ==2, 
             correct_message = "Final answer is a data frame with the correct number of rows",
             error_message = "Did you correctly summarize the data? The number of rows in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 2,
             test = ncol(diarrhea_crypto_prev) ==2, 
             correct_message = "Final answer is a data frame with the correct number of columns",
             error_message = "Did you correctly summarize the data? The number of columns in your result is incorrect.")
  
  CheckPoint(checkpoint_number = 3,
             test = "diarrhea" %in% colnames(diarrhea_crypto_prev),
             correct_message = "Final answer is a data frame with the diarrhea column",
             error_message = "Did you calculate prevalence within each level of diarrhea?")
  
  CheckPoint(checkpoint_number = 4,
             test = all(!is.na(diarrhea_crypto_prev$crypto_prev)),
             correct_message = "The crypto_prev column contains no missing values",
             error_message = "Did you remember to remove missings mean calculating prevalence? The prevalence result is incorrect.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(diarrhea_crypto_prev$crypto_prev, diarrhea_crypto_prev$crypto_prev),
             correct_message = "The crypto_prev column contains the correct values",
             error_message = "Did you correctly summarize the data? The prevalence result is incorrect.")
  
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
             test = p16 =="b", 
             correct_message = "Correct!",
             error_message = "Incorrect. Try again!")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}


