#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Epidemiologic data analysis
# homework #2: analysis preparation
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(12)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1
  num_tests <<- 1
  tests_failed <<- num_tests
  
  pkg = (.packages())
  
  CheckPoint(checkpoint_number = 1,
             test = "dplyr" %in% pkg & "ggplot2" %in% pkg, 
             correct_message = "Required packages have been correctly loaded",
             error_message = "You are missing one of the required packages.")
  
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
  num_tests <<- 6
  tests_failed <<- num_tests  
  
  treatment_cols = c("block", "clusterid", "tr")
  enroll_cols = colnames(read.csv(paste0(here::here(),"/data/washb-data/washb-bangladesh-enrol-public.csv")))
  anthro_cols = colnames(read.csv(paste0(here::here(),"/data/washb-data/washb-bangladesh-anthro-public.csv")))
  
  CheckPoint(checkpoint_number = 1,
             test = exists("treatment"),
             correct_message = "The treatment dataset has been correctly loaded",
             error_message = "Did you correctly load the treatment dataset?")
  
  CheckPoint(checkpoint_number = 2,
             test = exists("enroll"),
             correct_message = "The enroll dataset has been correctly loaded",
             error_message = "Did you correctly load the enroll dataset?")
  
  CheckPoint(checkpoint_number = 3,
             test = exists("anthro"),
             correct_message = "The anthro dataset has been correctly loaded",
             error_message = "Did you correctly load the anthro dataset?")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(colnames(treatment),treatment_cols),
             correct_message = "The column names of the treamtment dataset are correct",
             error_message = "Did you name the correct dataset treatment? The column names are incorrect.")
  
  CheckPoint(checkpoint_number = 5,
             test = setequal(colnames(enroll),enroll_cols),
             correct_message = "The column names of the enroll dataset are correct",
             error_message = "Did you name the correct dataset enroll? The column names are incorrect.")
  
  CheckPoint(checkpoint_number = 6,
             test = setequal(colnames(anthro),anthro_cols),
             correct_message = "The column names of the anthro dataset are correct",
             error_message = "Did you name the correct dataset anthro? The column names are incorrect.")
  
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
  
  enroll_tr_cols = c("block", "tr", "hfiacat")
  
  CheckPoint(checkpoint_number = 1,
             test = exists("enroll_tr"),
             correct_message = "The enroll_tr dataset has been created",
             error_message = "Did you create a merged dataset called enroll_tr?")
  
  CheckPoint(checkpoint_number = 2,
             test = sum(enroll_tr_cols %in% colnames(enroll_tr))==3,
             correct_message = "The column names of the enroll_tr dataset are correct",
             error_message = "Did you name the correct dataset enroll_tr? The column names are incorrect.")

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
  num_tests <<- 2
  tests_failed <<- num_tests 

  all_cols = c("block", "tr", "hfiacat", "laz")
  
  CheckPoint(checkpoint_number = 1,
             test = exists("enroll_tr"),
             correct_message = "The all dataset has been created",
             error_message = "Did you create a merged dataset called all?")
  
  CheckPoint(checkpoint_number = 2,
             test = sum(all_cols %in% colnames(all))==4,
             correct_message = "The column names of the all dataset are correct",
             error_message = "Did you name the correct dataset all? The column names are incorrect.")
  
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
             test = unique(all$tr)[1] == "Control" & length(unique(all$tr)) == 1,
             correct_message = "The tr column only contains the control arm",
             error_message = "Did you correctly subset the data? The tr column contains arms other than the control arm.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(nrow(all), 2252),
             correct_message = "The number of rows in the dataset is correct",
             error_message = "Did you correctly subset the data? The number of rows is incorrect.")
  
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
             test = setequal(names(table(all$svy)), "2"),
             correct_message = "The svy column only contains the 2 year measurement mark",
             error_message = "Did you correctly subset the data? The svy column contains measurements other than 2 year measurement mark.")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(nrow(all), 1122),
             correct_message = "The number of rows in the dataset is correct",
             error_message = "Did you correctly subset the data? The number of rows is incorrect.")
  
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
  num_tests <<- 4
  tests_failed <<- num_tests 
  
  all = readRDS("~/phw250fg2019/setup/autograder-setup/hw8b_epi_analysis/all_correct.RDS")
  
  CheckPoint(checkpoint_number = 1,
             test = ("implatrine" %in% colnames(all)), 
             correct_message = "The all dataset has a columns called implatrine",
             error_message = "Did you add a column called implatrine to the dataset called all?")
  
  CheckPoint(checkpoint_number = 2,
             test = setequal(names(table(all$implatrine_correct)), names(table(all$implatrine))),
             correct_message = "The implatrine column contains the correct values.",
             error_message = "Did you correctly label the values of implatrine?")
  
  CheckPoint(checkpoint_number = 3,
             test = is.factor(all$implatrine), 
             correct_message = "implatrine has been defined as a factor",
             error_message = "Did you define implatrine as a factor?")
  
  CheckPoint(checkpoint_number = 4,
             test = setequal(as.data.frame(table(all$implatrine))$Freq, as.data.frame(table(all$implatrine_correct))$Freq),
             correct_message = "The implatrine column contains the correct values",
             error_message = "Did you correctly assign values to the variable implatrine?")
  
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
             test = !"TRUE" %in% names(table(is.na(all$laz))),
             correct_message = "Missing values have been dropped from the laz column",
             error_message = "Did you drop missing values from the laz column of the all data frame?")
  
  CheckPoint(checkpoint_number = 2,
             test = !"TRUE" %in% names(table(is.na(all$implatrine))),
             correct_message = "Missing values have been dropped from the implatrine column",
             error_message = "Did you drop missing values from the implatrine column of the all data frame?")
  
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
  
  CheckPoint(checkpoint_number = 1,
             test ="ggplot" %in% class(laz_histogram),
             correct_message = "laz_histogram is a ggplot",
             error_message = "laz_histogram is not a ggplot.")
  
  CheckPoint(checkpoint_number = 2,
             test = sum(c("GeomBar", "GeomRect") %in% class(laz_histogram$layers[[1]]$geom))==2, 
             correct_message = "A histogram has in ggplot",
             error_message = "Did you define a histogram in ggplot?")
  
  CheckPoint(checkpoint_number = 3,
             test = laz_histogram$labels$x=="Length-for-age Z-score",
             correct_message = "The correct x-axis label has been added",
             error_message = "Did you add the correct x-axis label?")
  
  CheckPoint(checkpoint_number = 4,
             test = laz_histogram$layers[[1]]$aes_params$colour=="black", 
             correct_message = "The bar outline has been set to black",
             error_message = "Did you set the bar outline to be black?")
  
  CheckPoint(checkpoint_number = 5,
             test = laz_histogram$layers[[1]]$aes_params$fill=="gray", 
             correct_message = "The bar fill has been set to gray",
             error_message = "Did you set the bar fill to be gray?")
    
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
  num_tests <<- 4
  tests_failed <<- num_tests 
  
  CheckPoint(checkpoint_number = 1,
             test ="ggplot" %in% class(laz_lat_boxplot),
             correct_message = "laz_lat_boxplot is a ggplot",
             error_message = "laz_lat_boxplot is not a ggplot.")
  
  CheckPoint(checkpoint_number = 2,
             test = "GeomBoxplot" %in% class(laz_lat_boxplot$layers[[1]]$geom), 
             correct_message = "A histogram has in ggplot",
             error_message = "Did you define a histogram in ggplot?")
  
  CheckPoint(checkpoint_number = 3,
             test = laz_lat_boxplot$labels$x=="Type of latrine",
             correct_message = "The correct x-axis label has been added",
             error_message = "Did you add the correct x-axis label?")
  
  CheckPoint(checkpoint_number = 4,
             test = laz_lat_boxplot$labels$y=="Length-for-age Z-score",
             correct_message = "The correct y-axis label has been added",
             error_message = "Did you add the correct y-axis label?")
  
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
  num_tests <<- 1
  tests_failed <<- num_tests 
  
  CheckPoint(checkpoint_number = 1,
             test = round(diff_mean_laz_implat, 3)==0.504,
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
             test = round(diff_mean_laz_implat_refrig0, 3)==0.385,
             error_message = "Incorrect. Try again.")
  
  CheckPoint(checkpoint_number = 2,
             test = round(round(diff_mean_laz_implat_refrig1, 3)==0.315),
             error_message = "Incorrect. Try again.")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}



