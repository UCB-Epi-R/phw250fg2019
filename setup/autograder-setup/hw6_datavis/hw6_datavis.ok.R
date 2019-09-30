#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung, Nolan Pokpongkiat, Anna Nguyen

# Autograder: Data visualization 
###############################################
source("setup/autograder-setup/autograder_setup.R")

AutograderInit = function() {
  AutograderSetUp(7)
}

# --------------------------------------------
CheckProblem1 = function() {
  problemNumber = 1   
  num_tests <<- 9
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1, 
             test = "ggplot" %in% class(p1),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  CheckPoint(checkpoint_number = 2,
             test = length(grep("meanfcolif3d", p1$layers[[1]]$mapping))==1,
             correct_message = "The outcome variable has been correctly defined",
             error_message = "Did you define the correct outcome variable?")
  
  CheckPoint(checkpoint_number = 3,
             test = "GeomBar" %in% class(p1$layers[[1]]$geom), 
             correct_message = "A histogram has been defined in ggplot",
             error_message = "Did you define a histogram in ggplot?")
  
  CheckPoint(checkpoint_number = 4,
             test = p1$labels$x=="Mean log 10 fecal coliform concentration in the past 3 days",
             correct_message = "The x-axis label has been correctly added",
             error_message = "Did you add the correct x-axis label?")
  
  CheckPoint(checkpoint_number = 5,
             test = p1$labels$y=="Number of observations", 
             correct_message = "The y-axis label has been correctly added",
             error_message = "Did you add the correct y-axis label?")
  
  CheckPoint(checkpoint_number = 6,
             test = p1$layers[[1]]$stat_params$bins==50, 
             correct_message = "The x-axis label has been correctly added",
             error_message = "Did you set the number of bins to 50?")
  
  CheckPoint(checkpoint_number = 7,
             test = length(p1$layers[[1]]$aes_params)!=0, 
             correct_message = "The bar fill and outline colors have been set",
             error_message = "Did you set the bar fill and outline colors?")
  
  CheckPoint(checkpoint_number = 8,
             test = length(p1$layers[[1]]$aes_params)!=0 & p1$layers[[1]]$aes_params$fill=="gray", 
             correct_message = "The color of the bar fill is gray",
             error_message = "Did you set the color of the bar fill to gray?")
  
  CheckPoint(checkpoint_number = 9,
             test = length(p1$layers[[1]]$aes_params)!=0 & p1$layers[[1]]$aes_params$colour=="black", 
             correct_message = "The color of the bar outline is black",
             error_message = "Did you set the color of the bar outline to black?")
  
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
             test = p2=="a", 
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


CheckProblem3 = function() {
  problemNumber = 3   
  num_tests <<- 7
  tests_failed <<- num_tests
  
  p3.correct = readRDS("setup/autograder-setup/hw6_datavis/p3.correct.RDS")
  
  student_y_limits=as.list(ggplot_build(p3)$plot$scales$scales[[1]])$limits
  correct_y_limits=as.list(ggplot_build(p3.correct)$plot$scales$scales[[1]])$limits
  
  student_x_limits=as.list(ggplot_build(p3)$plot$scales$scales[[2]])$limits
  correct_x_limits=as.list(ggplot_build(p3.correct)$plot$scales$scales[[2]])$limits

  CheckPoint(checkpoint_number = 1, 
             test = "ggplot" %in% class(p3),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  CheckPoint(checkpoint_number = 2,
             test = "GeomPoint" %in% class(p3$layers[[1]]$geom), 
             correct_message = "A scatterplot has been defined in ggplot",
             error_message = "Did you define a scatterplot in ggplot?")
  
  CheckPoint(checkpoint_number = 3,
             test = p3$labels$x=="Precipitation in the past 3 days (inches)",
             correct_message = "The x-axis label has been correctly added",
             error_message = "Did you add the correct x-axis label?")
  
  CheckPoint(checkpoint_number = 4,
             test = p3$labels$y=="Mean log 10 fecal coliform concentration in the past 3 days",
             correct_message = "The y-axis label has been correctly added",
             error_message = "Did you add the correct y-axis label?")
  
  CheckPoint(checkpoint_number = 5,
             test = p3$layers[[1]]$aes_params$alpha==0.3, 
             correct_message = "Alpha has been set to 0.3",
             error_message = "Did you set alpha to 0.3?")
  
  CheckPoint(checkpoint_number = 6,
             test = setequal(student_y_limits, correct_y_limits),
             correct_message = "y axis limits have been correctly set",
             error_message = "Did you set the y axis limits to 0 and 4.5?")
  
  CheckPoint(checkpoint_number = 7,
             test = setequal(student_x_limits, correct_x_limits),
             correct_message = "x axis limits have been correctly set",
             error_message = "Did you set the y axis limits to 0 and 2.7?")
  
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
             test = p4=="c", 
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
  num_tests <<- 1
  tests_failed <<- num_tests
  
  CheckPoint(checkpoint_number = 1,
             test = p5=="b", 
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
CheckProblem6 = function() {
  problemNumber = 6
  num_tests <<- 4
  tests_failed <<- num_tests
  
  # NOT DONE
  CheckPoint(checkpoint_number = 1, 
             test = "ggplot" %in% class(p6),
             correct_message = "A ggplot has been defined",
             error_message = "You did not define a ggplot.")
  
  CheckPoint(checkpoint_number = 2,
             test = "GeomBoxplot" %in% class(p6$layers[[1]]$geom), 
             correct_message = "A boxplot has been defined in ggplot",
             error_message = "Did you define a boxplot in ggplot?")
  
  CheckPoint(checkpoint_number = 3,
             test = p6$labels$x=="Beach",
             correct_message = "The x-axis label has been correctly added",
             error_message = "Did you add the correct x-axis label?")
  
  CheckPoint(checkpoint_number = 4,
             test = p6$labels$y=="Mean log 10 fecal coliform concentration in the past 3 days",
             correct_message = "The y-axis label has been correctly added",
             error_message = "Did you add the correct y-axis label?")
  
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
  
  # NOT DONE
  CheckPoint(checkpoint_number = 1,
             test = p7=="a", 
             correct_message = "Correct!",
             error_message = "Incorrect. Try again!")
  
  if (tests_failed == 0){
    scores[problemNumber] <<- 1
  } else {
    scores[problemNumber] <<- 0
  }
  
  ReturnScore(problemNumber, num_tests, tests_failed)
}

