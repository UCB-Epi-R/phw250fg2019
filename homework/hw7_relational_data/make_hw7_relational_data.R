#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework: Relational data
#####################################
# ------------------------------------
# make data for main tabs of tutorial

# baseline survey to obtain demographic info
# data collection at baseline and 2 follow-ups
# survey at baseline and 2 follow-ups
# ------------------------------------
# dataset with demographic information 
baseline = data.frame(id=seq(1,10,1),
                      age=c(5,10,3,7,1,8,8,2,6,5),
                      female=c(0,0,1,1,1,0,1,1,0,0))

# # dataset with data collection information - long format
# set.seed(123)
# data_coll = data.frame(id = rep(seq(1,10,1),3),
#                        time = c(rep("Time1",10),
#                                 rep("Time2",10),
#                                 rep("Time3",10)),
#                        stool_collected = rbinom(30,size=1,prob=0.8))

# dataset with lab results - wide format
set.seed(123)
lab_data = data.frame(id=seq(1,10,1),
                      crypto_time1 = rbinom(10,size=1,prob=0.4),
                      # ETEC_time1 = rbinom(10,size=1,prob=0.3),
                      # campy_time1 = rbinom(10,size=1,prob=0.1),
                      crypto_time2 = rbinom(10,size=1,prob=0.4),
                      # ETEC_time2 = rbinom(10,size=1,prob=0.3),
                      # campy_time2 = rbinom(10,size=1,prob=0.1),
                      crypto_time3 = rbinom(10,size=1,prob=0.4))
                      # ETEC_time3 = rbinom(10,size=1,prob=0.3),
                      # campy_time3 = rbinom(10,size=1,prob=0.1))


# impute missings for some individuals
lab_data$crypto_time1[lab_data$id==3]=NA
# lab_data$ETEC_time1[lab_data$id==3]=NA
# lab_data$campy_time1[lab_data$id==3]=NA

lab_data$crypto_time2[lab_data$id==8]=NA
# lab_data$ETEC_time2[lab_data$id==8]=NA
# lab_data$campy_time2[lab_data$id==8]=NA
lab_data$crypto_time2[lab_data$id==5]=NA
# lab_data$ETEC_time2[lab_data$id==5]=NA
# lab_data$campy_time2[lab_data$id==5]=NA

lab_data$crypto_time3[lab_data$id==3]=NA
# lab_data$ETEC_time3[lab_data$id==3]=NA
# lab_data$campy_time3[lab_data$id==3]=NA
lab_data$crypto_time3[lab_data$id==8]=NA
# lab_data$ETEC_time3[lab_data$id==8]=NA
# lab_data$campy_time3[lab_data$id==8]=NA
lab_data$crypto_time3[lab_data$id==5]=NA
# lab_data$ETEC_time3[lab_data$id==5]=NA
# lab_data$campy_time3[lab_data$id==5]=NA

# lab_data = lab_data %>%
#   mutate(has_sample_time1 = ifelse(is.na(crypto_time1),0,1),
#          has_sample_time2 = ifelse(is.na(crypto_time2),0,1),
#          has_sample_time3 = ifelse(is.na(crypto_time3),0,1)) %>%
#   select(id, has_sample_time1, has_sample_time2, has_sample_time3, 
#          everything())

# dataset with follow-up surveys - long format
set.seed(124)
fu_survey = data.frame(id = seq(1,10,1),
                       diarrhea_time1 = rbinom(10,size=1,prob=lab_data$crypto_time1*rnorm(1,mean=0.55,sd=0.05)),
                       diarrhea_time2 = rbinom(10,size=1,prob=lab_data$crypto_time2*rnorm(1,mean=0.5,sd=0.05)),
                       diarrhea_time3 = rbinom(10,size=1,prob=lab_data$crypto_time3*rnorm(1,mean=0.6,sd=0.05)))

save(baseline, lab_data, fu_survey, file = "hw7_relational_data.RData")

