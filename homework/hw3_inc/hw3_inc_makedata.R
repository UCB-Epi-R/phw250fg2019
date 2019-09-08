#################################################
# R-for-Epi
# Epidemiologic Methods II (PHW250F, PHW250G)
# created by Jade Benjamin-Chung

# Homework 3, Incidence
# Make data
#####################################

km=data.frame(T=c(2,4,10,14,18,20),
              N=c(10,8,6,5,3,2),
              I=rep(1,6))

dm=data.frame(T=seq(2,24,by=2),
              N=c(10,8,7,7,7,5,5,4,3,2,1,1),
              I=c(1,1,0,0,1,0,1,0,1,1,0,0),
              W=c(0,1,0,0,1,1,0,0,0,0,0,0))

save(km, dm, file="hw3_incidence.RData")
