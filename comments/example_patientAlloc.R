library(randomizeR)

N <- 10 # total sample size
par <- crPar(N) # complete randomization
R <- genSeq(par) # randomization sequence

getRandPatient <- function(i, R){
  as.vector(getRandList(R))[i]
}

getRandList(R) # complete list
getRandPatient(2, R) # second patient
