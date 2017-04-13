
library(randomizeR)
N <- 10 # total sample size
par <- rarPar(N) # randomization procedure
R <- genSeq(par) # randomization sequence
# R <- createSeq() # randomization sequence from interactive mode

# save randomization list to csv
saveRand(R, file="myFile.csv")

vignette(package="randomizeR")
