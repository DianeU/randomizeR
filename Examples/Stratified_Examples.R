library(devtools)

# Set the path to the package root
setwd("~\\randomizeR")

# Softload the package via Devtools
current.code <- as.package("randomizeR")
load_all(current.code)

# Create a new pbrPar object using size of the Groups and the Size of the Blocks. 
pbrpar <- pbrPar(N = c(8,12,8), size = 4)

# Size cannot be used if the centers the number of patients in it is divisible by size. 
try(pbrPar(N = c(8,12,8), size = 3))

seqs <- pbrPar(N = c(8,12,8), size = 4)

# Using the bc parameter is more tedious as it is a list of the block vectors per center,
# but still possible. 
pbrPar(bc = list(c(4,4), c(4,4,4), c(4,4)))

# If a randomizations strategy object is created with N given as a vector, 
# genSeq and getAllSeqs will create an object of the new wrapper class 
# randSeqs
seqs1 <- genSeq(pbrpar, r = 100)

# The output formating of randSeqs
print(seqs1)
# The slots are near identical to a randSeqs object
# seqs1@N,seqs1@K,seqs1@ratio, seqs1@groups, seqs1@seed

# Seqs is a list of randSeq objects
# thus addressing it using [[]] returns the respective randSeq Object
seqs1@seqs[[1]]

# All functions for an randSeq Object can then be used on this object
# For example getting the formated Randomization Sequence by using getRandList or 
# using getExpectation on the single strata
getRandList(seqs1@seqs[[1]])[1:20,]


# Getter Method (Adapted from randSeq) to get the combined sequence
# Returns the sequence matrix of the concatenated sequences
getRandList(seqs1)

# Getter Method to get the Name of the Design used in the generation of a Sequence
getDesign(genSeq(hadaPar(N = c(12,18,14)), r = 10))



seqs2 <- genSeq((crPar(N = c(10,6, 8))), r = 100)
seqs3 <- genSeq((rarPar(N = c(10,6, 8))), r = 100)
endp <- normEndp(mu = c(2,2), sigma =  c(3,3))

bias1 <- selBias("CS", eta = 0.5, "exact", alpha = 0.05)
bias2 <- chronBias(type = "linT", theta = 0.5, method = "exact", saltus = 3)
bias3 <- combineBias(bias1, bias2)

# Also getExpectation support randSeqs
# (here only the first strata is shown for brevity) 
getExpectation(seqs1, bias1, endp = endp)[,1:7]


# The assess function now works with the new parameter class union of SeqsObj,
# thus either an object of type randSeq or randSeqs, however testDec will throw
# an error if the stratified T-test cannot be applied   
ass1 <- assess(seqs1, bias1, bias2, bias3, endp = endp)
ass2 <- assess(seqs2, bias1, bias2, bias3, endp = endp)
ass3 <-assess(seqs3, bias1, bias2, bias3, endp = endp)

# For example when testing selbias with an exponential endpoint
try(ass4 <- assess(seqs3, bias1, bias2, bias3, endp = expEndp(lambda = c(1,1), cenRate = 0.2, cenTime = 5)))

# The output formatting of an assessment object
print(ass1)

# The export of an assessment as a csv is also possible:
# ( Saving it in the documents folder under Assessment in this case)
saveAssess(ass1, file = "~\\Assessment.csv")

# The datamframe D of an Assessment Object
# Sequences are concatenated in a single column and split using an ' ' char
print(ass1@D)

# The compare function is also implemented and allows easy comparsion also for stratified sequences
compare(bias3, seqs2, seqs3, endp = endp)

# Generally the stratified T-Test is calculated with optimal weights according to the paper
# However, if one wants to calculate it with weights set to 1, one can pass the addional parameter
# weight to the assess function with the boolean value TRUE

assess(seqs1, bias1, bias2, bias3, weight = T, endp = endp)

compare(bias3, seqs1, seqs2, weight = T, endp = endp)

