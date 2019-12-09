context("getParameters")

# getExpectation
test_that("tests of equality of getParameters", {
  
  # first scenario, Weibull Endpoint
    randSeq <- genSeq(rpbrPar(rb = 2, N = 12))
    randSeq@M <- matrix(rep(c(0, 1), 6), nrow = 1)
    endp <- weibEndp(shape = c(0.25,1), scale = c(1,1), eta = 0, delta = 0, cenTime = 10, cenRate = 0.01)
    dpars <- getDistributionPars(randSeq = randSeq, endp =  endp)
    expect_equal(dpars$shape, matrix(rep(c(0.25, 1), 6), nrow = 1))
    expect_equal(dpars$scale, matrix(rep(c(1, 1), 6), nrow = 1))
    
  # second scenario, Weibull endpoint with SelBias Type CS
    biasSB <- selBias("CS", log(2), "exact")
    dpars <- getDistributionPars(randSeq = randSeq, biasSB, endp =  endp)
    expect_equal(dpars$shape, matrix(rep(c(0.25, 1), 6), nrow = 1))
    expect_equal(dpars$scale, matrix(rep(c(1, 2), 6), nrow = 1))
    
  # third scenario, Weibull endpoint with SelBias Type CS2  
    biasSB <- selBias("CS2", 0, "exact")
    endp <- weibEndp(shape = c(1,1), scale = c(2,2), eta = log(2), delta = 0, cenTime = 10, cenRate = 0.01)
    dpars <- getDistributionPars(randSeq = randSeq, biasSB, endp =  endp) 
    expect_equal(dpars$shape, matrix(rep(c(1, 1), 6), nrow = 1))
    expect_equal(dpars$scale, matrix(rep(c(2, 4), 6), nrow = 1))

  
  
  
  }


) 