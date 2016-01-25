###################################################################
# ----------------------------------------------------------------#
# Tests for the getDesScores function                             #
# ----------------------------------------------------------------#
###################################################################

context("Desirability class")

test_that("getDesScore returns valid object", {
  RAR <- getAllSeq(rarPar(4))
  issue1 <- corGuess("CS")
  issue2 <- corGuess("DS")
  A <- assess(RAR, issue1, issue2)
  d1 <- derFunc(TV = 0.1, 0.7, 2)
  d2 <- derFunc(0.5, c(0.3, 0.8))
  DesScore <- getDesScores(A, d1, d2, weights = c(5/6, 1/6))
  expect_is(DesScore, "desScores")
  
})

