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
  A1 <- assess(RAR, issue1, issue2)
  d1 <- derFunc(TV = 0.1, 0.7, 2)
  d2 <- derFunc(0.5, c(0.3, 0.8))
  DesScore1 <- getDesScores(A1, d1, d2, weights = c(5/6, 1/6))
  expect_is(DesScore1, "desScores")
  
  seqs <- getAllSeq(pbrPar(bc = c(2, 2, 2)))
  type <- sample(c("CS", "DS"), 1)
  i1 <- corGuess(type)
  A3 <- assess(seqs, i1)
  DesScore3 <- getDesScores(A3, d1)
  expect_is(DesScore3, "desScores")
  
  # Expect error if there are too few arguments and if signature does not fit
  expect_error(getDesScores(A1))
  expect_error(getDesScores(A1, d1))
  expect_error(getDesScores(A1, "blubb", "blibb"))
  expect_error(getDesScores(A1, 42, 34))
  expect_error(getDesScores(A1$D, d1, d2))
})



