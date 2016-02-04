#' @include derFunc.R
NULL

###############################################
# --------------------------------------------#
# Class desirability                          #
# --------------------------------------------#
###############################################

#' Desirability functions within the scope of clinical trials
#' 
#' Illustrates the interplay between functions related to desirability indices.
#' 
#' @details 
#' Currently, \code{randomizeR} encompasses the class of desirability functions introduced
#' by Derringer and Suich (1980) and corresponding functions to evaluate and compare 
#' randomization sequences which have been assessed on the basis of desirability indices 
#' of specific issues:
#' \itemize{
#' \item \strong{\link{derFunc}}
#'    represents the class of desirability functions according to Derringer-Suich (1980).
#' \item \strong{\link{getDesScores}}
#'    can be applied to an object of class \code{assessment} together with prespecified
#'    desirability functions to compare the behaviour of randomization sequences (on a 
#'    common scale [0,1]).
#' \item \strong{\link{evaluate}}
#'    performs a comparison of sequences from different randomization sequences on the 
#'    basis of object of the class \code{desScores}.
#' \item \strong{\link{probUnDes}}
#'    computes the proability of undesired randomization sequences with respect to 
#'    certain issues and desirability functions.
#' }
#' 
#' @examples 
#' # Suppose we would like to perform a comparison of sequences from different 
#' # randomization procedures with the help of desirability functions:
#' 
#' issue1 <- corGuess("CS")
#' issue2 <- corGuess("DS")
#' RAR <- getAllSeq(rarPar(4))
#' BSD <- getAllSeq(bsdPar(4, mti = 2))
#' A1 <- assess(RAR, issue1, issue2)
#' A2 <- assess(BSD, issue1, issue2)
#' 
#' d1 <- derFunc(TV = 0.1, 0.7, 2)
#' d2 <- derFunc(0.5, c(0.3, 0.8), c(1, 1))
#' 
#' # By applying the \code{getDesScores} function to the assessment output together 
#' # with the specified desirability functions the behaviour of randomization sequences 
#' # is evaluated and scaled to [0,1]:
#' DesScore <- getDesScores(A1, d1, d2)
#' DesScore2 <- getDesScores(A2, d1, d2)
#' 
#' # Summarize the results of getDesScore with respect to the statistic "mean":
#' evaluate(DesScore, DesScore2)
#' 
#' # Which randomzation procedure produces more undesired randomization sequences 
#' # with respect to certain issues and desirability functions?
#' probUnDes(DesScore)
#' probUnDes(DesScore2)
#' 
#' @name desirability
NULL