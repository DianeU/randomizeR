#' @include issue.R
#' @include randSeq.R
#' @include util.R
#' @include endpoint.R
#' @include assess.R
#' @include desScores.R
NULL

###############################################
# --------------------------------------------#
# Class Evaluation                            #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Function for validity check
# --------------------------------------------

# Validity check function for objects of the desirability class
# 
# @inheritParams overview 
#
# @return Returns a \code{TRUE}, if the settings of the object are valid.
validateEvaluation <- function(object) {
  errors <- character()
  
  if(length(errors) == 0) TRUE else errors
}


# --------------------------------------------
# Class definition for evaluation
# --------------------------------------------

# Evaluation paramters generic
setClass("evaluation",
         slots = c(D = "data.frame", desFuncs = "character", weights = "numeric", 
                   statistic = "character"),
         validity = validateEvaluation)


# --------------------------------------------
# Accesssor functions for evaluation
# --------------------------------------------

#' Method defining the $ operator for the evaluation class
#' 
#' @inheritParams overview
setMethod("$", "evaluation",
          function(x, name) slot(x, name))


# --------------------------------------------
# Show function for desirability
# --------------------------------------------

setMethod("show", "evaluation", function(object) {
  # headline
  cat("\nObject of class \"", class(object)[1],"\"\n\n", sep="")
  # iterate through all slots of the object
  names <- slotNames(object)
  names <- names[!(names == "D")] # without D
  for(name in names) {
    if(is.numeric(slot(object, name))){
      cat(name, "=", round(slot(object, name), digits = 3), "\n")
    } else{
      cat(name, "=", slot(object, name), "\n")
    }
  }
  cat("\n") 
  # The data.frame D is printed seperately dependent on its size.
  if (dim(object@D)[1] <= 3) {
    if (nchar(as.character(object@D[1, 1])) >= 10)
      object@D[ ,1] <- paste(substr(object@D[, 1], 1, 9), "...")
    object@D[ ,-1] <- round(object@D[ ,-1], digits = 3)
    print(object@D) 
  } else {
    cat("\nThe first 3 rows of", dim(object@D)[1], "rows of D: \n\n")
    object <- object@D[1:3, ]
    if (nchar(as.character(object[1, 1])) >= 10)
      object[ ,1] <- paste(substr(object[, 1], 1, 9), "...")
    object[ ,-1] <- round(object[ ,-1],digits = 3)
    print(object) 
    cat("...")
  }
  cat("\n") 
}  
)


# --------------------------------------------
# Generic functions for using objects of type desScores
# --------------------------------------------

#' Evaluation of several randomization procedures with respect to certain desirability
#' functions applied to specified issues.
#'
#'
#' @inheritParams overview
#' 
#' @param ... at least one object of the class \code{desScores} or a list of objects of 
#' the class \code{desScores}.
#' @param statistic character string that specifies on the basis of which statistic the 
#' \code{evaluate} function should be applied. The statistic can be chosen from "mean", 
#' "median", "min" or "max". 
#' 
#' @details
#'
#' The \code{evaluate} function allows the user to compare and evaluate different 
#' randomization procedures. It expects a number of objects that result when applying the 
#' \code{getDesScores} function to an assess object and specified desirability functions. 
#' The \code{evaluate} function summarizes the desirability scores of each randomization 
#' procedure obny the basis of a prespecified statistic and encorporates them into a data frame. If the function is applied to only
#' one object it corresponds simply to \code{summary(getDesScores(...))}.
#'
#' @examples 
#' # Compare Random Allocation Rule to Big Stick Design with respect to different issues
#' # and their corresponding desirability functions
#' issue1 <- corGuess("CS")
#' issue2 <- corGuess("DS")
#' RAR <- getAllSeq(rarPar(4))
#' BSD <- getAllSeq(bsdPar(4, mti = 2))
#' A1 <- assess(RAR, issue1, issue2)
#' A2 <- assess(BSD, issue1, issue2)
#' 
#' d1 <- derFunc(TV = 0.1, 0.7, 2)
#' d2 <- derFunc(0.5, c(0.3, 0.8), c(1, 1))
#' DesScore <- getDesScores(A1, d1, d2, weights = c(5/6, 1/6))
#' DesScore2 <- getDesScores(A2, d1, d2)
#' 
#' evaluate(DesScore, DesScore2)
#' evaluate(DesScore, DesScore2, statistic = "max")
#' 
#'
#' @return
#' \code{S4} object of class \code{evaluation} Comparison of randomization procedures 
#' with respect to desirability functions applied to specified issues, summarized by a
#' prespecified statistic. 
#'
#' @seealso Representation of randomization procedures: \code{\link{randPar}}
#' @seealso Generation of randomization sequences: \code{\link{genSeq}}
#' @seealso \code{\link{issues}} for the desirability of randomization sequences
#' 
#' @name evaluate
NULL

#' @rdname evaluate
#'
#' @family desirability topics
#'
#' @export
setGeneric("evaluate", function(..., statistic) standardGeneric("evaluate"))

# --------------------------------------------
# Methods for Evaluation
# --------------------------------------------

#' @rdname evaluate
setMethod("evaluate", signature(statistic = "missing"),
          function(...) {
            dScores <- list(...)
            if(length(dScores) == 1 && is.list(dScores[[1]])){
              dScores <- c(...)
            }
            
            stopifnot(all(sapply(dScores, function(x) is(x, "desScores"))))
            n <- ncol(dScores[[1]]$D)
            if(!all(unlist(lapply(dScores, function(x) all(n == ncol(x$D)))))){
              stop("Error: Objects have different number of columns!")
            }
            colnames <- colnames(dScores[[1]]$D)
            if(!all(unlist(lapply(dScores, function(x) all(colnames == colnames(x$D)))))){
              stop("Error: Colnames do not coincide!")
            }
            desFuncs <- dScores[[1]]$desFuncs
            if(!all(unlist(lapply(dScores, function(x) all(desFuncs == x$desFuncs))))){
              warning("The desirability functions do not coincide. The show function only 
                      displays the desirability functions of the first desScores object.")
            }
            weights <- dScores[[1]]$weights
            if(!all(unlist(lapply(dScores, function(x) all(weights == x$weights))))){
              warning("The weights do not coincide. The show function only 
                      displays the weights of the first desScores object.")
            }
            
            # Creates the first column which contains the designs of the different 
            # randomization procedures
            D <- data.frame("RandProc" = sapply(dScores, function(x) x@design))
            # Uses summary(.) to generate the means of the desirability functions and
            # puts it in one matrix
            M <- lapply(dScores, function(x) summary(x)[1,])
            M <- do.call(rbind, M)
            D <- cbind(D, M)           
            
            new("evaluation", 
                D = D, desFuncs = dScores[[1]]$desFuncs, weights = dScores[[1]]$weights, 
                statistic = "mean")   
          }
)

#' @rdname evaluate
setMethod("evaluate", signature(statistic = "character"),
          function(..., statistic) {
            dScores <- list(...)
            if(length(dScores) == 1 && is.list(dScores[[1]])){
              dScores <- c(...)
            }
            
            stats <- c("mean", "median", "min", "max")
            stopifnot(statistic %in% stats)
            stopifnot(all(sapply(dScores, function(x) is(x, "desScores"))))
            n <- ncol(dScores[[1]]$D)
            if(!all(unlist(lapply(dScores, function(x) all(n == ncol(x$D)))))){
              stop("Error: Objects have different number of columns!")
            }
            colnames <- colnames(dScores[[1]]$D)
            if(!all(unlist(lapply(dScores, function(x) all(colnames == colnames(x$D)))))){
              stop("Error: Colnames do not coincide!")
            }
            desFuncs <- dScores[[1]]$desFuncs
            if(!all(unlist(lapply(dScores, function(x) all(desFuncs == x$desFuncs))))){
              warning("The desirability functions do not coincide. The show function only 
                      displays the desirability functions of the first desScores object.")
            }
            weights <- dScores[[1]]$weights
            if(!all(unlist(lapply(dScores, function(x) all(weights == x$weights))))){
              warning("The weights do not coincide. The show function only 
                      displays the weights of the first desScores object.")
            }
            
            
            # Creates the first column which contains the designs of the different 
            # randomization procedures
            D <- data.frame("RandProc" = sapply(dScores, function(x) x@design))
            # colStats contains the row number of the corresponding statistics
            colStats <- c(1, 7, 4, 3)
            # ind determines which statistic was chosen
            ind <- which(statistic == stats)
            # Uses summary(.) to generate the statistic of the desirability functions and
            # puts it in one matrix
            M <- lapply(dScores, function(x) summary(x)[colStats[ind],])
            M <- do.call(rbind, M)
            D <- cbind(D, M)           
            
            new("evaluation", 
                D = D, desFuncs = dScores[[1]]$desFuncs, weights = dScores[[1]]$weights, 
                statistic = statistic)   
            }
)