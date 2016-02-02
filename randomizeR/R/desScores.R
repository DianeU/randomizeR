#' @include issue.R
#' @include randSeq.R
#' @include util.R
#' @include endpoint.R
#' @include desFunc.R
#' @include derFunc.R
#' @include derringerLs.R
#' @include derringerRs.R
#' @include derringerTs.R
NULL

###############################################
# --------------------------------------------#
# Class Desirability Scores                   #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Function for validity check
# --------------------------------------------

# Validity check function for objects of the desirability class
# 
# @param object object
#
# @return Returns a \code{TRUE}, if the settings of the object are valid.
validateDesScores <- function(object) {
  errors <- character()
  
  if(length(errors) == 0) TRUE else errors
}


# --------------------------------------------
# Class definition for desScores
# --------------------------------------------

# Randomization paramters generic
setClass("desScores",
         slots = c(D = "data.frame", design = "character", N = "numeric", K = "numeric",
                   groups = "character", desFuncs = "character", weights = "numeric"),
         validity = validateDesScores)


# --------------------------------------------
# Accesssor functions for desScores
# --------------------------------------------

#' Method defining the $ operator for the assessemnt class
#' 
#' @inheritParams overview
setMethod("$", "desScores",
          function(x, name) slot(x, name))


# --------------------------------------------
# Show function for desScores
# --------------------------------------------

setMethod("show", "desScores", function(object) {
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
# Generic functions for using objects of type assess and desFunc
# --------------------------------------------

#' Applying desirability functions on issues of individual randomization sequences
#'
#' Applying desirability function on issues of individual randomization sequences.
#'
#' @inheritParams overview
#' @param assess object of class \code{assessment}.
#' @param ... at least one object of class \code{\link{derFunc}} or a list of objects of 
#' the class \code{\link{derFunc}}.
#' @param weights weights for computing the geometric mean of several desirability 
#'                scores. If missing, the issues are automatically equally weighted. 
#'
#' @details
#' Randomization sequences behave differently with respect to issues
#' like selection bias, chronological bias, or loss in power estimation.
#' The \code{getDesScores} function evaluates the behaviour of randomization 
#' sequences with respect to these issues. The difference to the assess
#' function is that it scales them to [0,1] and makes them easier interpretable.  
#' The first argument should be a result of the \code{\link{assess}} function.
#' The second argument should be any number of \code{\link{derFunc}} objects
#' that represent the desirability functions. The last argument \code{weights} 
#' may be provided if the desirability functions should be weighted differently.  
#'
#' @examples 
#' # Compute the desire-function for the full set of Random Allocation Rule for N=4 patients
#' sequences <- getAllSeq(rarPar(4))
#' issue1 <- corGuess("CS")
#' issue2 <- chronBias("linT", 0.25, "exact")
#' endp <- normEndp(mu = c(0,0), sigma = c(1,1))
#' A <- assess(sequences, issue1, issue2, endp = endp)
#' d1 <- derFunc(0.5, 0.75, 1)
#' d2 <- derFunc(0.05, 0.1, 1)
#' 
#' getDesScores(A, d1, d2)
#' getDesScores(A, d1, d2, weights = c(3/4, 1/4))
#'
#' @return
#' \code{S4} object of class \code{desirability} summarizing the desirability of the 
#' randomization procedure.
#'
#' @seealso Representation of randomization procedures: \code{\link{randPar}}
#' @seealso Generation of randomization sequences: \code{\link{genSeq}}
#' @seealso \code{\link{issues}} for the desirability of randomization sequences
#' 
#' @name getDesScores
NULL

#' @rdname getDesScores
#'
#' @export
setGeneric("getDesScores", function(assess, ..., weights) standardGeneric("getDesScores"))

#' Summary of desirability scores of a randomization procedure
#' 
#'
#' @details
#' For each issue the desirability score of the sequences is summarized to permit a 
#' design-based desirability score of the randomization procedure.
#' This approach uses the sequence-wise values of the desirability and the probabilities
#' in order to give an overall summary.
#'
#' @return 
#' Data frame with a summary of the desirability scores object. 
#' 
#' @examples 
#' # Compute the desirability scores of the full set of PBR(4)
#' seq <- getAllSeq(pbrPar(4))
#' issue1 <- corGuess("CS")
#' issue2 <- corGuess("DS")
#' A <- assess(seq, issue1, issue2)
#' d1 <- derFunc(0.5, c(0.1, 0.8), c(1, 1))
#' d2 <- derFunc(0.1, 0.7, 2)
#' D <- getDesScores(A, d1, d2, weights = c(5/6, 1/6))
#' summary(D)
#' 
#' @name summary
NULL

#' @rdname summary
#'
#' @export
setGeneric("summary")



# --------------------------------------------
# Methods for Desirability Scores
# --------------------------------------------

#' @rdname getDesScores
setMethod("getDesScores", signature(assess = "assessment", weights = "missing"),
          function(assess, ...) {
            desFuns <- list(...)
            lenDesFuns <- length(desFuns)
            if(lenDesFuns==1 && is.list(desFuns[[1]])){
              desFuns <- c(...)
            }
            stopifnot(all(sapply(desFuns, function(x) is(x, "desFunc"))))
            stopifnot(ncol(assess$D[,-c(1,2)]) == lenDesFuns)
            # drop = FALSE is for length(desFuns) = 1
            D <- assess$D[, -c(1,2), drop = FALSE]
            colnames <- colnames(D)
            W <- NULL
            for(i in 1:length(desFuns)){
              d <- sapply(D[,i], function(x) derringer(desFuns[[i]], x))
              W <- cbind(W , d)
              colnames(W)[i] <- paste("d(", colnames[i], ")", sep="")
            }
            # Weights are missing => default setting: equally weighted!
            geometricMean <- apply(W, 1, function(x) exp(mean(log(x))))
            D <- cbind.data.frame(assess$D[, c(1,2), drop = FALSE], W, geometricMean)
      
            new("desScores",
                D = D, design = assess@design,
                N = assess@N, K = assess@K, groups = assess@groups, 
                desFuncs = sapply(desFuns, function(x) getDesFunc(x)), 
                weights = rep(1/lenDesFuns, lenDesFuns))   
          }
)

#' @rdname getDesScores
setMethod("getDesScores", signature(assess = "assessment", weights = "numeric"),
          function(assess, ..., weights) {
            desFuns <- list(...)
            lenDesFuns <- length(desFuns)
            if(lenDesFuns == 1 && is.list(desFuns[[1]])){
              desFuns <- c(...)
            }
            stopifnot(all(sapply(desFuns, function(x) is(x, "desFunc"))))
            stopifnot(ncol(assess$D[,-c(1,2)]) == lenDesFuns)
            stopifnot(length(weights) == lenDesFuns)
            stopifnot(sum(weights) == 1)
            
            D <- assess$D[, -c(1,2), drop = FALSE]
            colnames <- colnames(D)
            E <- NULL
            W <- NULL
            for(i in 1:length(desFuns)){
              d <- sapply(D[,i], function(x) derringer(desFuns[[i]], x))
              E <- cbind(E, d)
              colnames(E)[i] <- paste("d(", colnames[i], ")", sep="")
              # Applying the specified weights on desirability scores
              W <- cbind(W , d^weights[i])
            }
            
            # Compute geometric mean as a product of all rows in W
            geometricMean <- apply(W, 1, prod)
            
            # Bind the relevant columns in one data frame
            D <- cbind.data.frame(assess$D[, c(1,2)], E, geometricMean)
            
            new("desScores",
                D = D, design = assess@design,
                N = assess@N, K = assess@K, groups = assess@groups, 
                desFuncs = sapply(desFuns, function(x) getDesFunc(x)), 
                weights = weights)   
          }
)


#' @rdname summary
setMethod("summary", signature(object = "desScores"), function(object) {
  D <- object@D
  colnames(D)[2] <- "Probability"
  probs <- D$Probability
  if (dim(D)[1] == 1) stop("Selected randomization procedure(s) should have more than one generated sequence.")
  D$Probability <- D$Sequence <- NULL
  stat <- apply(D, 2, function(x) {
    ## weighted mean value
    x1 <- sum(x*probs)
    ## weighted standard deviation
    x2 <- sqrt(sum(probs * (x-x1)^2)/(1 - sum(probs^2)))
    ## weighted quantiles
    sA <- cbind(x, probs)
    sA <- sA[order(x), ]
    wv <- cumsum(sA[ ,2])
    x <- sA[,1]
    x05 <- x[wv >= 0.05][1]
    x25 <- x[wv >= 0.25][1]
    x50 <- x[wv >= 0.5][1]
    x75 <- x[wv >= 0.75][1]
    x95 <- x[wv >= 0.95][1]
    c(x1, x2, max(x), min(x), x05, x25, x50, x75, x95)
  }) 
  rownames(stat) <- c("mean", "sd", "max", "min", "x05", "x25", "x50", "x75", "x95")
  stat
}
)




