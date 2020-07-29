#' @include getExpectation.R
#' @include survEndp.R
#' @include getParameters.R
NULL

###############################################
# --------------------------------------------#
# Class weibEndp                              #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Function for validity check
# --------------------------------------------

# Validity check function for objects of the endpoint class
#
# @inheritParams overview
#
# @return Returns a \code{TRUE}, if the settings of the object are valid.
validateWeibEndp <- function(object) {
  errors <- character() 
  if (!(all(object@shape > 0))) {
    msg <- ("The shape parameter must be positive.")
    errors <- c(errors, msg)
  }
  
  if (!(all(object@scale > 0))) {
    msg <- ("The scale parameter must be positive.")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}


# --------------------------------------------
# Class definition for weibEndp
# --------------------------------------------

# Representation of the Weibull endpoints
setClass("weibEndp", 
         slots = c(shape = "numeric", scale = "numeric"),
         contains = "survEndp", validity = validateWeibEndp)



# --------------------------------------------
# Constructor function for weibEndp
# --------------------------------------------

#' Representation of Weibull distributed endpoints
#' 
#' Represents Weibull distributed endpoints in clinical trials.
#'
#' @inheritParams overview
#'
#' @details
#' The \code{weibEndp} function is a constructor function
#' for an S4 object of the class \code{weibEndp} representing 
#' a Weibull distributed endpoint in a clinical trial.
#' In conjunction with the assess function, Weibull endpoints
#' admit the calculation of the 'exact' type-I-error probability and power
#' using an approximation formula.
#'
#' @family endpoint types
#'
#' @seealso Compute exact or simulated type-I-error: \code{\link{assess}}.
#' 
#' @export
weibEndp <- function(shape , scale , cenRate , accrualTime = 0, 
                      cenTime, weights = c(0,0)) {
  new("weibEndp", shape = shape, scale = scale, cenRate = cenRate, accrualTime = accrualTime, 
      cenTime = cenTime, weights = weights)
}


# --------------------------------------------
# Generic function for weibEndp
# --------------------------------------------

#' @rdname getExpectation
setMethod("getExpectation", signature(randSeq = "randSeq", issue = "missing", 
                                      endp = "weibEndp"), 
          function(randSeq, endp) {
            stopifnot(randSeq@K == length(endp@shape))
            stopifnot(randSeq@K == length(endp@scale))
            validObject(randSeq); validObject(endp)
            
            expectation <- matrix(numeric(0), ncol = ncol(randSeq@M), 
                                  nrow = nrow(randSeq@M))
            for(i in 0:(randSeq@K-1)) {
              expectation[randSeq@M == i] <- endp@scale[i+1]*gamma(1+1/endp@shape[i+1])
            }  
            expectation
          }
)


# --------------------------------------------
# Get shape and scale parameters of weibEndp
# --------------------------------------------

#' @rdname getDistributionPars
setMethod("getDistributionPars", signature(randSeq = "randSeq", issue = "missing", 
                                      endp = "weibEndp"), 
          function(randSeq, endp) {
            stopifnot(randSeq@K == length(endp@shape))
            stopifnot(randSeq@K == length(endp@scale))
            validObject(randSeq); validObject(endp)
            
            shape <- matrix(numeric(0), ncol = ncol(randSeq@M), 
                                  nrow = nrow(randSeq@M))
            scale <- matrix(numeric(0), ncol = ncol(randSeq@M), 
                            nrow = nrow(randSeq@M))
            for(i in 0:(randSeq@K-1)) {
              shape[randSeq@M == i] <- endp@shape[i+1]
              scale[randSeq@M == i] <- endp@scale[i+1]
            }  
            list(shape = shape, scale = scale)
          }
)

