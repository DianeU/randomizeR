#' @include getExpectation.R
#' @include survEndp.R
NULL

###############################################
# --------------------------------------------#
# Class expEndp                              #
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
validateExpEndp <- function(object) {
  errors <- character() 
  if (!(all(object@lambda > 0))) {
    msg <- ("The rate parameter lambda must be positive.")
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}


# --------------------------------------------
# Class definition for expEndp
# --------------------------------------------

# Representation of the exponential endpoints
setClass("expEndp", 
         slots = c(lambda = "numeric"), contains = "survEndp", 
         validity = validateExpEndp)



# --------------------------------------------
# Constructor function for expEndp
# --------------------------------------------

#' Representation of exponentially distributed endpoints
#' 
#' Represents exponentially distributed endpoints in clinical trials.
#'
#' @inheritParams overview
#'
#' @details
#' The \code{expEnd} function is a constructor function
#' for an S4 object of the class \code{expEnd} representing 
#' an exponentially distributed endpoint in a clinical trial.
#' In conjunction with the assess function, exponential endpoints
#' admit the calculation of the 'exact' type-I-error probability and power
#' using an approximation formula.
#'
#' @family endopoint types
#'
#' @seealso Compute exact or simulated type-I-error: \code{\link{assess}}.
#' 
#' @export
expEndp <- function(lambda, 
                    cenRate = min(lambda)*10^{-5}, accrualTime = 0, cenTime = qexp(1-10^{-5}, rate = min(lambda)) ) {
  new("expEndp", lambda = lambda, cenRate = cenRate, accrualTime = accrualTime, cenTime = cenTime)
}


# --------------------------------------------
# Generic function for expEndp
# --------------------------------------------

#' @rdname getExpectation
setMethod("getExpectation", signature(randSeq = "randSeq", issue = "missing", 
                                      endp = "expEndp"), 
          function(randSeq, endp) {
            stopifnot(randSeq@K == length(endp@lambda))
            validObject(randSeq); validObject(expEndp)
            expectation <- matrix(numeric(0), ncol = ncol(randSeq@M), 
                      nrow = nrow(randSeq@M))
            for(i in 0:(randSeq@K-1)) {
              expectation[randSeq@M == i] <- 1/endp@lambda[i+1]
            }  
            expectation
          }
)

