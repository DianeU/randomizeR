# --------------------------------------------
# Generic function for distribution parameters 
# --------------------------------------------

#' Get distribution parameters of a randomization list
#' 
#' Generates a matrix of the distribution parameters of the included patients 
#' in the clinical trial.
#' 
#' @param randSeq object of the class randSeq.
#' @param issue object of the class issue (optional).
#' @param endp object of the class endpoint.
#'
#' @name getDistributionPars
NULL

#' @rdname getDistributionPars
#' 
#' @export
setGeneric("getDistributionPars", function(randSeq, issue, endp) standardGeneric("getDistributionPars"))

