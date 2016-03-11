
###############################################
# --------------------------------------------#
# Class dmVal                                 #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Function for validity check
# --------------------------------------------

# Validity check function for objects of the dmVal class
#
# @inheritParams overview
#
# @return Returns a \code{TRUE}, if the settings of the object are valid.
validatedmVal <- function(object) {
  errors <- character() 
  distri <- object@distri
  dpara  <- object@dpara
  ngroups<- object@ngroups
  
  if(!(distri=="bernoulli" || distri == "urn")) {
    msg <- paste(distri, " is not a valid Type of distribution for the missing Values. 
                 Use bernoulli or urn ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
  }
  
  if(!((ngroups%%1)==0)){
    msg <- paste(ngroups, " must be an integer. ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
    
  }
  if(ngroups<=0){
    msg <- paste(ngroups, " is not a valid number of treatment groups ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
    
  }
  
  
  
  if (length(errors) == 0) TRUE else errors
}


# --------------------------------------------
############## Class definition for dmVal ################
# --------------------------------------------
# Representation of certain types of missing Values
# 
# @description This classes provides the distribution of missing values. 
# @slot distri character "bernoulli" with propability dpara a value is missing
#       "urn" #dpara indices will be drawn from an urn with #patients
#        balls and the values with this indices in the response vector 
#        will be declared missing
# @slot dpara. parameter of the distribution 
# @slot ngroups. Number of treatment groups 

# Class definition for randSeq
# --------------------------------------------

#' @title An S4 Class for the representation of the distribution of missing values
#' 
#' @description This classes provides the distribution of missing values. 
#' @slot distri character "bernoulli" with propability dpara a value is missing
#'       "urn" dpara*(number of patients) indices will be drawn from an urn with number of patients
#'        balls and the values with this indices in the response vector 
#'        will be declared missing
#' @slot dpara. parameter of the distribution 
#' @slot ngroups. Number of treatment groups 
setClass("dmVal", 
         slots = c(distri = "character", dpara = "numeric", ngroups ="numeric"),
         validity = validatedmVal)


# --------------------------------------------
# Constructor function for dmVal
# --------------------------------------------
#' Representation of the distribution of the missing Values
#' 
#' Represents the distribution of missing Values in clinical trials.
#'
#' @inheritParams overview
#'
#' @details
#' The \code{dmVal} function is a constructor function
#' for an S4 object of the class \code{dmVal} representing 
#' the distribution of missing Values in clinical trials.
#' 
#' @param distri character, "bernoulli", "urn"
#' @param dpara vector with parameters of the distribution, 
#'        number of parameters = number of groups (ngroups)
#' @param ngroups numeric, number of groups
#' 
#' @examples
#' dmVal(distri = "bernoulli", dpara = 0.05, ngroups = 3)
#' 
#' @export
#' 
dmVal <- function(distri, dpara, ngroups) {
  new("dmVal", distri = distri, dpara = dpara, ngroups=ngroups)
}


