
###############################################
# --------------------------------------------#
# Class mVal                                  #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Function for validity check
# --------------------------------------------

# Validity check function for objects of the mVal class
#
# @inheritParams overview
#
# @return Returns a \code{TRUE}, if the settings of the object are valid.
validatemVal <- function(object) {
  errors <- character()
  dist <- object@dist
  depType <- object@depType
  rSetType <- object@rSetType
  
  
  if(!(rSetType == "CRS" || rSetType == "RRS")) {
    msg <- paste("rSetType =", rSetType, " should be CRS or RRS",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
  }
  
  if((depType=="at random" && !(length(dist@dpara)==1))) {
    msg <- paste("For", depType, "the number of the parameters is wrong ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
  }
  
  if((depType=="treatment" && !(length(dist@dpara)==dist@ngroups))) {
    msg <- paste("For", depType, "the number of the parameters is wrong ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
  }
  
  if(!(depType=="at random" || depType == "treatment")) {
    msg <- paste(depType, "is not a valid Type of missing Values. Use at random or treatment ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
  }
  
  if(!(class(dist)=="dmVal" )) {
    msg <- paste(dist, "is not a valid Type of the distribution of the missing values ",
                 ".", sep = "", collapse = ",")
    errors <- c(errors, msg)
  }
  
  if(length(errors) == 0) TRUE else errors
}



################ Class definition for mVal #################
# --------------------------------------------

#' @title An S4 Class for the representation of missing values
#' 
#' @description This set of classes provides functionality of storing parameters 
#' which represent the design of the missing values .
#' 
#' @slot dist element of the class dmVal, represtents the distirbution of the missing values
#' @slot depType character, "at random" missing values are drawn at random 
#'                          "treatment" probability of missing values varies with 
#'                          different treatment groups
#' @slot rSetType character, "CRS" complete reference Set, missing values treated as zero
#'                          "RRS" reduce reference Set, missing values treated as NA 
setClass("mVal", slots = c( dist ="dmVal", depType="character", rSetType="character" ), 
         validity = validatemVal
)

##########################################################


# --------------------------------------------
# Constructor function for mVal
# --------------------------------------------

#' Representation of missing Values
#' 
#' Represents the structure of missing Values in clinical trials.
#'
#' @inheritParams overview
#'
#' @details
#' The \code{mVal} function is a constructor function
#' for an S4 object of the class \code{mVal} representing 
#' the structure of missing Values in a clinical trial.
#'
#'
#  @rdname generateObservationmVal
#'
#' @param dist dmVal object, represents distribution of missing values
#' @param depType character, "at random", "treatment"
#' @param rSetType character, "CRS" or "RRS"
#' 
#' @examples
#' dm <- dmVal(distri = "bernoulli", dpara = 0.05, ngroups = 3)
#' mVal(dm, depType = "at random", rSetType = "CRS")
#' 
#' @export
mVal <- function(dist, depType, rSetType) {
  new("mVal", dist = dist, depType=depType, rSetType = rSetType)
}


##################################################################

######################Method - MVal######################


# #' Function returning the dist slot of an S4 object
# #'
# #' 
# #' @export
# #' 
# dist <- function(object) {
#   if (.hasSlot(object, "dist")) {
#     object@dist
#   } else {
#     stop("object has no slot named dist.")
#   }
# }
# 
# #' Function returning the dist slot of an S4 object
# #'
# #' 
# #' @export
# #' 
# depType <- function(object) {
#   if (.hasSlot(object, "depType")) {
#     object@depType
#   } else {
#     stop("object has no slot named depType.")
#   }
# }
# 
# 
# #' Function returning the rSetType slot of an S4 object
# #'
# #' 
# #' @export
# #' 
# rSetType <- function(object) {
#   if (.hasSlot(object, "rSetType")) {
#     object@rSetType
#   } else {
#     stop("object has no slot named rSetType.")
#   }
# }
# 




