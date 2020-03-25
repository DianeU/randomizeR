


###############################################
# --------------------------------------------#
# Class randSeqs                               #
# --------------------------------------------#
###############################################


# --------------------------------------------
# Function for validity check
# --------------------------------------------

validateRandSeqs <- function(object) {
  errors <- character()
  groups <- object@groups
  K <- object@K
  
  
  if (!(length(groups) == K)) {
    msg <- paste("Length of groups is ", length(groups), ". Should have length ", K,
                 ".", sep = "")
    errors <- c(errors, msg)
  }
  
  if (.hasSlot(object, "seed")) {
    seed <- object@seed
    if (length(seed) != 1) {  
      warning(paste("Length of seed is ", length(seed), ". First argument ", seed[1] ,
                    " is used.", sep = ""))
      
    }
    if (!(round(seed[1]) == seed[1])) {  
      warning(paste("First argument of seed is ", seed[1], ". Used seed was", 
                    integer(seed[1]), ".", sep = ""))
      
    }
  }
  
  if (length(errors) == 0) TRUE else errors
}


# --------------------------------------------
# Class definition for randSeq
# --------------------------------------------

#' @title An S4 Class for the representation of  randomization sequences
#' 
#' @description This wrapper classe provides functionality of storing stratified randomization
#' sequences of the different randomization procedures along with the parameters 
#' representing the design. It is created instead of an \linkS4class{randSeq} object, when creating an
#' object of \linkS4class{randPar} with \code{N} as an vector instead of a scalar value.
#' 
#' 
#' 
#' @slot N vector of the total number of patients included in the strata
#' @slot seqs list containing the underlying randSeq object of the randomization strategy
#' @slot K number of treatment groups
#' @slot groups character string of length K defining the names of the treatment groups
#'
#' @examples 
#' # Create a randSeqs object containing 3 strata with N = 10, 12 and 8 and strategy CR
#' crPar <- crPar(N = c(10, 12, 8))
#' seqs <- genSeq(crPar, r = 10)
#' 
setClass("randSeqs",
         slots = c(seqs = "list", N = "numeric", K = "numeric",
                   ratio = "numeric", groups = "character"),
         validity = validateRandSeqs)

# --------------------------------------------
# Class definition for rRandSeq
# --------------------------------------------

# @title An S4 Class for the representation of  randomization sequences generated at random.
# 
# @description This class provides the functionality of storing stratified randomization
# sequences of the different randomization procedures along with the parameters 
# representing the design.
# 
# @slot seed integer specifying the seed for the generation of randomization sequences
setClass("rRandSeqs",
         slots = c(seed = "numeric"),
         contains = "randSeqs",
         validity = validateRandSeqs)

# --------------------------------------------
# Accesssor functions for randSeq
# --------------------------------------------

#' Method defining the $ operator for the randSeqs class
#' @keywords internal
#' @inheritParams overview
setMethod("$", "randSeqs",
          function(x, name) slot(x, name))



# @rdname getRandomizationList
#  
# @export
setMethod('getRandList',"randSeqs", function(obj) {
  
  seqs <- lapply(obj@seqs, function(x){getRandList(x)})

     do.call(cbind, seqs)
     
})
    


setMethod("getDesign", "randSeqs", function(obj){
    strsplit(getDesign(obj@seqs[[1]]),'(', fixed   = TRUE)[[1]][1]
})

# --------------------------------------------
# Show function for randSeq
# --------------------------------------------

setMethod("show", "randSeqs", function(object) {
  # headline
  sequences <- getRandList(object)
  
  cat("\nObject of class \"", class(object)[1],"\"\n\n", sep = "")
  # crop the method from the class name of the randPar object
  cat("\nContaining ", length(N(object))," strata with a total sample size of ",dim(sequences)[2],"\n \n", sep = "")
  cat("design =", getDesign(object), "\n") 
  # iterate through all slots of the object
  names <- slotNames(object)
  names <- names[!(names == "seqs")] 
  if (K(object) == 2) names <- names[!(names %in% "K")]
  if (all(ratio(object) == rep(1, K(object)))) {
    names <- names[!(names %in% "ratio")]
  }
  for(name in names) {
    cat(name, "=", slot(object, name), "\n")
  }  
  cat('Number of Strata','=', length(N(object)))
  # The matrix M is printed seperately dependent on its size.
  print.matrix <- function(m) {
    write.table(format(m, justify = "left"),
                row.names = T, col.names = F, quote = F)
  }

  
  if (nrow(sequences) %in% 2:3) {
    
    cat("\nThe sequences M: \n\n")
    if (ncol(sequences) < 11) {
      print(sequences)
    } else {
      print(cbind(sequences[ , 1:10], "..."))
    }
  } else if (nrow(sequences) == 1) {
    cat("\nThe sequence M: \n\n")
    if (ncol(sequences) < 11) {
      print(sequences)
    } else {
      sequences <- t(matrix(sequences[1,1:11]))
      sequences[1,11] <- "..."
      print(sequences)
    }
  } else {
    cat("\nThe first 3 of", nrow(sequences), "sequences: \n\n")
    if (ncol(sequences) < 11) {
      print(sequences[1:3, ])
      cat("...")
    } else {
      print(cbind(sequences[1:3, 1:10], "..."))
      cat("...")
    }
  }
  
  cat("\n") 
})







