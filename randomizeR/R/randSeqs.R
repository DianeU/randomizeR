###############################################
# --------------------------------------------#
# Class randSeq                               #
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
#' @description This set of classes provides functionality of storing randomization
#' sequences of different randomization procedures along with the parameters 
#' representing the design.
#' 
#' @slot N total number of patients included in the trial
#' @slot M matrix containing randomization sequences of length \code{N} in its
#' rows.
#' @slot K number of treatment groups
#' @slot groups character string of length K defining the names of the treatment groups
setClass("randSeqs",
         slots = c(seqs = "list", N = "numeric", K = "numeric",
                   ratio = "numeric", groups = "character"),
         validity = validateRandSeqs)

# --------------------------------------------
# Class definition for rRandSeq
# --------------------------------------------

# @title An S4 Class for the representation of  randomization sequences generated at random.
# 
# @description This set of classes provides functionality of storing random randomization
# sequences of different randomization procedures along with the parameters 
# representing the design 
# 
# @slot seed integer specifying the seed for the generation of randomization sequences
setClass("rRandSeqs",
         slots = c(seed = "numeric"),
         contains = "randSeqs",
         validity = validateRandSeqs)

# --------------------------------------------
# Accesssor functions for randSeq
# --------------------------------------------

#' Method defining the $ operator for the randSeq class
#' @keywords internal
#' @inheritParams overview
setMethod("$", "randSeqs",
          function(x, name) slot(x, name))


#' Function returning the allocation seed slot of an object
#'
#' Returns the seed that was either generated at random or user specified.
#' The seed can be specified for any random operation e.g. genSeq.
#'
#' @inheritParams overview
seed <- function(obj) {
  if (.hasSlot(obj, "seed")) obj@seed
  else stop("Object has no slot named seed.") 
}

#' Accessor function for the randomization list 
#'
#' Get the randomization list coded in its groups.
#'
#' @param combined  logical If the Sequences should be combined in a single matrix or not 
#' @inheritParams overview 
#'
#' @name getCombinedSequence
#'
#' @export
getCombinedSequences <- function(obj, combined = TRUE) {
  
  seqs <- lapply(obj@seqs, function(x){getRandList(x)})
  
  if(combined){
     do.call(cbind, seqs)
  }else{
     seqs
  }
     
}
    


setMethod("getDesign", "randSeqs", function(obj){
    strsplit(getDesign(obj@seqs[[1]]),'(', fixed   = TRUE)[[1]][1]
})

# --------------------------------------------
# Show function for randSeq
# --------------------------------------------

setMethod("show", "randSeqs", function(object) {
  # headline
  sequences <- getCombinedSequences(object)
  
  cat("\nObject of class \"", class(object)[1],"\"\n\n", sep = "")
  # crop the method from the class name of the randPar object
  cat("\nContaining ", length(N(object))," centers with ",dim(sequences)[2]," total patiens \n \n", sep = "")
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
  # The matrix M is printed seperately dependent on its size.
  print.matrix <- function(m) {
    write.table(format(m, justify = "left"),
                row.names = T, col.names = F, quote = F)
  }
  sequences <- getCombinedSequences(object)
  
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


# --------------------------------------------
# Generic functions for randSeq
# --------------------------------------------

#' Theoretical probability for randomization sequences
#'
#' Calculate theoretical probability for observed randomization sequences
#'
#' @aliases getProbabilities calculateProbabilities calcProb
#' 
#' @param obj object of a class inheriting from randSeq. Formal representation 
#' of a randomization sequences together with the parameters that belong to
#' the procedure that generated the sequences.
#'
#' @examples 
#' myPar <- bsdPar(10, 2)
#' M <- genSeq(myPar, 2)
#' getProb(M)
#' 
#' # all Sequences
#' par <- pbrPar(bc=c(2,2))
#' refSet <- getAllSeq(myPar)
#' probs <- getProb(refSet)
#' 
#' # sequences with probabilities
#' cbind(probs, refSet$M)
#' 
#' @name getProbabilities
NULL

#' @rdname getProbabilities
#'
#' @export
setGeneric("getProb", function(obj) standardGeneric("getProb"))


