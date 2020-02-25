###############################################
# --------------------------------------------#
# Functions for the doubly t-distribution     #
# --------------------------------------------#
###############################################

#' Approximation of the distribution function of the doubly noncentral t-distribution 
#'
#' Computes the value of the distribution function of the doubly noncentral t-distribution at \code{x}.
#'
#' @inheritParams overview
#' @keywords internal
#' @return Distribution value of the doubly noncentral t-distribution at \code{x}.
doublyT <- function(x, df, lambda1, lambda2, lb = 0, ub) {
  values <- lb:ub
  suppressWarnings(sum(dpois(values, lambda2/2)*pt(x*(1+2*values/df)^(0.5), df+2*values, lambda1)))
}

#' Calculation of the NCPs of each randomization sequence for the doubly noncentral t-distribution
#'
#' Computes the noncentrality parameters delta and lambda for the doubly noncentral t-distribution of each randomization sequence.
#'
#' @param randSeq object of the class randSeq.
#' @param bias object of the class bias.
#' @param endp object of the class endpoint.
#' @keywords internal
#' @return matrix containing the noncentrality parameters delta and lambda of all randomization sequences.
genNcps <- function(randSeq, bias, endp, weight = FALSE, power = FALSE ) {
  stopifnot(is(randSeq, "SeqObj"), randSeq@K == 2,
            is(bias, "issue"), is(endp, "endpoint"), sum(duplicated(endp@sigma)) == 1)
  
  # we need to iterate over all randSeq and bias objects
  iter = length(randSeq)
  
  # number of patients allocated to treatment 0
  n_C <- lapply(1:iter, function(i){
    ass_matrix <- randSeq[[i]]@M
    sapply(1:dim(ass_matrix)[1], function(x) sum(ass_matrix[x,]))
  })
  
  # number of patients allocated to treatment 1
  n_E <- lapply(1:iter, function(i){
    temp_N <- rep(randSeq[[i]]@N, dim(randSeq[[1]]@M)[1])
    temp_N - n_C[[i]]
  })
  
  
  # get expectations based on the bias chosen
  expectations <- list()
  for(i in 1:iter){
    
    expectations[[i]] <- matrix(0, dim(randSeq[[i]]@M)[1], randSeq[[i]]@N)
    for(x in 1:dim(randSeq[[i]]@M)[1]){
      temp_randSeq <- genSeq(crPar(randSeq[[i]]@N))
      temp_randSeq@M <- t(matrix(randSeq[[i]]@M[x, ]))
      expectations[[i]][x, ] <- getExpectation(temp_randSeq, bias[[i]], endp);
    }
  }
  
  # 
  avgExp <- list()
  for(i in 1:iter){
    avgExp[[i]] <- matrix(0, dim(randSeq[[i]]@M)[1], 2)
    for(x in 1:dim(randSeq[[i]]@M)[1]){
      splitGroups <- split(expectations[[i]][x,], randSeq[[i]]@M[x,])
      # average expectations in both groups
      avgExp[[i]][x,1] <- mean(splitGroups$`0`)
      avgExp[[i]][x,2] <- mean(splitGroups$`1`)
    }
    avgExp[[i]][which(is.na(avgExp[[i]]))] <- 0
  }
  
  temp_delta <- list()
  norm_factor <- list()
  for(i in 1:iter){
    if(power){
      if(weight){
        temp_delta[[i]] <- sapply(1:dim(randSeq[[i]]@M)[1], function(x){(0.635)}) 
      }else{
        temp_delta[[i]] <- sapply(1:dim(randSeq[[i]]@M)[1], function(x){(n_E[[i]][x]*n_C[[i]][x]/(n_E[[i]][x]+n_C[[i]][x]))*(0.635)}) 
      }
      temp_delta[[i]][which(is.na(temp_delta[[i]]))] <- 0
      norm_factor[[i]] <- sapply(1:dim(randSeq[[i]]@M)[1], function(x){n_E[[i]][x]*n_C[[i]][x]/(n_E[[i]][x]+n_C[[i]][x])}) 
    }else{
      if(weight){
        temp_delta[[i]] <- sapply(1:dim(randSeq[[i]]@M)[1], function(x){(avgExp[[i]][x,1]-avgExp[[i]][x,2])}) 
      }else{
        temp_delta[[i]] <- sapply(1:dim(randSeq[[i]]@M)[1], function(x){(n_E[[i]][x]*n_C[[i]][x]/(n_E[[i]][x]+n_C[[i]][x]))*(avgExp[[i]][x,1]-avgExp[[i]][x,2])}) 
      }
      temp_delta[[i]][which(is.na(temp_delta[[i]]))] <- 0
      norm_factor[[i]] <- sapply(1:dim(randSeq[[i]]@M)[1], function(x){n_E[[i]][x]*n_C[[i]][x]/(n_E[[i]][x]+n_C[[i]][x])}) 
    }
  }
  
  weight_no_star <- Reduce(`+`, norm_factor)
  if(weight)
    weight_no_star <- rep(1, length(norm_factor[[1]]))
  # delta - non centrality parameter
  #delta = do.call(sum, temp_delta)/(endp@sigma[1]*sqrt(do.call(sum, norm_factor)))
  delta = Reduce(`+`, temp_delta)/(endp@sigma[1]*sqrt(weight_no_star^2/Reduce(`+`, norm_factor)))
  delta[which(is.na(delta))] <- 0
  
  
  temp_expSum <- list()
  for(i in 1:iter){
    temp_expSum[[i]] <- rowSums(expectations[[i]]^2) - n_E[[i]]*(avgExp[[i]][,1]^2) - n_C[[i]]*(avgExp[[i]][,2]^2)
  }
  # lambda - non centrality parameter
  lambda = (1/endp@sigma[1]^2) * Reduce(`+`, temp_expSum)
  return(list(delta, lambda))
}


#' Calculation of the biased type-one-error probability (resp. power) of Student`s t-test
#'
#' Computes the biased type-one-error probability (resp. power) of Student`s t-test due to shifts in the expectation vectors 
#' in both treatment groups.
#'
#' @param randSeq object of the class randSeq.
#' @param bias object of the class bias.
#' @param endp object of the class endpoint.
#' @keywords internal
#' @return the biased type-one-error probability (resp. power) of all randomization sequences.
doublyTValues <- function(randSeq, bias, endp) {
  stopifnot(is(randSeq, "SeqObj"), randSeq@K == 2,
            is(bias, "issue"), is(endp, "endpoint"), sum(duplicated(endp@sigma)) == 1)   
  # calculation of of the matrix containing the ncp
  ncps <- genNcps(randSeq, bias, endp)
  # variable for the defined alpha quantile
  alpha <- bias@alpha
  # function for calculating the p values of the singular randomization sequences
  p.value <- sapply(1:length(npcs[[1]]), function(i) {
      delta <- ncps[[1]][i]
      lambda <-ncps[[2]][i]
      # lower boundary
      lb <- max(floor(lambda/2 - qpois(.995, lambda/2)), 0)
      # upper boundary
      ub <- as.vector(ceiling(lambda/2 + qpois(.995, lambda/2)))
      # degrees of freedom
      #df <- sum(N-2)
      # t quantiles
      tQuantLow <- qt(alpha/2, df)
      tQuantUpper <- -tQuantLow 
      p.value.less <- doublyT(tQuantLow , df, delta, lambda, lb, ub)
      p.value.greater <- 1 - doublyT(tQuantUpper, df, delta, lambda, lb, ub)
      p.value <- p.value.less + p.value.greater
      return(p.value)
    }
  )
  p.value
}

setClassUnion('SeqObj', c("randSeqs", 'randSeq'))
