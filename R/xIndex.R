#' Generate Indices
#' 
#' A function to create a split index. Comes in handy if parallelizing a computation over a very large dataset
#' 
#' @param totalN A numeric value representing the total observations to split an index over
#' @param eachN A numeric value representing the desired number of splits
#' @param mat A boolean indicating whether to return the splits as a list or matrix (simplified)
#'
#' @export
#'
#' @examples
#' totalN <- 1000
#' eachN <- 10
#' SplitIndex(totalN, eachN) # Not simplified
#' SplitIndex(totalN, eachN, mat = TRUE) # simplified
SplitIndex <- function(totalN=NULL, eachN, mat=FALSE){
    # totalN <- 99962
    # eachN <- 338
    eachLen <- totalN %/% eachN
    remainLen <- totalN - eachN * eachLen
    adjLen <- totalN-remainLen
    
    last <- NULL
    if(remainLen > 0){
        warning("eachN is not a factor of the total length of the index. 
                Choosing closest number and last index element will be of different length")
        last <- (adjLen + 1):(totalN)
    }
        
    a <- seq(1, adjLen, by = eachLen)
    b <- seq(eachLen, adjLen, by = eachLen)
    
    if(mat){
        if(remainLen > 0)
            warning("matrix will not cover total length. eachN not an integer factor")
        return(mapply(`:`, a, b, SIMPLIFY = FALSE))
    }else{
        return(c(mapply(`:`, a, b, SIMPLIFY = FALSE), list(last)))
    }
    
}



