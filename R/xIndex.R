#' Generate Indices
#' 
#' A function to create a split index. Comes in handy if parallelizing a computation over a very large dataset
#' 
#' @param len A numeric value representing the total observations to split an index over
#' @param eachN A numeric value representing the desired number of splits
#' @param mat A boolean indicating whether to return the splits as a list or matrix (simplified)
#'
#' @export
#'
#' @examples
#' len <- 1000
#' eachN <- 10
#' xIndex(len, eachN) # Not simplified
#' xIndex(len, eachN, mat = TRUE) # simplified
xIndex <- function(len=NULL, eachN, mat=FALSE){
    eachLen <- len/eachN        
    
    a <- seq(1, len, by = eachLen)
    b <- seq(eachLen, len, by = eachLen)
    
    mapply(`:`, a, b, SIMPLIFY = mat)
}