#' Split into List
#' 
#' Functions to work with indices, or split objects by indices. Functionality includes:
#'    - Split into list element chunks using a given index that identifies the split points
#'    - Create a split index for distributed analysis (parallelization, etc.)
#'    
#' @param dat A matrix, data.frame, data.table, or vector. If table form, split will occur at rows
#' @param index A numeric vector that identifies the positions for which to split at
#' @param include_at_index A logical indicating whether to include the element at the index, or drop it.
#' The default value of TRUE will keep the element in result
#' @param include_first A logical indicating whether it is required to keep elements leading up to the first
#' index. For example, if I want to split a vector at position 5, 10, and 15, the default value of TRUE would ensure
#' elements 1 to 5 are returned (or 1 to 4 if argument \code{include_at_index} were set to FALSE)
#' @param totalN A numeric value representing the total observations to split an index over
#' @param eachN A numeric value representing the desired number of splits
#' @param mat A boolean indicating whether to return the splits as a list or matrix (simplified)
#' @param vec A vector to split into n list elements
#' @param n The number of desired list elements to split vec into
#' 
#' @importFrom data.table shift
#' @name index_functions
NULL


# tpath <- system.file("ext", "template.yml", package = "spawnr")
# ci_template <- readLines(tpath)     # template stored in package folder
# 
# ## break template into named list elements
# index <- which(stringr::str_detect(ci_template, "^#\\-\\-$"))
# split_by_index(ci_template, index, include_first = FALSE, include_at_index=F)

#' @describeIn index_functions Split a vector or table by a position given by index vector
#' @export
split_by_index <- function(dat = NULL, index=NULL, include_first = TRUE, include_at_index = TRUE){
    
    # add unique in case position 1 was already included
    if(include_first == TRUE)
        index <- unique(c(1, index))
    
    n <- length(index)
    a <- index[-n]
    b <- data.table::shift(index, type = "lead")[-n]
    
    # shift elements to be disjoint such that index elements are dropped
    if(!include_at_index){
        a <- a + 1
        b <- b - 1
    }else{
        # shift so index elements are included in mutually exclusive sets
        # only shifting b is enough
        b <- b - 1
    }
    lapply(1:(n-1), function(i){
        dat[a[i]:b[i]]
    })
}

#' @describeIn index_functions Create a split index for distributed analysis (parallelization, etc.)
#' @export
#' @examples
#' totalN <- 1000
#' eachN <- 10
#' make_index(totalN, eachN) # Not simplified
#' make_index(totalN, eachN, mat = TRUE) # simplified
make_index <- function(totalN=NULL, eachN, mat=FALSE){
    eachLen <- totalN %/% eachN
    remainLen <- totalN - eachN * eachLen
    adjLen <- totalN-remainLen
    
    last <- NULL
    if(remainLen > 0)
        last <- (adjLen + 1):(totalN)
    
    a <- seq(1, adjLen, by = eachLen)
    b <- seq(eachLen, adjLen, by = eachLen)
    
    if(mat){
        if(remainLen > 0)
            warning("matrix will not cover total length. eachN not an integer factor")
        mapply(`:`, a, b, SIMPLIFY = TRUE)
    }else{
        c(mapply(`:`, a, b, SIMPLIFY = FALSE), list(last)[!is.null(last)])    
    }
}

#' @describeIn index_functions A convenience wrapper around splitIndices. Reduces a step potentially
#' @export
#' @examples
#' splitn(letters, 4)
splitn <- function(vec, n){
    lapply(parallel::splitIndices(length(vec), n), function(i) vec[i])
}
