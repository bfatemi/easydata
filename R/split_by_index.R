#' Split into List
#'
#' Split into list element chunks using a given index that identifies the split points
#'
#' @param dat A matrix, data.frame, data.table, or vector. If table form, split will occur at rows
#' @param index A numeric vector that identifies the positions for which to split at
#' @param include_at_index A logical indicating whether to include the element at the index, or drop it.
#' The default value of TRUE will keep the element in result
#' @param include_first A logical indicating whether it is required to keep elements leading up to the first
#' index. For example, if I want to split a vector at position 5, 10, and 15, the default value of TRUE would ensure
#' elements 1 to 5 are returned (or 1 to 4 if argument \code{include_at_index} were set to FALSE)
#'
#' @importFrom data.table shift
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