#' Swap all instances in a data.table
#'
#' \code{xSwap} is a function that efficiently finds and replaces all instances of a given
#' value with another given value. This occurs across all columns to use, unless an optional argument
#' specifying the columns is provided.
#'
#' @param dt  A data.table to search and swap values in
#' @param find  A value to find across all or a subset of columns. Default is NA.
#' @param swap  A value to replace 'find' with
#' @param cols  An optional character vector giving the names of the columns to make the swap in.
#'      If not provided, swap will happen across all columns. See examples below.
#' @param force A boolean value specifying whether to try and coerce column classes in the event
#'      where the swap value is a different data type than the column. See examples below.
#' @param copy A boolean value indicating whether to make the swap on a COPY of the data. Default
#'      is false and the swap is made in memory. See examples below.
#' @return A modified copy of dt
#' @family quick wranglers
#' @example /examples/example-swap.R
#' @export
xSwap <- function(dt=NULL, find=NA, swap=NULL, cols=NULL, force=FALSE, copy=FALSE){

    if(is.null(dt))
        stop("Provide data.table, matrix, or vector")

    # if copy, do not make replacement in memory. Return a NEW dt
    if(copy) dt <- copy(dt)

    # if cols not provided, do for all columns
    if(is.null(cols)) cols <- colnames(dt)

    # if find is NA need to use "is.na"
    if(is.na(find))
        index <- lapply(cols, function(i) which(dt[,is.na(get(i))]))
    else
        index <- lapply(cols, function(i) which(do.call(`==`, list(find, dt[,get(i)]))))


    for(i in 1:length(index)){
        # if no values equal to "find", then next i
        if(!!length(index[[i]])){

            # we need to check column classes so the user knows if the swap value is
            # of a different column class. Try and force if "force" is specified
            #
            swapClass <- class(swap)
            colClass <- dt[, class(get(cols[i]))]

            if(swapClass != colClass){
                if(!force)
                    stop("'swap' is not the same data type as column. Use 'force' if desired", call. = F)

                # if we are forcing, first equate classes by changing to character
                dt[,(cols[i]):=as.character(dt[, get(cols[i])])]
                swap <- as.character(swap)
            }

            # make the swap
            set(dt, index[[i]], cols[i], swap)
        }
    }
    return(dt)
}




