#' Swap all instances in a data.table
#'
#' \code{xSwap} is a function that efficiently finds and replaces all instances of a given
#' value with another given value. This occurs accross all columns, unless an optional argument
#' specifying the columns is provided.
#'
#' @param x  A data.table, matrix, or vector to split into list elements
#' @param find  A value to find across all or a subset of columns. Default value to find is NA.
#' @param swap  A value to replace 'find' with
#' @param cols  An optional character vector giving the names of the columns to make the swap in.
#' If not provided, swap will happen across all columns. See examples below.
#' @param force A boolean value specifying whether to try and coerce column classes in the event
#' where the swap value is a different data type than the column. See examples below.
#' @param copy A boolean value indicating whether to make the swap on a COPY of the data. Default
#' is false and the swap is made in memory. See examples below.
#' @return A list of data.tables or a single data.table that is
#' the result of a provided data.table being split/subsetted
#' @examples
#' # Create sample data.table
#' dt <- data.table(A = c("A", NA, "A", "A", NA),
#'                  B = c(NA, "B", "B", "B", "B"),
#'                  C = rep(NA, 5),
#'                  D = c("D", "D", "D", NA, "D"))
#'
#'
#' # Simple use
#' xSwap(dt, find="A", swap = "AA")
#'
#'
#' \donttest{
#' # ERROR: swaping a numeric into char column. USE FORCE = TRUE
#' xSwap(dt, swap = 0)
#' }
#' xSwap(dt, swap = 0, force = T) # No error
#'
#'
#' # Make a copy of the table rather than an "IN-MEMORY" swap. This will return a NEW dt
#' xSwap(dt, swap = 0, force = T, copy = T)
#'
#'
#' # swap in 1 or more specific columns
#' xSwap(dt,
#'       find = "0",
#'       swap = "999",
#'       cols = c("A", "B"))
#' @export


xSwap <- function(x=NULL, find=NA, swap=NULL, cols=NULL, force=FALSE, copy=FALSE){

    if(is.null(x))
        stop("Provide data.table, matrix, or vector")

    # if copy, do not make replacement in memory. Return a NEW dt
    if(copy) x <- copy(x)

    # if cols not provided, do for all columns
    if(is.null(cols)) cols <- colnames(x)

    # if find is NA need to use "is.na"
    if(is.na(find))
        index <- lapply(cols, function(i) which(x[,is.na(get(i))]))
    else
        index <- lapply(cols, function(i) which(do.call(`==`, list(find, x[,get(i)]))))


    for(i in 1:length(index)){
        # if no values equal to "find", then next i
        if(!!length(index[[i]])){

            # we need to check column classes so the user knows if the swap value is
            # of a different column class. Try and force if "force" is specified
            #
            swapClass <- class(swap)
            colClass <- x[, class(get(cols[i]))]

            if(swapClass != colClass){
                if(!force)
                    stop("'swap' is not the same data type as column. Use 'force' if desired", call. = F)

                # if we are forcing, first equate classes by changing to character
                x[,(cols[i]):=as.character(x[, get(cols[i])])]
                swap <- as.character(swap)
            }

            # make the swap
            set(x, index[[i]], cols[i], swap)
        }
    }
    return(x)
}




