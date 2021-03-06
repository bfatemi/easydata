#' Quick and Efficient Data Splitting
#'
#' \code{easysplit} is a function that efficiently splits a data.table,
#' vector, or matrix based on criteria given by the user into multiple datasets
#' within a list.
#'
#' @param x A data.table, matrix, or vector to split into list elements
#' @param ll A list of named vectors where the vector names correspond to named elements of
#' x and such that the values dictate the groups to split "x" by. where the values  The elements of "ll" could be vectors of length greater than or equal to 1.
#' See examples below.
#' @return A list with one or more elements that are of the same class as "x" 
#' but have been split on values in "ll"
#' @section Note: The number of elements in "ll" and the size of those 
#' elements determines the number of elements in the return value.
#' @family quick wranglers
#' @examples
#' library(data.table)
#' 
#' dt <- data.table(ColA = c(1, 2, 3, 4, 5, 4, 4, 3, 3),
#'                  ColB = c("a", "b", "b", "c", "c", "c", "d", "e", "f"),
#'                  ColC = rnorm(9))
#'
#' # Split the data by two values from ColA and one value from ColB.
#' # This will return a list of three data.tables
#' #
#' easysplit(x=dt, list(ColA = 3:4, ColB = "c"))
#' @export
easysplit <- function(x=NULL, ll=NULL){

    # x is not optional
    if(is.null(x))
        stop("Missing x argument", call. = FALSE)

    cols <- names(ll)

    if(!sum(cols %in% colnames(x)))
        stop("Not all columns in x", call. = FALSE)

    f <- function(k){
        vals <- ll[[k]]
        out <- lapply(vals, function(i) x[which(do.call(`==`, list(i, get(k))))])
        names(out) <- vals
        return(out)
    }
    ret <- sapply(cols, f, USE.NAMES = TRUE)
    return(ret)
}


