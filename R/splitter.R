#' Quick and Efficient Data Splitting
#'
#' \code{xSplit} is a function that efficiently splits a data.table (DT),
#' vector, or matrix based on criteria given by the user into multiple datasets
#' within a list.
#'
#' @param x A data.table, matrix, or vector to split into list elements
#' @param ll A list with named elements that correspond to named elements of
#' x. The elements of ll could be vectors of length greater than or equal to 1.
#' The number of elements in ll and the size of those elements determines the
#' number of elements in the return value. See examples below.
#' @return A list of data.tables or a single data.table that is
#' the result of a provided data.table being split/subsetted
#' @examples
#' dt <- data.table(ColA = c(1, 2, 3, 4, 5, 4, 4, 3, 3),
#'                  ColB = c("a", "b", "b", "c", "c", "c", "d", "e", "f"),
#'                  ColC = rnorm(9))
#'
#' # Split the data by two values from ColA and one value from ColB.
#' # This will return a list of three data.tables
#' #
#' xSplit(x=dt, list(ColA = 3:4, ColB = "c"))
#' @export

xSplit <- function(x=NULL, ll=NULL){

    # x is not optional
    if(is.null(x))
        stop("Missing x argument", call. = F)

    cols <- names(ll)

    if(!sum(cols %in% colnames(x)))
        stop("Not all columns in x", call. = F)

    f <- function(k){
        vals <- ll[[k]]
        lapply(vals, function(i) x[which(do.call(`==`, list(i, get(k))))])
    }
    sapply(cols, f, USE.NAMES = T)
}




