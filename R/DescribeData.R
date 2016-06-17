#' Describing Data
#'
#' @description These functions help "describe" data.tables in order to
#'      get an initial view of what it contains, the size, the column 
#'      classes, etc.
#' @param dt A data.table to describe
#' @examples
#' library(data.table)
#' 
#' dt <- data.table(ColumnA = c(1,2,3),
#'                  ColumnB = c(4,5,6))
#' describe(dt)
#' 
#' # Classes 'data.table' and 'data.frame': 
#' #      3 obs. of 2 variables:
#' #  $ ColumnA: num  1 2 3
#' #  $ ColumnB: num  4 5 6
#' 
#' @describeIn describe A wrapper around function \code{str}, with preset parameters
#'      convient for objects of class data.table (data.frame)
describe <- function(dt){
    warning("This function is deprecated", call. = FALSE)
    str(dt, no.list = T, vec.len = 2, give.attr = FALSE, give.length = FALSE)
}
