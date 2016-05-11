#' Describing Data
#'
#' @description These functions help describe data.tables to get an initial view of
#'      what it contains, the size, the column classes, etc.
#' @param dt A data.table to describe
#' @return No returns. Prints information to the console.
#' @export
describe <- function(dt){
    str(dt, no.list = T, vec.len = 2, give.attr = F, give.length = F)
}
