#' Quick filtering rows or columns
#'
#' \code{rFilter} is a function that enables quick filtering a data.table based on
#'  a TRUE/FALSE condition applied down the column (e.g. returns entire row if
#'  condition is TRUE for each value down rows of specified columns).
#'  \code{cFilter} is analogous for columns. This will test a condition down rows
#'  of every specified column and drop the column if the condition is met.
#' @param dt A data.table to filter rows or columns
#' @param val A value to filter rows or columns on
#' @param boolfun A function that can be used instead of val to filter. This function
#'      must be a "predicate" function applied across rows (\code{cFilter}), or down 
#'      columns (\code{rFilter}). See details and examples below.
#'      (returns T/F based on a test of values in dt
#'      applied across rows, or down columns)
#' @param cols An OPTIONAL character vector specifying columns to include as part of
#'      testing the conditions. If cols is not provided, all column of dt will be 
#'      tested
#' @export
rFilter <- function(dt, val=NA, boolfun=NULL, cols=NULL){
    checkdt(dt, cols)
    
    if(copy)
        dt <- copy(dt)
    
    if(is.null(cols))
        cols <- names(dt)
    
    if(is.null(cols))
        cols <- colnames(dt)
    dt[Reduce("&", lapply(dt, function(i) !is.na(i)))]
}

cFilter <- function(dt, val=NA, boolfun=NULL, cols=NULL){
    
}