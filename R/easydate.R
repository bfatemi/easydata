#' Functions to Change Column Classes
#'
#' The functions \code{\link{ClassMorph}}, and \code{\link{NumMorph}}, in addition to \code{DateMorph},
#' are functions designed to make dealing with column classes easy. See examples for how
#' to avoid uncessary frustrations related to unexpected column classes or date formats.
#'
#' @param dt A data.table to morph column classes
#' @param format Format of the value to convert to date. See examples for acceptable formats.
#' @param tz A character value indicating the timezone. Options are: "current", "utc", and "gmt"
#' @family class handler
#' @example /examples/example-classmorph.R
#' @export
#' 
#' @import data.table
DateMorph <- function(dt, cols=NULL, format=NULL, tz=c("current","utc","gmt"), copy=FALSE){
    if(missing(tz))
        tz <- "current"
    else
        tz <- match.arg(type)
    
    switch(tz,
           current = "",
           utc = "UTC",
           gmt = "GMT")
    
    # cols is not optional here (for now)
    if(is.null(cols))
        stop("Provide column name(s) for conversion to date class", call. = F)
    
    # perform general checks on dt
    checkdt(dt, cols)
    
    if(copy)
        dt <- copy(dt)
    
    if(missing(format))
        format = "%Y-%mm-%dd"
    
    for(k in cols){
        set(dt, j=k, value = as.POSIXct(dt[,get(k)], "UTC"))
    }
    
}