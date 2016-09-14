#' Functions to Change Column Classes
#'
#' The functions \code{\link{ClassMorph}}, and \code{\link{NumMorph}}, in addition to \code{DateMorph},
#' are functions designed to make dealing with column classes easy.
#'
#' @section Important:
#' This function is deprecated while development of a complete rehaul is ongoing.
#' The new \code{DateMorph} has an algorithm to smartly identify date columns anywhere 
#' in the table, while also identifying the format of the dates, using \code{fudate} 
#' to do both, only then changing the column class to the appropriate date class
#' 
#' @param dt A data.table to morph column classes
#' @param format Format of the value to convert to date. See examples for acceptable formats.
#' @param tz A character value indicating the timezone. Options are: "current", "utc", and "gmt"
#' @inheritParams ClassMorph
#' 
#' @import data.table
DateMorph <- function(dt, cols=NULL, format=NULL, tz=c("current","utc","gmt"), copy=FALSE){
    warning("Function is deprecated. Please see details in the help page", call. = FALSE)
    
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
        stop("Provide column name(s) for conversion to date class", call. = FALSE)

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