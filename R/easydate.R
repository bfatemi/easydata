#' \code{MorphToDate} is a convenience wrapper around  that efficiently splits and subsets a data.table (DT) and
#' returns a single new DT (if no splitting) or multiple DTs within a list.
#'
#' @param DT A data.table to morph column classes
#' @param format For \code{DateMorph} only. Format of the value to convert to date. See examples for acceptable formats.
#' @param tz A character value indicating the timezone. Options are: "current", "utc", and "gmt"
#' @return A list of data.tables or a single data.table that is
#' the result of a provided data.table being split/subsetted
#' @export
DateMorph <- function(dt, format=NULL, tz=c("current","utc","gmt"), cols=NULL, copy=FALSE){
    tz <- match.arg(type)
    switch(tz,
           current = "",
           utc = "UTC",
           gmt = "GMT")
    
    # no return. checks conditions of data.table (nrows>1, is data.table, cols exist)
    checkdt(dt, cols)
    
    if(missing(format))
        format = "%Y-%mm-%dd"
    
    for(k in cols){
        set(dt, j=k, value = as.POSIXct(dt[,get(k)], "UTC"))
    }
    
}