#' Miscellaneous Functions
#'
#' @param i A data table to clean
#' @param DateVector A vector of values of class POSIXct
#'
#' @describeIn CleanCols A function to remove all columns that have only NA values 
#' @export
CleanCols <- function(i){
    f <- function(c){
        if(sum(is.na(c)) == length(c))
            return(FALSE)
        return(TRUE)
    }
    i[, sapply(i, f), with=FALSE]
}

#' @describeIn CleanCols A function to expand elements of a timestamp into 
#'      numerous columns in a new data.table
#' @export
xDate <- function(DateVector){
    DateDT <- data.table(
        Wkday = lubridate::wday(DateVector),       # day of week
        Monthday = lubridate::day(DateVector),   # day of month
        Yrday = lubridate::yday(DateVector),   # day of year
        Week = lubridate::week(DateVector),        # Week of year
        Month = lubridate::month(DateVector),      # month of year
        Qtr = lubridate::quarter(DateVector),      # quarter of year
        Year = lubridate::year(DateVector),         # Year
        Hour = lubridate::hour(DateVector),
        Minute = lubridate::minute(DateVector),
        Second = lubridate::second(DateVector)
    )
    return(DateDT)
}