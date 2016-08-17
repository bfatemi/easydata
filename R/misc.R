#' Miscellaneous Functions
#'
#' @param i A data table to clean or dedup
#' @param DateVector A vector of values of class POSIXct
#' @param cols columns of a data.table to remove duplicate rows accross
#' @param verbose A boolean indicating whether to print information on the console
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

#' @describeIn CleanCols A function to remove duplicates across all columns (default),
#'          or a given set of columns
#' @export
ddup <- function(i, cols=NULL, verbose=TRUE){
    cDT <- copy(i)
    cnames <- colnames(cDT)
    setkeyv(cDT, cnames) # set all but change if needed below
    
    if(!is.null(cols)){
        # if none of cols exist - error. If some exist- warning
        if(!any(cols %in% cnames))
            stop("Provided names not valid columns in the data", call. = FALSE)
        if(!all(cols %in% cnames))
            warning("Some cols not in data. Using those that exist", call. = FALSE)
        
        setkeyv(cDT, cols[which(cols %in% cnames)]) # will capture all if warning is not relevent
    }
    
    del <- nrow(i) - nrow(cDT)
    fubaR::PrintMessage(paste0("Removed ", del, " rows from total ", nrow(i), " rows"))
    return(cDT)
}