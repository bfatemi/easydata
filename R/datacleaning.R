#' Functions to help with Data Cleaning
#'
#' @param i A data table to operate on
#' @param date_vec A vector of values of class POSIXct
#' @param cols columns of a data.table to remove duplicate rows accross
#' @param verbose A boolean indicating whether to print information on the console
#' @name dataclean
NULL

#' @describeIn dataclean A function to remove all columns that have only NA values 
#' @export
CleanCols <- function(i){
    f <- function(c){
        if(sum(is.na(c)) == length(c))
            return(FALSE)
        return(TRUE)
    }
    i[, sapply(i, f), with=FALSE]
}

#' @describeIn dataclean A function to remove na rows in specified columes
#' @export
narm_c <- function(i = NULL, cols=NULL){
    if(!data.table::is.data.table(i))
        stop("i must be a data.table", call. = FALSE)
    
    e <- substitute(!is.na(get(X)))
    i[eval(e, list(X = cols))]
}

#' @param digits A numeric indicating the number of digits to round in \code{RoundCols}
#' @param igncols A character vector indicating which columns to ignore 
#' @param b_skip A boolean indicating whether to skip non-numeric columns that \code{RoundCols} encounters
#' @param b_copy A boolean indicating whether to make a copy of the data, or operate on by reference
#' @describeIn dataclean A convienience wrapper for \code{round} that applies to all or a subset of cols
#' @export
RoundCols <- function(i, digits = 2, igncols = NULL, cols = NULL, b_skip=FALSE, b_copy=TRUE){
    # digits = 2
    # igncols = c("SummaryGrp", "NumObs_tn")
    dat <- data.table::copy(i)
    
    # ADD THIS FUNCTIONALITY (recursive call if copy = TRUE)
    # if(copy){
    #    return(RoundCols(cdt, digits, cols, b_skip, copy=FALSE))
    # }
    
    allcols <- colnames(dat)
    if(!is.null(igncols))
        allcols <- allcols[!allcols %in% igncols]
    
    if(!is.null(cols))
        cols <- cols[cols %in% allcols]
    else
        cols <- allcols
    
    if(!data.table::is.data.table(dat))
        stop("arg not class data.table. Use: 'as.data.table'", call. = FALSE)
    
    
    tmp <- sapply(cols, function(c) is.numeric(get(c, dat)))
    if(!all(tmp) & !b_skip)
        stop("some calls are not numeric. To skip non-numerics, use b_skip=TRUE")
    
    numCols <- tmp[which(tmp==TRUE)]
    skipped <- tmp[which(tmp==FALSE)]
    
    if(!length(numCols))
        stop(paste0("No numeric cols. All skipped: ", paste0(skipped, collapse = ", ")), call. = FALSE)
    
    
    # cols <- c("Value", "spec_lower", "spec_upper")
    
    for(c in cols)
        data.table::set(dat, j = c, value = round(get(c, dat), digits))
    
    return(dat)
}


#' @param aslast A boolean indicating whether arrange the non-order columns before or after (asLast = TRUE)
#'      the ordered ones
#' @describeIn dataclean A convienience wrapper for \code{data.table::setcolorder} that makes it easy 
#'      set the order of a subset of columns
#' @export
p_setcolorder <- function(i, cols=NULL, aslast=TRUE){
    if(is.null(cols))
        stop("provide a vector of column names to set order")
    
    cnam <- colnames(i)
    othercols <- cnam[!cnam %in% cols] 
    
    if(aslast)
        setcolorder(i, c(othercols, cols))
    else
        setcolorder(i, c(cols, othercols))
    
    content <- paste0(1:length(colnames(i)), ". ", colnames(i), collapse = "\n")
    fubar::PrintMessage("New column order set", content = content)
}


#' @describeIn dataclean A function to expand elements of a timestamp into 
#'      numerous columns in a new data.table
#' @export
xDate <- function(date_vec){
    DateDT <- data.table(
        Wkday = lubridate::wday(date_vec),       # day of week
        Monthday = lubridate::day(date_vec),     # day of month
        Yrday = lubridate::yday(date_vec),       # day of year
        Week = lubridate::week(date_vec),        # Week of year
        Month = lubridate::month(date_vec),      # month of year
        Qtr = lubridate::quarter(date_vec),      # quarter of year
        Year = lubridate::year(date_vec),        # Year
        Hour = lubridate::hour(date_vec),
        Minute = lubridate::minute(date_vec),
        Second = lubridate::second(date_vec)
    )
    return(DateDT)
}

#' @describeIn dataclean A function to remove duplicates across all columns (default),
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
    fubar::PrintMessage(paste0("Removed ", del, " rows from total ", nrow(i), " rows"))
    return(cDT)
}