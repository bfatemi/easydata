#' Functions to help with Data Cleaning
#'
#' @param DT A data table to operate on
#' @param date_vec A vector of values of class POSIXct
#' @param cols columns of a data.table to focus the operation on
#' @param value In the case of booleanize, a value to look for throughout the table
#' @param not A boolean. Negates the output of \code{booleanize}
#' @param index In the case of \code{CleanRows}, if \code{index} is TRUE, 
#'      an Index column will be created to track which rows were removed
#' @param verbose A boolean indicating whether to print information on the console
#' @example inst/examples/ex-cleaning.R
#' @name dataclean
NULL

#' @describeIn dataclean A function to remove all columns that have ONLY NA values 
#' @export
CleanCols <- function(DT){
    f <- function(c){
        if(sum(is.na(c)) == length(c))
            return(FALSE)
        return(TRUE)
    }
    DT[, sapply(DT, f), with=FALSE]
}

#' @describeIn dataclean A function to remove na rows in specified columes
#' @export
CleanRows <- function(DT, cols = NULL, index = FALSE){
    DT <- CleanCols(DT)
    if(index) DT[, Ind := .I]
    if(is.null(cols)) cols <- colnames(DT)
    
    DT[Reduce("&", Booleanize(DT, cols, NA, TRUE))]
}

#' @describeIn dataclean A function to remove duplicates across all columns (default),
#'          or a given set of columns
#' @export
ddup <- function(DT, cols=NULL, verbose=TRUE){
    cDT <- copy(DT)
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
    
    del <- nrow(DT) - nrow(cDT)
    fubar::PrintMessage(paste0("Removed ", del, " rows from total ", nrow(DT), " rows"))
    return(cDT)
}


#' @describeIn dataclean A function that turns a data.table into all logical values based on finding the value arg
#'      in all or selected columns
#' @export
Booleanize <- function(DT=NULL, cols=NULL, value=NULL, not=FALSE){
    if(is.null(value)) stop("function needs value argument")
    if(!is.data.table(DT)) stop("DT must be of class data.table")
    if(is.null(cols)) cols <- colnames(DT)
    
    if(is.na(value))
        e <- substitute(is.na(get(i, DT)))
    else 
        e <- substitute(get(i, DT) == value)
    
    if(not) DT[, sapply(cols, function(i) !eval(e), simplify = FALSE)]
    else DT[, sapply(cols, function(i) eval(e), simplify = FALSE)]
}

#' @param digits A numeric indicating the number of digits to round in \code{RoundCols}
#' @param igncols A character vector indicating which columns to ignore 
#' @param b_skip A boolean indicating whether to skip non-numeric columns that \code{RoundCols} encounters
#' @param b_copy A boolean indicating whether to make a copy of the data, or operate on by reference
#' @describeIn dataclean A convienience wrapper for \code{round} that applies to all or a subset of cols
#' @export
RoundCols <- function(DT, digits = 2, igncols = NULL, cols = NULL, b_skip=FALSE, b_copy=TRUE){
    # digits = 2
    # igncols = c("SummaryGrp", "NumObs_tn")
    dat <- data.table::copy(DT)
    
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
p_setcolorder <- function(DT, cols=NULL, aslast=TRUE){
    if(is.null(cols))
        stop("provide a vector of column names to set order")
    
    cnam <- colnames(DT)
    othercols <- cnam[!cnam %in% cols] 
    
    if(aslast)
        setcolorder(DT, c(othercols, cols))
    else
        setcolorder(DT, c(cols, othercols))
    
    content <- paste0(1:length(colnames(DT)), ". ", colnames(DT), collapse = "\n")
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


