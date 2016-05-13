#' Quick filtering rows or columns
#'
#' \code{rFilter_All} is a function that enables quick filtering a data.table based on
#'  a TRUE/FALSE condition applied down the column (e.g. returns entire row if
#'  condition is TRUE for each value down rows of specified columns).
#'  \code{cFilter} is analogous for columns. This will test a condition across rows
#'  of every specified columns and drop each if the condition is met.
#' @param dt A data.table to filter rows or columns
#' @param val A value to filter rows or columns on
#' @param boolfun A function that can be used instead of val to filter. This function
#'      must be a "predicate" function applied across rows (\code{cFilter}), or down 
#'      columns (\code{rFilter_All}). See details and examples below.
#'      (returns T/F based on a test of values in dt
#'      applied across rows, or down columns)
#' @param cols An OPTIONAL character vector specifying columns to include as part of
#'      testing the conditions. If cols is not provided, all column of dt will be 
#'      tested
#' @param copy A boolean indicating whether the returned data.table should be a 
#'      copy of "dt" or a reference to the data in memory. Default value is TRUE to protect
#'      against unnexpected changes to the original data, however setting copy=FALSE when
#'      appropriate is much more efficient. See example use cases.
#' @param set An optional list element that can be provided to set a desired new column after filtering.
#'      set should be a named list where the name corresponds to the new column name, and the value
#'      is the new column value. This is ideal when needing to quick boolean or indicator column to 
#'      identify the group that was just filtered. See examples
#' @describeIn rFilter_All A function to filter rows of a data.table
#' @export
#' 
rFilter_All <- function(dt, val=NA, boolfun=NULL, cols=NULL, copy=TRUE){
    checkdt(dt, cols)
    
    if(copy)
        dt <- copy(dt)
    
    if(is.null(cols))
        cols <- names(dt)
    
    res <- dt[Reduce("&", lapply(dt, function(i) !is.na(i)))]
    
    if(nrow(res)==0)
        stop("No data after applying filter", call. = F)
    
    return(res)
}

#' @describeIn rFilter_All A function to apply a text based search to a given data table's rows and columns,
#'      returning rows where any column's value meets the search condition
#' @param exact A boolean that describes whether to exactly match. Default is FALSE which means a text based
#'      search will occur in order to match whether val is contained in the row
#' @export
rFilter <- function(dt, val=NA, exact=FALSE, set=NULL){
    if(is.na(val))
        dt <- dt[Reduce("|", lapply(colnames(dt), function(i) is.na(get(i))))]
    else if(exact)
        dt <- dt[Reduce("|", lapply(colnames(dt), function(i) get(i)==val))]
    else
        dt <- dt[Reduce("|", lapply(colnames(dt), function(i) grepl(val, get(i), ignore.case = T)))]
    
    if(!nrow(dt))
        stop("No entries for search criteria", call. = F)
    
    if(!is.null(set)){
        cname <- names(set)
        
        if(is.null(cname))
            stop("Argument 'set' is unnamed. Please provide name for new column")
        
        set(dt, j=cname, value=set[[1]])
    }
    return(dt)
}

#' @describeIn rFilter_All A function to filter columns of a data.table
#' @export
cRemoveNA <- function(dt){
    cols <- copy(colnames(dt))
    for(k in cols){
        if(sum(is.na(dt[, k, with=F])) == nrow(dt))
            set(dt, j=k, value=NULL)
    }
}

#' @describeIn rFilter_All A function to filter columns of a data.table
#' @export
cFilter <- function(dt, val=NA, boolfun=NULL, cols=NULL){
    
    
    
}