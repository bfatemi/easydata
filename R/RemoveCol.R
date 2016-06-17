#' Easy & flexible column deletion
#' 
#' The function \code{RemoveCols} helps to remove columns of a data.table, data.frame, list, 
#' or generally any object with named elements efficiently with an intuitive interface
#'
#' @param dt A data.table that contains columns to remove 
#' @param byname An optional character vector of length 1 or greater used when wanting to remove columns 
#'      by approximate (or exact) matching of names. If argument "exact" is TRUE (default), "byname"
#'      can be a vector of column names to remove in batch. If false, byname should be a pattern to
#'      match against the column names
#' @param byval An optional value to search through each column and find. If "all" is TRUE (default), every 
#'      value in the column has to match "byval", else only 1 match is required to flag column for
#'      removal. If "exact" is FALSE (default is TRUE), approximate text matching will occur on the values.
#' @param exact A boolean value with a default of TRUE indicating whether matching of column names or values
#'      should be an exact match or an approximate match based on pattern matching
#' @param all A boolean value with a default of TRUE indicating whether ALL values in a column should match
#'      the value provided by "byval". If FALSE, only one match is required to flag column for removal
#' @param copy A boolean with a default of TRUE indicating whether a copy of the data.table should be returned.
#'      If false, the data.table is altered in memory and the modified input "dt" is returned
#'
#' @return A data.table such that some columns have been removed. If no columns were flagged for removal, 
#'      an error code will be returned and a friendly message will be printed on the screen
#' @export
#' 
#' @import data.table
RemoveCols <- function(dt, byname=NULL, byval=NULL, exact=TRUE, all=TRUE, copy=FALSE){
    cnames <- copy(colnames(dt))
    if(copy) dt <- copy(dt)
    
    # priority:
    #       - Do byname first if given
    #       - Then byvalue if given
    if(!is.null(byname)){
        if(exact){
            if(!sum(byname %in% cnames))
                stop("no matched column names found", call. = FALSE)
            
            if(sum(byname %in% cnames) != length(byname))
                warning("some columns provided by 'byname' are not in the data table", call. = FALSE)
            
            for(col in cnames[which(cnames %in% byname)])
                set(dt, j=col, value=NULL)
        }else{
            for(col in cnames[unique(unlist(lapply(byname, grep, cnames)))])
                set(dt, j=col, value=NULL)
        }
    }
    
    if(!is.null(byval)){
        for(col in cnames){
            colvals <- dt[, col, with=FALSE]
            
            if(exact){
                if(all){
                    if(sum(colvals %in% byval) == nrow(dt))
                        set(dt, j=col, value=NULL)
                }else{
                    if(!sum(colvals %in% byval))
                        set(dt, j=col, value=NULL)
                }
            }else{
                
                e <- substitute(unlist(lapply(colvals, stringr::str_detect, val)))
                index <- do.call("|", lapply(byval, function(val) eval(e)))
                
                if(all){
                    if(sum(index) == nrow(dt))
                        set(dt, j=col, value=NULL)
                }else{
                    if(!sum(index)) set(dt, j=col, value=NULL)
                }
            }
        }
    }
    return(dt)
}

