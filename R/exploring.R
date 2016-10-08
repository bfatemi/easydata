#' Functions to help with pre-data exploration
#' 
#' Helper functions to assist with basic data exploration tasks.
#' 
#' @param DT A data.table  
#' @param all Boolean indicating whether to perform for all columns (default is FALSE). If 
#'      this is false and optional argument "cols" was also not provided, the default behavior 
#'      is to perform this for only factor and character column classes in DT
#' @param cols optional arg providing column names to operate on
#'
#' @return A descriptive data.table
#' @export
#'
#' @examples
#' # dtDescribe(DT)
dtDescribe <- function(DT, all=FALSE, cols=NULL){
    ccdt <- pcc(DT, bret = TRUE)
    
    if(all){
        ccdt[, CountUnique := sapply(CName, function(i) length(unique(DT[, get(i)])))]
    }
    else if(!is.null(cols)){
        ccdt[, CountUnique := sapply(cols, function(i) length(unique(DT[, get(i)])))]
    }
    else{
        e <- substitute(Class %in% c("factor", "character"))
        cols <- ccdt[eval(e), CName]
        
        if(length(cols) == 0) 
            stop("cols not in DT")
        
        ccdt[eval(e), CountUnique := sapply(cols, function(i) length(unique(DT[, get(i)])))]
    }
    
    datecols <- ccdt[Class == "Date", CName]
    if(length(datecols) > 0){
        for(d in datecols)
            ccdt[CName == d, DateRange := paste0(min(DT[, get(d)]), ":", max(DT[, get(d)]))]
    }
        
    return(ccdt[])
}

globalVariables(c("CountUnique", "CName", "Class"))