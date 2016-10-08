#' Functions to help with pre-data exploration
#' 
#' Helper functions to assist with basic data exploration tasks.
#' 
#' @param DT A data.table  
#' @param all Boolean indicating whether to perform for all columns (default is FALSE). If 
#'      this is false and optional argument "cols" was also not provided, the default behavior 
#'      is to perform this for only factor and character column classes in DT
#' @param cols optional arg providing column names to operate on
#' @param cclass Not implemented. Useful if desiring to describe all columns a particular class
#' @param FUN NI; provide function to apply over cols (if provided)
#'
#' @return A descriptive data.table
#' @export
#'
#' @examples
#' dtDescribe(as.data.table(iris))
dtDescribe <- function(DT, cols=NULL, cclass = NULL, FUN = NULL, all=NULL){
    if(!is.data.table(DT)) stop("DT should be data.table class")
    
    ccdt <- pcc(DT, bret = TRUE) # start the descriptive table
    
    ## count of unique values
    ## count of NAs per columns
    ##
    cols <- ccdt[, CName]        # get colnames of DT to describe
    ccdt[, count_narm   := sapply(cols, function(i) nrow(DT[!is.na(get(i))]))]
    ccdt[, count_na     := nrow(DT) - count_narm]
    ccdt[, count_unique := lapply(cols, function(i) length(unique(DT[, get(i)])))]
    
        
    ## date range of date class columns
    ##
    dcols <- ccdt[Class == "Date", CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols)
            ccdt[CName == d, range_date := paste0("(", min(DT[, get(d)]), "):(", max(DT[, get(d)]), ")")]
    }
    return(ccdt[])
}

globalVariables(c("count_unique", "count_na", "count_narm", "CName", "Class", "range_date"))


