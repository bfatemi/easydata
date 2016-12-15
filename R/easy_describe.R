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
#' easy_describe(as.data.table(iris))
easy_describe <- function(DT, cols=NULL, cclass = NULL, FUN = NULL, all=NULL){
    
    if(!is.data.table(DT)) stop("DT should be data.table class")
    
    ccdt <- pcc(DT, bret = TRUE) # start the descriptive table
    
    ## count of unique values
    ## count of NAs per columns
    ##
    cols <- ccdt[, CName]   # get colnames of DT to describe
    
    ccdt[, count_nonNA   := sapply(cols, function(i) sum(DT[, !is.na(get(i))]))]
    ccdt[, count_NA     := nrow(DT) - count_nonNA]
    ccdt[, count_unique := sapply(cols, function(i) length(unique(DT[, get(i)])))]
    ccdt[, pct_true := NA_real_]
        
    ## date range of date class columns
    ##
    dcols <- ccdt[Class == "Date", CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols)
            ccdt[CName == d, 
                 range_values := paste0("(", min(DT[!is.na(get(d)), get(d)]), "):(", max(DT[!is.na(get(d)), get(d)]), ")")]
    }
    
    dcols <- ccdt[Class %in% c("integer", "numeric"), CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols)
            ccdt[CName == d, 
                 range_values := paste0("(", min(DT[!is.na(get(d)), get(d)]), "):(", max(DT[!is.na(get(d)), get(d)]), ")")]
    }
    
    dcols <- ccdt[Class == "logical", CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols){
            ccdt[CName == d, range_values := "(FALSE):(TRUE)"]
            ccdt[CName == d, pct_true := DT[!is.na(get(d)), round(sum(get(d))/count_nonNA, 3)]]
        }
    }
    setnames(ccdt, c("Pos", "CName", "Class"), c("col_position", "col_name", "col_class"))
    setcolorder(ccdt, c("col_position", "col_name", "col_class", "count_unique", 
                        "count_NA", "count_nonNA", "range_values", "pct_true"))
    return(ccdt[])
    
}

globalVariables(c("count_unique", "count_NA", "count_nonNA", "CName", "Class", "range_values", "pct_true"))



