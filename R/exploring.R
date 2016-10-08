#' Functions to help with pre-data exploration
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
        e <- Class %in% c("factor", "character")
        cols <- ccdt[eval(e), CName]
        
        if(length(cols) == 0) 
            stop("cols not in DT")
        
        ccdt[eval(e), CountUnique := sapply(cols, function(i) length(unique(DT[, "PortType", with=FALSE])))]
    }
    return(ccdt)
}
