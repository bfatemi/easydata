#' Find known column name in messy csv files
#' 
#' Function assists with finding a prior known column name in the event that a csv
#' file is read into and first few rows or columns are potentially junk. 
#' 
#' This function returns the position of the column in a list in a row, column order.
#' For example, a list with elements (0, 0) means column name was not found. A list with
#' first element 0 and last element > 0 means the column name is a current column name (0th row)
#' and at column position 1 or greater. (>0, >0) gives the literal index of the cell where
#' the searched name was found.
#'
#' @param colName A name representing a known column, to search for the position
#' @param DT A data.table where the searching should take place
#'
#' @return A 2 entry list representing the row and column where the search criteria was found
#' @export
#'
#' @examples
#' library(data.table)
#' ## Create fake data
#' ##
#' DT <- rbindlist(
#'     list(
#'         data.table(V1 = "Junk notes as first row", V2=NA, V3=NA, V4=NA), 
#'         data.table("colA", "colB", "colC", "colD")
#'     ), fill=TRUE)
#'     
#' ## Can't find name
#' find_column("does not exist", DT) # (0, 0)
#' 
#' ## Finds name as current column name
#' find_column("V3", DT) # (0, 3)
#' 
#' ## Finds a name
#' find_column("colA", DT)
find_column <- function(colName, DT){
    
    if(!is.data.table(DT))
        stop("DT should be of class 'data.table'")
    
    cnames <- colnames(DT)
    if(colName %in% cnames)
        return(list(nrow=0, ncol = which(cnames == colName)))
    
    ## at this point we know colName is not part of the column names of DT
    ## Now search each column through the first 30 rows for the location
    for(k in 1:length(cnames)){
        tmp <- which(DT[, k, with=FALSE] == colName)
        
        if(length(tmp) > 0)
            return(list(nrow = tmp, ncol = k))
    }
    
    warning(colName, " Not Found")
    return(list(nrow = 0, ncol = 0))
}