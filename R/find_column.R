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
#' #EXAMPLES TBD
find_column <- function(colName, DT){
    
    if(class(DT) != "data.table")
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