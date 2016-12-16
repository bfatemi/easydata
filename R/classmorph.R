#' Functions to Change Column Classes
#' 
#' The functions \code{ClassMorph}, and \code{NumMorph}, in addition to
#' \code{DateMorph}, are functions designed to make dealing with column classes
#' easy. See examples for how to avoid uncessary frustrations related to
#' unexpected column classes or date formats.
#' 
#' This function takes care of multiple conversion steps that are sometimes
#' needed to avoid data loss or unexpected results (e.g. a factor that "looks"
#' like the number "1" may not convert to a \code{numeric} "1". To get around
#' this common frustration, this function will perform an indirect conversion to
#' \code{character} before converting to \code{numeric})
#' 
#' @param DT A data.table to use for any "class" operations
#' @param old ClassMorph only. A column type, specified as a character value, to
#'   detect and convert to the class specified by 'new'
#' @param new ClassMorph only. A column type, specified as a character value, to
#'   convert all columns of the class specified by 'old' to.
#' @param cols An optional character vector of column names to operate on. Not
#'   applicable for function ClassMorph.
#' @param copy A boolean value indicating whether to alter \code{DT} in memory
#'   or whether to return a new (copy) of the input data.table.
#' @param force A boolean indicating whether to force conversion from class
#'   factor to class numeric despite NAs being generated.
#' @param verbose A boolean indicating whether to be chatty
#' @return Returns a data.table that is either a copy of the input DT that has
#'   been modified, or the same input DT that has been modified in memory
#' @export
#' @describeIn ClassMorph A function to convert all columns of class "old" to class "new". This
#' function handles the indirect conversions sometimes needed to avoid data loss or
#' unexpected results (see details).
#' 
#' @import data.table
#' @example inst/examples/ex-cleaning.R
ClassMorph <- function(DT,
                       old   = c("factor","integer","character","numeric","logical", "Date"),
                       new   = c("factor","integer","character","numeric"),
                       copy  = FALSE,
                       force = FALSE){
    
    old <- match.arg(old) # Capture args
    new <- match.arg(new)
    
    if(copy) DT <- copy(DT)
    
    if(!data.table::is.data.table(DT))
        stop("Data should be class data.table. Use 'as.data.table'", call. = FALSE)
    
    
    # NEED TO IMPLEMENT force
    # trace how NAs are/could be generated and return error. recommend to 
    # rerun with force=TRUE
    # ERROR WITH FACTOR TO INTEGER. NO CONVERSION TO CHARACTER FIRST
    if(!old %in% sapply(DT, class)){
        warning("No columns of class specified with the argument 'old'", call. = FALSE)
        return(DT[])
    }

    
    
    colK <- quote(DT[[k]])
    
    if(old == "factor" & (new %in% c("integer", "numeric")))
        colK <- call("as.character", quote(DT[[k]]))
    
    oldcols <- names(which(sapply(DT, class) == old)) # Get position of cols to convert
    
    
    for(k in oldcols){
        
        if(!copy) copycol <- copy(DT[[k]]) # if we are converting by ref, copy in case NAs are generated unexpectadly
        
        newcol <- do.call(paste0("as.", new), list(colK))
        tryCatch(set(DT, j = k, value = newcol),
                 warning = function(c){
                     if(!force){
                         if(!copy) DT[, (k) := copycol] # reset data table
                         stop(paste0("Converting", k, " generated NAs. Set force=TRUE if desired."),call. = FALSE)    
                     }
                     suppressWarnings(set(DT, j = k, value = newcol))
                 })
    }
    return(DT[])
}

#' @describeIn ClassMorph A function to standards classes to "numeric" for all columns of
#'      DT, where a conversion to numeric would not generate NA values. 
#' @export
#' @import data.table
NumMorph <- function(DT, cols=NULL, copy=FALSE, verbose=FALSE){
    # checkdt(DT, cols)

    if(copy) DT <- copy(DT)
    
    if(is.null(cols)) cols <- names(DT)
    
    # recursive function to catch warning if value generated error, and skip the column
    f <- function(i){
        if(!is.na(cols[i])){
            tryCatch({
                set(DT, j=cols[i], value = as.numeric(DT[, get(cols[i])]))
                f(i + 1)
            }, warning = function(e){
                if(verbose)
                    warning(paste0("Conversion generated NA. Skipping column: ", cols[i]), call. = F)
                f(i + 1)
            })
        }
    }
    f(1)
    return(DT)
}

# #' @describeIn ClassMorph returns column classes
# #' @export
# cc <- function(DT){
#     sapply(DT, class)
# }


#' @describeIn ClassMorph prints (default) or returns a data.table describing column classes
#' @param ord For convenience, an optional char vec indicating how to sort rows. Can be one 
#'          of: "CName", "Class", or "Pos" (position)
#' @param bret A boolean indicating whether to return the data.table (rather than print by default)
#' @import data.table
#' @export
#' 
#' @examples
#' pcc(iris)                       # print sorted by ord (default)
#' pcc(iris, "Class")              # print sorted by "Class"
#' pcc(iris, "CName", bret = TRUE) # sort and return data.table
#' 
pcc <- function(DT, ord=NULL, bret=FALSE){
    r <- sapply(DT, class)
    rdt <- data.table(CName = names(r), Class = r, Pos = 1:length(r))
    
    if(!is.null(ord)){
        ord <- match.arg(ord, choices = c("CName", "Class", "Pos"))
        setorderv(rdt, ord)
    } 
    if(bret) 
        return(rdt)
    print(rdt, row.names = FALSE)
}




