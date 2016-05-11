
#' Functions to Change Column Classes
#'
#' @param dt A data.table with greater than 0 rows to operate on.
#' @param old ClassMorph only. A column type, specified as a character value, to detect
#'      and convert to the class specified by 'new'
#' @param new ClassMorph only. A column type, specified as a character value, to 
#'      convert all columns of the class specified by 'old' to. 
#' @param cols An optional character vector of column names to operate on.
#'      Not applicable for function ClassMorph.
#' @param copy A boolean value indicating whether to alter \code{dt} in memory or 
#'      whether to return a \italics{new (copy)} of the input data.table.
#' @param force A boolean indicating whether to force conversion from class factor
#'      to class numeric despite NAs being generated.
#' @return Returns value is a data.table that is either a copy of the input dt that 
#'      has been modified, or the same input dt that has been modified in memory
#' @export
#'
#' @examples "examples/example-classmorph.R"
ClassMorph <- function(dt,
                       old   = c("factor","integer","character","numeric"),
                       new   = c("factor","integer","character","numeric"),
                       copy  = FALSE,
                       force = FALSE){
    
    # NEED TO IMPLEMENT force
    # trace how NAs are/could be generated and return error. recommend to 
    # rerun with force=TRUE
    
    # ERROR WITH FACTOR TO INTEGER. NO CONVERSION TO CHARACTER FIRST
    
    # Capture args
    old <- match.arg(old)
    new <- match.arg(new)
    
    checkdt(dt)
    
    # Save original column order. Set at end
    colorder <- copy(colnames(dt))
    
    if(copy)
        dt <- copy(dt)
    
    # Get all pairs and id those that require indirect conversion
    # E.g.  direct:   factor -> numeric
    #       indirect: factor -> character -> numeric
    #
    types <- c("factor","integer","character","numeric")
    
    dtTypes <- CJ(From = types, To = types)
    setkey(dtTypes, From, To)
    
    dtTypes[.("factor", "numeric"), CharFirst := TRUE]
    dtTypes[is.na(CharFirst), CharFirst := FALSE]
    
    # If "from/to" pair requires indirect conversion, then wrap captured
    # column in a call to "as.character"
    #
    colK <- quote(dt[[k]])
    if( dtTypes[.(old, new), CharFirst] )
        colK <- call("as.character", colK)
    
    # Get position of cols to convert
    cols <- which(sapply(dt, class) == old)
    
    #k <- names(cols)[1]
    # For each col, evaluate call & set new
    for (k in names(cols)){
        ind <- !sapply(eval(colK), is.null)
        val <- eval(call(paste0("as.", new), colK))
        newk <- paste0("new_", k)
        set(dt, j = newk, value = eval(call(paste0("as.", new), rep(NA, nrow(dt)))))
        set(dt, i = which(ind), j = newk, value = val)
        set(dt, j = k, value = NULL)
        setnames(dt, newk, k)
    }
    
    # reset column orders
    setcolorder(dt, colorder)
    
    return(dt)
}

#' @describeIn ClassMorph
#' @export
NumMorph <- function(dt, cols=NULL, copy=FALSE){
    checkdt(dt, cols)

    if(copy)
        dt <- copy(dt)
    
    if(is.null(cols))
        cols <- names(dt)
    
    # recursive function to catch warning if value generated error, and skip the column
    f <- function(i){
        if(!is.na(cols[i])){
            tryCatch({
                set(dt, j=cols[i], value = as.numeric(dt[, get(cols[i])]))
                f(i + 1)
            }, warning = function(e){
                warning(paste0("Conversion generated NA. Skipping column: ", cols[i]))
                f(i + 1)
            })
        }
    }
    f(1)
    return(dt)
}



