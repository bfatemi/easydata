
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

ClassMorph <- function(dt,
                   old = c("factor","integer","character","numeric"),
                   new = c("factor","integer","character","numeric"),
                   copy = FALSE){
    # Capture args
    old <- match.arg(old)
    new <- match.arg(new)
    
    checkdt(dt)
    
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
    return(dt)
}

