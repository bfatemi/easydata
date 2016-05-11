rFilter <- function(dt, val=NA, boolfun=NULL, cols=NULL){
    checkdt(dt, cols)
    
    if(copy)
        dt <- copy(dt)
    
    if(is.null(cols))
        cols <- names(dt)
    
    if(is.null(cols))
        cols <- colnames(dt)
    dt[Reduce("&", lapply(dt, function(i) !is.na(i)))]
}

cFilter <- function(dt, val=NA, boolfun=NULL, cols=NULL){
    
}