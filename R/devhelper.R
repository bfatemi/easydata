checkdt <- function(dt=NULL, cols=NULL){
    if(is.null(dt))
        stop("no data.table provided", call. = F)
    
    if(!is.data.frame(dt))
        stop("dt not of class data.table or data.frame", call. = F)
    
    if(nrow(dt)<1)
        stop("no data in dt")
    
    if(!is.null(cols)){
        if(sum(!cols %in% colnames(dt))){
            dne <- paste(cols[which(!cols %in% colnames(dt))], ", ")
            stop(paste0("the following cols not in data.table: ", dne), call. = F)
        }
    }
}