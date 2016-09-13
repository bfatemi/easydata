#' A function that identifies outliers in a vector of numeric data
#'
#' @param x A vector that is of class numeric, or coercible to one
#'
#' @return A logical vector where TRUE identifies the position of an outlier in the
#'     input argument
#' @export
#' @examples 
#' #vec <- rnorm(100, 0, 1)
#' #bout <- id_outliers(vec)
#' #dt <- data.table(Values = vec, bOutlier = bout)
#' #dt[bOutlier == TRUE]
id_outliers <- function(x = NULL){
    if(is.null(x))
        stop("Input x is null", call. = FALSE)
    if(!is.numeric(x)){
        warning("coercing x to class numeric", call. = FALSE)
        x <- suppressWarnings(as.numeric(x))
    }
    if(length(x)==0)
        stop("length of x is 0", call. = FALSE)
    
    # interquantile range
    q3 <- stats::quantile(x, .75)
    q1 <- stats::quantile(x, .25)
    
    lower <- q1 - 1.5*(q3 - q1)
    upper <- q3 + 1.5*(q3 - q1)
    
    return(x < lower | x > upper)
}
