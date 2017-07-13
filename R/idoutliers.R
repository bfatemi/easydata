#' A function that identifies outliers in a vector of numeric data
#' 
#' Identify outliers using the standard IQR approach or assuming a normal distribution 
#' and identifying outliers using probability. See details below for description of the
#' calculations.
#' 
#' IQR approach: outliers are defined as observations that fall above 1.5 times the third 
#' quantile and below 1.5 times the first quantile.
#' 
#' Probability approach: assuming a normal distribution of values, outliers are flagged as 
#' values that have a 5% probability (parameterized for user control) of occuring given the
#' sample data. This method uses both tails of the distribution (e.g. values that are below 
#' and above the mean)
#'  
#' @param x A vector that is of class numeric, or coercible to one
#' @param method The method to use in order to identify outliers. This should be one of "quantile" or "prob"
#' @param p If method "prob" is selected, this parameter defines the probability threshold for 
#' outlier classification
#' 
#' @return A logical vector where TRUE identifies the position of an outlier in the input argument
#' 
#' @export
#' @examples 
#' #vec <- rnorm(100, 0, 1)
#' #bout <- id_outliers(vec)
#' #dt <- data.table(Values = vec, bOutlier = bout)
#' #dt[bOutlier == TRUE]
id_outliers <- function(x = NULL, method = c("quantile", "prob"), p = .05){
    method <- match.arg(method, c("quantile", "prob"))
    
    if(is.null(x))
        stop("Input x is null", call. = FALSE)
    if(!is.numeric(x)){
        warning("coercing x to class numeric", call. = FALSE)
        x <- suppressWarnings(as.numeric(x))
    }
    
    # x <- x[!is.na(x)]
    
    if(length(x)==0)
        stop("length of x is 0", call. = FALSE)
    
    
    if(method == "quantile"){
        # interquantile range
        q3 <- stats::quantile(x, .75)
        q1 <- stats::quantile(x, .25)
        
        lower <- q1 - 1.5*(q3 - q1)
        upper <- q3 + 1.5*(q3 - q1)
        
        return(x < lower | x > upper)    
    }
    
    if(method == "prob"){
        res <- round(pnorm(x, mean(x), sd(x), FALSE), digits = 5)
        return(res < p | res > 1-p)
    }
    
    stop("Incorrect method specified. Should be one of: prob, quantile")
}
