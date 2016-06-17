#' Summary Statistics 
#'
#' A set of functions to run a set of customized summary statistics on 1 numeric column of a data.table or
#' all columns of a data.table that contains only numeric values.
#' 
#' @param x An r expression naming the column to run a custom set of statistics functions on
#' @param lab A character value labeling a newly created column of results. Default is "Value"
#' @param d A numeric digit representing the number of digits to round in the final answer. Default is 3.
#' @param na.rm A boolean with a default of TRUE that indicates whether NAs should be removed from the 
#'      calculation.
#' 
#' @describeIn MorphStat A function to apply a set of summary statistics function 
#'      to a column in a data.table
#' @export
#' @examples
#' #
#' # Show examples here
#' #
MorphStat <- function(x, lab="Value", d=3, na.rm=TRUE){
    if(na.rm)
        x <- x[!is.na(x)]
    
    if(!is.numeric(x))
        x <- as.numeric(x)
    
    if(na.rm)
        x <- x[!is.na(x)]
    
    StatFuns <- list(nObs = length,
                     Sum  = sum,
                     Ave  = mean,
                     Min  = min,
                     Max  = max,
                     Median = stats::median,
                     SD     = stats::sd)
    dt <- lapply(StatFuns, function(f) round(f(x), d))
    setDT(dt)
    return(as.data.table(c(Var = lab, dt)))
}

#' @describeIn MorphStat A wrapper that has vectorized \code{MorphStat} over a number of columns specified by the 
#' argument \code{x}
#' @param dt A data.table to apply MorphStat through
#' @param cols An optional argument for \code{plyStats} that subsets the data.table by column
#'      prior to applying MorphStat functions
#' @export
plyStat <- function(dt, cols){
    f <- function(i) dt[, MorphStat(get(i), lab = i)]
    return(rbindlist(lapply(cols, f)))
}

