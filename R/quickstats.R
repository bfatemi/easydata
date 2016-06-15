#' Summary Statistics 
#'
#' A set of functions to run a set of customized summary statistics on 1 numeric column of a data.table or
#' all columns of a data.table that contains only numeric values.
#' 
#' @describeIn MorphStat A function to apply a set of summary statistics function to a column in a data.table
#' @param col An r expression naming the column to run a custom set of statistics functions on
#' @param lab A character value labeling a newly created column of results. Default is "Value"
#' @param d A numeric digit representing the number of digits to round in the final answer. Default is 3.
#' @param na.rm A boolean with a default of TRUE that indicates whether NAs should be removed from the 
#'      calculation.
#'
#' @export
#'
#' @examples
#' #
#' # Show examples here
#' #
MorphStat <- function(col, lab="Value", d=3, na.rm=TRUE){
    if(na.rm)
        col <- col[!is.na(col)]
    
    if(!is.numeric(col))
        col <- as.numeric(col)
    
    if(na.rm)
        col <- col[!is.na(col)]
    
    StatFuns <- list(nObs = length,
                     Sum  = sum,
                     Ave  = mean,
                     Min  = min,
                     Max  = max,
                     Median = median,
                     SD     = sd)
    dt <- lapply(StatFuns, function(f) round(f(col), d))
    setDT(dt)
    return(as.data.table(c(Var = lab, dt)))
}

#' @describeIn MorphStat A wrapper that has vectorized \code{MorphStat} over a number of columns specified by the 
#' argument \code{col}
#' 
#' @export
plyStat <- function(dt, cols){
    f <- function(i) dt[, MorphStat(get(i), lab = i)]
    return(rbindlist(lapply(cols, f)))
}

