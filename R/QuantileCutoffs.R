#' Calculate Quantile Cutoffs
#' 
#' This function takes in a data.table, the column name of a measurement
#' variable, the column name of a group variable, and a vector of quantiles
#' in order to calculate quantile cutoffs for the measurement, by grouping.
#' 
#' @details 
#' The flags melt and round control rounding of the resulting quantiles and
#' whether to return a long table (suitable for plotting) or a wide table
#' (better for presentation of the table)
#'
#' @param DT A data.table that contains the data
#' @param values A character string that names the column of observations to 
#'      calculate quantiles over
#' @param grouping A character string that names the column containing the groups
#'      to calculate quantiles for
#' @param quants A vector of quantiles defining cuttoffs (e.g. what is the value
#'      in \code{values} that defines the 30th percentile, by each group in 
#'      the column \code{group})
#' @param bMelt A boolean indicating whether to output a long table (TRUE) or wide.
#'      Default behavior is to produce a long table, which is more suitable for plots
#' @param round.digit An integer defining the decimal places to round the results to. Default
#'      behavior is 3
#'
#' @return A long or wide table showing quantile cutoffs for a given measurement
#'      by a group variable.
#'      
#' @import data.table
#' @import stats
#' 
#' @export
#' @examples
#' ## - Example - 
#' ## By US state, what is the 30th and 70th percentile for number of 
#' ## jobs in that geographic area
#' 
#' library(data.table)
#' 
#' # Use data provided in this package
#' #  - change column class to numeric
#' 
#' DT <- copy(fiberCountyDem)
#' DT[, JobsCount := as.numeric(JobsCount)]
#' 
#' values <- "JobsCount"
#' grouping <- "State"
#' quants <- c(.3, .7)
#' 
#' # Set bMelt to false to receive a wide table for display purposes
#' #
#' QuantileCutoffs(DT, values, grouping, quants, bMelt=FALSE)
QuantileCutoffs <- function(DT, values, grouping, quants, bMelt=TRUE, round.digit=3){
    res   <- DT[, (as.list(quantile(get(values), quants))), grouping]
    qlabs <- paste0(values, "_", quants*100)
    setnames(res, c(grouping, qlabs))
    
    # round to digits specified by 'round.digits' (default = 3)
    res[, (qlabs) := lapply(qlabs, function(col) round(get(col), digits=round.digit))]
    
    if(bMelt){
        res <- melt(res, id.vars = grouping, variable.name = "Quantile", value.name = values)
        setkeyv(res, c(grouping, "Quantile"))
    }else{
        setkeyv(res, grouping)
    }
    res[]
}


