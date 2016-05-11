#' A helper function to perform common data.table checks
#' 
#' @description \code{checkdt} is intended for developers who write functions that 
#'      accept a \code{data.table} as an input. \code{checkdt} performs common checks 
#'      and generates accessible and informative errors (or returns error codes if 
#'      desired), if a data.table fails various checks. An optional character vector 
#'      argument \code{cols} enables an additional check to determine if they exist as 
#'      column names of \code{dt}. See details below the checks that are performed, the
#'      error messages, and if applicable, what error codes are returned.
#'  @details If any of the following are true, a friendly error will be shown and the 
#'      code execution will be stopped:
#'          \itemize{
#'              \item{dt is NULL}
#'              \item{dt is not of class data.table (data.frame)}
#'              \item{dt has 0 rows}
#'              \item{if \code{cols} not valid column names of dt (optional check)}
#'          }
#'     The following error codes are generated when each of the above situations 
#'     occur, \strong{and the flag \code{noerror} is set to TRUE:}
#'          \itemize{
#'              \item{if null -> returns 999} test
#'              \item{if class not data.table -> returns 101} test2
#'              \item{if empty -> returns 555} test
#'              \item{if cols not column names -> 422} test
#'          }
#'
#' @param dt An object to make common checks to verify it is a valid data.table
#' @param cols An optional character vector of column names to check against column names of dt
#'
#' @keywords internal
checkdt <- function(dt=NULL, cols=NULL, noerror=FALSE){
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