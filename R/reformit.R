#' Construct formula
#' 
#' A helper to build a formula.
#'
#' @param lhs Character vector
#' @param rhs Character vector
#'
#' @export
reformit <- function(lhs, rhs){
    c.lhs <- paste0(lhs, collapse = " + ")
    c.rhs <- paste0(rhs, collapse = " + ")
    stats::as.formula(paste0(c.lhs, " ~ ", c.rhs))
}