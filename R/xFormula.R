#' Quickly Create Formula Objects
#' 
#' A function that takes left-hand vars and right-hand vars as character strings
#' and returns and R formula object.
#'
#' @param y Left-hand side of desired formula provided as a character vector or left null
#' @param x Right-hand side of desired formula provided as a character vector or left null
#'
#' @return An object of class formula
#' @export
#'
#' @examples
#' Y <- c("A", "B", "C")
#' X <- c("D", "E", "F")
#' xFormula(Y, X)
xFormula <- function(y = NULL, x = NULL){
    
    env <- rlang::caller_env()
    
    y[is.null(y)] <- "."
    x[is.null(x)] <- "."
    
    y[y == "" | length(y) == 0] <- "."
    x[x == "" | length(x) == 0] <- "."
    
    f <- rlang::new_formula(
        rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(x, collapse = " + "))),
        lhs = lazyeval::as_call(stringr::str_c(stringr::str_c(y, collapse = " + "))),
        env = env
    )
    rlang::f_env(f) <- pryr::parenv()
    return(f)
}
