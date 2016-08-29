#' Functions to Change Column Classes
#'
#' The functions \code{ClassMorph}, and \code{NumMorph}, in addition to \code{DateMorph},
#' are functions designed to make dealing with column classes easy. See examples for how
#' to avoid uncessary frustrations related to unexpected column classes or date formats.
#' 
#' This function takes care of multiple conversion steps that are sometimes needed to avoid 
#' data loss or unexpected results (e.g. a factor that "looks" like the number "1" may 
#' not convert to a \code{numeric} "1". To get around this common frustration, this function
#' will perform an indirect conversion to \code{character} before converting to \code{numeric})
#' 
#' @param dt A data.table to use for any "class" operations
#' @param old ClassMorph only. A column type, specified as a character value, to detect
#'      and convert to the class specified by 'new'
#' @param new ClassMorph only. A column type, specified as a character value, to 
#'      convert all columns of the class specified by 'old' to. 
#' @param cols An optional character vector of column names to operate on.
#'      Not applicable for function ClassMorph.
#' @param copy A boolean value indicating whether to alter \code{dt} in memory or 
#'      whether to return a new (copy) of the input data.table.
#' @param force A boolean indicating whether to force conversion from class factor
#'      to class numeric despite NAs being generated.
#' @return Returns a data.table that is either a copy of the input dt that 
#'      has been modified, or the same input dt that has been modified in memory
#' @examples
#' #-------------------------------------------------------------------------------------
#' # Example: Change column classes in batch
#' #-------------------------------------------------------------------------------------
#' library(data.table)
#' dt <- data.table(A = as.factor(c(1:2, "-", 4:5)),
#'                  B = rep(2.0, 5),
#'                  C = as.factor(c(1, "garbage", 2, "", 3)),
#'                  D = as.factor(rep(999, 5)),
#'                  E = letters[1:5],
#'                  F = rep("2020-01-22", 5))
#' 
#' ClassMorph(dt, "factor", "integer", force = TRUE) # no error
#' pclass(dt)
#' 
#' @export
#' @describeIn ClassMorph A function to convert all columns of class "old" to class "new". This
#' function handles the indirect conversions sometimes needed to avoid data loss or
#' unexpected results (see details).
#' 
#' @import data.table
ClassMorph <- function(dt,
                       old   = c("factor","integer","character","numeric","logical"),
                       new   = c("factor","integer","character","numeric"),
                       copy  = FALSE,
                       force = FALSE){
    
    if(!data.table::is.data.table(dt))
        stop("Data should be class data.table. Use 'as.data.table'", call. = FALSE)
    
    
    # NEED TO IMPLEMENT force
    # trace how NAs are/could be generated and return error. recommend to 
    # rerun with force=TRUE
    # ERROR WITH FACTOR TO INTEGER. NO CONVERSION TO CHARACTER FIRST
    if(!old %in% sapply(dt, class)){
        warning("No columns of class specified with the argument 'old'", call. = FALSE)
        return(NULL)
    }
        
    old <- match.arg(old) # Capture args
    new <- match.arg(new)
    
    checkdt(dt) # standard checks
    colorder <- copy(colnames(dt)) # Save original column order. Set at end
    
    cdt <- dt
    if(copy) 
        cdt <- copy(dt)
    
    # Get all pairs and id those that require indirect conversion
    # E.g.  direct:   factor -> numeric
    #       indirect: factor -> character -> numeric
    #
    types <- c("factor","integer","character","numeric","logical")
    
    dtTypes <- CJ(From = types, To = types)
    setkey(dtTypes, From, To)
    
    dtTypes[.("factor", "numeric"), CharFirst := TRUE]
    dtTypes[.("factor", "integer"), CharFirst := TRUE]
    dtTypes[is.na(CharFirst), CharFirst := FALSE]
    
    
    # If "from/to" pair requires indirect conversion, then wrap captured
    # column in a call to "as.character"
    #
    
    colK <- quote(cdt[[k]])
    if( dtTypes[.(old, new), CharFirst] )
        colK <- call("as.character", colK)
    
    # Get position of cols to convert
    cols <- which(sapply(cdt, class) == old)
    
    
    # For each col, evaluate call & set new
    if(new == "character"){
        navector <- rep(NA_character_, nrow(cdt))
    }else if(new == "numeric"){
        navector <- rep(NA_real_, nrow(cdt))
    }else if(new == "integer"){
        navector <- rep(NA_integer_, nrow(cdt))
    }else{
        navector <- rep(NA, nrow(cdt))
    }
        
    
    # Set columns to fill
    cdt[, (paste0("new_", names(cols))) := (navector)]
    
    for (k in names(cols)){
        
        # Removed functionality:
        # Get the index of non-null elements (nulls get collapsed by default
        # but we want the number of replacement elements to match the number
        # of rows in the cdt we are replacing in)
        
        val <- tryCatch(eval(call(paste0("as.", new), colK)),
                        warning = function(c){
                            if(!force){
                                cdt[, (paste0("new_", names(cols))) := NULL] # reset data table
                                stop("Conversion generated NAs. If expected set force=TRUE", call. = FALSE)
                            }
                            return(suppressWarnings(eval(call(paste0("as.", new), colK))))
                        })
        
        newk <- paste0("new_", k)
        set(cdt, j = newk, value = val)
    }
    
    # now that we have new cols with correct classes, 
    # delete old ones and change new names to previous names
    cdt[, (names(cols)) := NULL]
    setnames(cdt, paste0("new_", names(cols)), names(cols))
    
    # reset column orders
    setcolorder(cdt, colorder)
    
    # ensure it was completed
    # if(old %in% sapply(cdt, class))
    #     stop("oops... something went wrong in ClassMorph")
    
    return(cdt)
}

#' @describeIn ClassMorph A function to standards classes to "numeric" for all columns of
#'      dt, where a conversion to numeric would not generate NA values. 
#' 
#' @param verbose A boolean indicating whether to be chatty
#' @export
#' 
#' @import data.table
NumMorph <- function(dt, cols=NULL, copy=FALSE, verbose=FALSE){
    checkdt(dt, cols)

    if(copy) dt <- copy(dt)
    
    if(is.null(cols)) cols <- names(dt)
    
    # recursive function to catch warning if value generated error, and skip the column
    f <- function(i){
        if(!is.na(cols[i])){
            tryCatch({
                set(dt, j=cols[i], value = as.numeric(dt[, get(cols[i])]))
                f(i + 1)
            }, warning = function(e){
                if(verbose)
                    warning(paste0("Conversion generated NA. Skipping column: ", cols[i]), call. = F)
                f(i + 1)
            })
        }
    }
    f(1)
    return(dt)
}

#' @describeIn ClassMorph A wrapper to sapply that prints the class of each column on the R console 
#'
#' @export
pclass <- function(dt){
    print(sapply(dt, class))
}

#' @describeIn ClassMorph Similar to \code{pclass} except this function returns an object, 
#'      instead of simply printing to the console
#' @export
cc <- function(dt){
    sapply(dt, class)
}
