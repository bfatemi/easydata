#' Functions to help with pre-data exploration
#' 
#' Helper functions to assist with basic data exploration tasks.
#' 
#' @param DT A data.table  
# @param all Boolean indicating whether to perform for all columns (default is FALSE). If
#      this is false and optional argument "cols" was also not provided, the default behavior
#      is to perform this for only factor and character column classes in DT
#' @param cols optional arg providing column names to operate on
#' @param cclass Not implemented. Useful if desiring to describe all columns of a particular class
#' @param FUN NI; provide function to apply over cols (if provided)
#'
#' @return A descriptive data.table
#' 
#' @importFrom lubridate
#' @export
#'
#' @examples
#' library(data.table)
#' library(easydata)
#' DT <- as.data.table(iris)
#' 
#' easy_describe(DT)
easy_describe <- function(DT, cols=NULL, cclass = NULL, FUN = NULL, all=NULL){
    
    if(!is.data.table(DT)) stop("DT should be data.table class")
    
    
    # cols <- ccdt[, CName]        # get colnames of DT to describe
    # ccdt[, count_nonNA  := sapply(cols, function(i) sum(DT[, !is.na(get(i))]))]
    # ccdt[, count_NA     := nrow(DT) - count_nonNA ]
    
    cc <- pcc(DT, bret = TRUE) # start the descriptive table
    cc[, pct_NA   := vapply(CName, function(i) DT[is.na(get(i)), .N / DT[, .N]], FUN.VALUE = numeric(1)) ]
    cc[, pct_Uniq := vapply(CName, function(i) length(unique(DT[, get(i)])) / DT[, .N], FUN.VALUE = numeric(1))]
    cc[, pct_true := NA_real_]
    
    ##
    ## date range of date class columns
    ##
    isDate <- function(i) is.Date(i) | is.POSIXct(i)
    isNum  <- function(i) is.double(i) | is.numeric(i) | is.integer(i)
    isBool <- function(i) is.logical(i)
    
    index  <- which(as.logical(sapply(DT, function(i) isDate(i) | isNum(i))))
    cnames <- cc[index, CName]
    
    range(c(TRUE, TRUE, TRUE, FALSE))
    f <- function(mn, mx) paste0("(", mn, "):(", mx, ")")
    for(col in cnames)
        cc[CName == col, range_vals := do.call(f, as.list(range(DT[, get(col)], na.rm = TRUE)))]
    
    dcols <- cc[Class == "logical", CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols){
            cc[CName == d, range_values := "FALSE : TRUE"]
            cc[CName == d, pct_true := DT[!is.na(get(d)), round(sum(get(d))/count_nonNA, 5)]]
        }
    }
    
    cc[count_NA > 0, pct_NA := round(count_NA/nrow(DT), 5)]
    
    setnames(cc, c("Pos", "CName", "Class"), c("col_position", "col_name", "col_class"))
    setcolorder(cc, c("col_position", "col_name", "col_class", "count_unique", 
                        "count_NA", "count_nonNA", "range_values", 
                        "pct_true", "pct_NA"))
    setorderv(cc, c("col_class", "count_unique", "col_position"))
    return(cc[])
    
}

#' @describeIn easy_describe A function to split the column names of a data.table into groups specified by class
#' @export
split_by_class <- function(DT){
    cdt <- pcc(DT, bret = TRUE)
    return(lapply(split(cdt, as.factor(cdt$Class)), function(i) i$CName))
}

#' @describeIn easy_describe Which columns contain all NA values down the rows (all obervations are NA)
#' @export
wcolNA <- function(DT){
    which(apply(DT[, lapply(.SD, is.na)], 2, sum)==nrow(DT))
}

#' @describeIn easy_describe Which rows contain all NA values across the columns
#' @export
wrowNA <- function(DT){
    which(apply(DT[, lapply(.SD, is.na)], 1, sum)==ncol(DT))
}






# make_factor <- function(num, DT, descDT){
#     DT <- copy(MDT)
#     descDT <- copy(desc_MDT)
#     cnames <- descDT[count_unique < 100 & 
#                          count_NA/count_nonNA < .2 & 
#                          col_class %in% c("character", "integer", "numeric", "factor"), 
#                      col_name]
#     
#     for(cn in cnames)
#         set(DT, j = cn, value = as.factor(DT[, cn, with=FALSE]))
#     return(DT[])
# }



# easy_describe <- function(DT, cols=NULL, cclass = NULL, FUN = NULL, all=NULL){
#     
#     if(!is.data.table(DT)) stop("DT should be data.table class")
#     
#     ccdt <- pcc(DT, bret = TRUE) # start the descriptive table
#     
#     ## count of unique values
#     ## count of NAs per columns
#     ##
#     cols <- ccdt[, CName]   # get colnames of DT to describe
#     
#     ccdt[, count_nonNA   := sapply(cols, function(i) sum(DT[, !is.na(get(i))]))]
#     ccdt[, count_NA     := nrow(DT) - count_nonNA]
#     ccdt[, count_unique := sapply(cols, function(i) length(unique(DT[, get(i)])))]
#     ccdt[, pct_true := NA_real_]
#     
#     ## date range of date class columns
#     ##
#     dcols <- ccdt[Class == "Date", CName]   # get date columns if any
#     if(length(dcols) > 0){
#         for(d in dcols)
#             ccdt[CName == d, 
#                  range_values := paste0("(", min(DT[!is.na(get(d)), get(d)]), "):(", max(DT[!is.na(get(d)), get(d)]), ")")]
#     }
#     
#     dcols <- ccdt[Class %in% c("integer", "numeric"), CName]   # get date columns if any
#     if(length(dcols) > 0){
#         for(d in dcols)
#             ccdt[CName == d, 
#                  range_values := paste0("(", min(DT[!is.na(get(d)), get(d)]), "):(", max(DT[!is.na(get(d)), get(d)]), ")")]
#     }
#     
#     dcols <- ccdt[Class == "logical", CName]   # get date columns if any
#     if(length(dcols) > 0){
#         for(d in dcols){
#             ccdt[CName == d, range_values := "(FALSE):(TRUE)"]
#             ccdt[CName == d, pct_true := DT[!is.na(get(d)), round(sum(get(d))/count_nonNA, 3)]]
#         }
#     }
#     setnames(ccdt, c("Pos", "CName", "Class"), c("col_position", "col_name", "col_class"))
#     setcolorder(ccdt, c("col_position", "col_name", "col_class", "count_unique", 
#                         "count_NA", "count_nonNA", "range_values", "pct_true"))
#     setorderv(ccdt, c("col_class", "count_unique", "col_position"))
#     
#     easydata::pcc(ccdt)
#     return(ccdt[])
#     
# }

