#' Functions to help with pre-data exploration
#' 
#' Helper functions to assist with basic data exploration tasks.
#' 
#' @param DT A data.table  
#' @param all Boolean indicating whether to perform for all columns (default is FALSE). If 
#'      this is false and optional argument "cols" was also not provided, the default behavior 
#'      is to perform this for only factor and character column classes in DT
#' @param cols optional arg providing column names to operate on
#' @param cclass Not implemented. Useful if desiring to describe all columns a particular class
#' @param FUN NI; provide function to apply over cols (if provided)
#'
#' @return A descriptive data.table
#' @export
#'
#' @examples
#' easy_describe(as.data.table(iris))
easy_describe <- function(DT, cols=NULL, cclass = NULL, FUN = NULL, all=NULL){
    
    if(!is.data.table(DT)) stop("DT should be data.table class")
    
    ccdt <- pcc(DT, bret = TRUE) # start the descriptive table
    ## count of unique values
    ## count of NAs per columns
    ##
    cols <- ccdt[, CName]   # get colnames of DT to describe
    
    ccdt[, count_nonNA   := sapply(cols, function(i) sum(DT[, !is.na(get(i))]))]
    ccdt[, count_NA     := nrow(DT) - count_nonNA]
    ccdt[, count_unique := sapply(cols, function(i) length(unique(DT[, get(i)])))]
    ccdt[, pct_true := NA_real_]
    
    ## date range of date class columns
    ##
    dcols <- ccdt[Class == "Date" | Class == "POSIXct_POSIXt", CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols)
            ccdt[CName == d, 
                 range_values := paste0("(", min(DT[!is.na(get(d)), get(d)]), "):(", max(DT[!is.na(get(d)), get(d)]), ")")]
    }
    
    dcols <- ccdt[Class %in% c("integer", "numeric"), CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols)
            ccdt[CName == d, 
                 range_values := paste0("(", min(DT[!is.na(get(d)), get(d)]), "):(", max(DT[!is.na(get(d)), get(d)]), ")")]
    }
    
    dcols <- ccdt[Class == "logical", CName]   # get date columns if any
    if(length(dcols) > 0){
        for(d in dcols){
            ccdt[CName == d, range_values := "FALSE : TRUE"]
            ccdt[CName == d, pct_true := DT[!is.na(get(d)), round(sum(get(d))/count_nonNA, 5)]]
        }
    }
    
    ccdt[count_NA > 0, pct_NA := round(count_NA/nrow(DT), 5)]
    
    setnames(ccdt, c("Pos", "CName", "Class"), c("col_position", "col_name", "col_class"))
    setcolorder(ccdt, c("col_position", "col_name", "col_class", "count_unique", 
                        "count_NA", "count_nonNA", "range_values", 
                        "pct_true", "pct_NA"))
    setorderv(ccdt, c("col_class", "count_unique", "col_position"))
    return(ccdt[])
    
}

globalVariables(c("count_unique", "count_NA", "count_nonNA", "CName", "Class", "range_values", "pct_true", "pct_NA"))



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

