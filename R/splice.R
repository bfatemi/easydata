#' Splice Data
#'
#' \code{splice} is a function that efficiently splits and subsets a data.table (DT) and
#' returns a list with one or more data.table objects
#'
#' @param DT A data.table to split and/or subset
#' @param ... values or logical operators for a given column(s) that are used to
#'      subset or split the data. See examples below.
#' @param apply An R expression to apply to the data, typically a function to run on
#'      a column, after any splitting and subsetting is performed. See examples below.
#' @return A list with one or more elements that are data.tables resulting from 
#'      DT being split/subsetted
#' @family quick wranglers
#' @example /examples/example-splice.R
#' @export
splice <- function(DT=NULL, ..., apply=NULL){
    # Save call to extract subset conditions. Remove the fn from call obj.
    slice <- sys.call()
    slice <- slice[-1]

    # Define helper function to recursive extract 'AND' or 'OR' values
    RecurseGet <- function(a){
        values <- a[[length(a)]] # extract
        newa <- a[-length(a)]    # remove

        # test after removal and append if ok
        if(length(newa) > 0)
            return(c(values, RecurseGet(a = newa[[-1]])))

        # return final vector of results
        return(values)
    }

    #----------------------------------------------------
    # Extract args: Store the data with a lazy call
    #----------------------------------------------------
    dInd <- which(names(slice) == "DT")

    # data is not optional
    if(!length(dInd))
        stop("Provide data in 'DT' argument to subset", call. = F)

    nDT    <- slice[[dInd]] # double bracks bc we expect only 1 DT
    slice  <- slice[-dInd]

    #----------------------------------------------------
    # Extract args: Get the function call if provided
    #----------------------------------------------------
    
    fInd <- which(names(slice) == "apply")

    # if apply arg is provided, extract and store the call to
    # apply to the result
    APPLY = FALSE
    if(!!length(fInd)){
        APPLY = TRUE
        ply <- slice[fInd]
        slice <- slice[-fInd]
    }

    #-----------------------------------------------------------
    # Extract args: Logical operators are unnamed list elements
    #-----------------------------------------------------------

    # if the user does not provide at least 1 named list element,
    # then names are NULL rather than "". To account for that,
    # convert all null to "". Then get index.
    names(slice)[is.null(names(slice))] <- "NONAME"
    oInd <- which(names(slice) == "NONAME" | names(slice) == "")

    # operators are optional but declare null to prevent error
    # when binding together
    logOp <- c()
    if(!!length(oInd)){
        logOp  <- slice[oInd]

        # if all calls have no names, they are all logical ops
        # and there is no need to continue.
        if(length(oInd) == length(names(slice)))
            return(DT[Reduce("&", lapply(logOp, eval, DT))])

        slice  <- slice[-oInd]
    }

    #-----------------------------------------------------------
    # Extract args: Remaining args are named and need processing
    #-----------------------------------------------------------
    cnames <- sapply(names(slice), as.symbol)

    # check to make sure column names are valid
    ccheck <- cnames[which(!cnames %in% colnames(DT))]
    if(!!length(ccheck)){
        msg <- paste0("Column(s) not in data.table: ",
                      paste(ccheck, collapse = ", "))
        stop(msg, call. = T)
    }

    # Setup empty vectors to store the valid calls after extracting from slice
    OrLogical    <- c()
    OneCondition <- c()
    AndLogical   <- c()

    for(i in 1:length(slice)){
        sCall <- slice[[i]][1] # Get call

        # Check call:
        #       - If OR operator, make valid by collapsing values and modifying call
        #       - If AND operator, each value represents a new data.table. Use mapply
        #         because values are not collapsed as they are done for the "OR" operator
        #       - If character, then sCall is the value being tested for
        #         This is equivalent to the user supplying a double equal sign, but it's
        #         counter-intuitive not to accept this as valid input
        #       - If vector, then this is an OR condition that doesn't require collapsing
        #         values because user provided them as a vector. Again, counter-intuitive
        #         not to accept this scenario
        #
        if(call("|", `|`)[1] == sCall){

            # Recursively extract OR values. More than 1 value by definition
            orVals <- RecurseGet(slice[[i]])
            OrLogical <- c(OrLogical, call("%in%", cnames[[i]], orVals))

        }else if(call("&", `&`)[1] == sCall){

            # Recursively extract AND values. More than 1 value by definition
            andVals <- RecurseGet(slice[[i]])
            AndLogical <- c(AndLogical, mapply(call, "==", cnames[i], andVals, USE.NAMES = F))

            # Set names so when they generate new tables as list elements, lapply will keep name
            # Ensure we are setting names for the tail end of AndLogical that we just created
            # and not write over previous names
            #
            index <- (length(AndLogical) - length(andVals) + 1):length(AndLogical)
            names(AndLogical)[index] <- mapply(paste0, cnames[i], ": ", andVals, USE.NAMES = F)

        }else if(!is.call(sCall)){
            OneCondition <- c(OneCondition, call("==", cnames[[i]], sCall))
        }else if(call("c", `c`)[1] == sCall){
            OrLogical <- c(OrLogical, call("%in%", cnames[[i]], slice[[i]]))
        }else if(call(":", `:`)[1] == sCall){
            OrLogical <- c(OrLogical, call("%in%", cnames[[i]], eval(slice[[i]])))
        }else{
            stop("Unrecognized operator", call. = F)
        }
    }

    # Collapse the conditions to be run for each AND condition
    collapseCond <- c(OrLogical, OneCondition, as.list(logOp))

    # Evaluate data here
    #DT <- eval(nDT)

    # Helper function to subset DT for each AND
    f <- function(and){
        AndBool <- 1 # default to 1 to enable no 'and' condition

        if(!is.null(and))
            AndBool <- eval(and, DT)

        Reduce("&", c(list(AndBool), lapply(collapseCond, eval, DT)))
    }
    AndLogical[is.null(AndLogical)] <- list(NULL)

    if(APPLY){
        res <- lapply(lapply(AndLogical, f), function(i) DT[i][,eval(ply[[1]])])
    }else{
        res <- lapply(lapply(AndLogical, f), function(i) DT[i])
    }
    return(res)
}


