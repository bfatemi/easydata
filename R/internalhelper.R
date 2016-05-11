#############################################################################
# Script Name:  morphDevHelp.R
# Description:  Contains helper functions to assist with debugging lower 
#               levels not exposed to community (e.g. connection environment, 
#               fn call stack, etc.)
# Author:       Bobby Fatemi
#############################################################################

DebugInfo <- function(msg=NULL, nFrame=NULL){
    bordLen <- 60
    border  <- quote(cat(paste(c(rep("-", bordLen), "\n"), collapse = "")))
    
    if(is.null(nFrame))
        nFrame <- sys.nframe()
    if(is.null(msg))
        msg="Generic Error Message"
    
    if(!exists("cn.env", mode = "environment")) {
        connEnvironment <- "None"
        openConns       <- "No Open Connections"
    }else{
        connEnvironment <- capture.output(str(cn.env))
        openConns       <- ls(cn.env)
        if(!length(ls(cn.env)))
            openConns   <- "No Open Connections"
    }
    
    #ll.debug <- list(
    ErrorInfo = list(Message     = msg$message,
                     OccurredIn  = gsub("\\(.*\\)", "", sys.calls()[nFrame]),
                     TraceBack   = .CallStack(nFrame),
                     Environment = capture.output(str(sys.frame(nFrame)))) #,
    Connection = list(OpenConns  = openConns,
                      ConnEnvir  = connEnvironment,
                      Info       = "Run ConnStatus() for details") #)
    
    # Print info with title centered on border length
    title <- "Debug Information"
    spaces <- paste(rep(" ", floor((bordLen - nchar(title))/ 2)), collapse="")
    cat(paste0(spaces, title, spaces, "\n"))
    #.PrintInfo(ll.debug, "Debug", "")
    .PrintInfo(ErrorInfo, "Debug", "Error Information")
    .PrintInfo(Connection, "Debug", "Connection Environment")
    stop("See error information above",call. = F)
}

timetaken <- function(time){
    return(as.numeric(Sys.time() - time))
}

Timer <- function(START=TRUE){
    if(START){
        tstamp <<- Sys.time()
    }else{
        dur <- difftime(Sys.time(), tstamp, units = "sec")
        print(paste0("Completed duration (sec): ", round(dur, 2)))
        cat("\n")
        return(dur)
    }
}

# Helper fn returns call stack for conn obj attribute 
.CallStack <- function(nFrame){
    calls <- as.character(sys.calls()[1:nFrame])
    calls <- gsubfn(replacement = "", x = calls, 
                    pattern = "\\(.*\\)")
    
    #calls <- c(calls, "blahblah")
    if(length(calls)>1){
        # eliminate all calls past the first "try" call
        firstTryCall <- grep("try", calls)[1]
        if(!is.na(firstTryCall))
            calls <- calls[-(firstTryCall:length(calls))]    
    }
    paste(calls, collapse = " => ")
}

# INTERNAL
.Caller <- function(nFrame=NULL){
    if(is.null(nFrame))
        nFrame <- sys.nframe()-2
    caller <- as.character(sys.calls()[nFrame])
    caller <- gsubfn(replacement = "", x = caller, 
                     pattern = "\\(.*\\)")
    paste(caller, collapse = " => ")
}

.PrintInfo <- function(ll, label, title){
    bordLen <- 60
    border  <- quote(cat(paste(c(rep("-", bordLen), "\n"), collapse = "")))
    
    eval(border)
    cat(title,"\n")
    str(object = ll, 
        indent.str = label, 
        comp.str = " ",
        no.list = TRUE,
        give.length = FALSE, 
        give.attr = FALSE,
        give.head = FALSE)
    eval(border)
}

