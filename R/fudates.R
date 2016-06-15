#' Identify Dates with Patterns
#'
#' The function \code{fudate} returns the regex pattern that identifies any date using most common date formats. 
#' 
#' @details 
#' A universal regex pattern to 
#' identify any date with certainty is very difficult to do. This regex uses built in logic to rule out 
#' false positives despite having the correct format. To ensure accuracy, this pattern does not attempt
#' to identify the position of the year, month, or day groups. 
#' 
#' The argument \code{form} is a character value of 'ymd', 'ydm', 'mdy', 'dmy' which determines the returned
#' regex pattern:
#' 
#'  \itemize{
#'      \item{\code{ymd} identifies dates where the year is first, month second, followed by day.}
#'      \item{The other options are: \code{ydm}, \code{mdy}, or \code{dmy}}
#'      \item{\code{yr4} is a boolean specifying whether to expect a 4 digit or 2 digit year. 
#'      This is done to ensure the highest possible accuracy in the match}
#'  }
#'  
#' @section Additional Information:
#' For details of the logic see the vignette LINK HERE. This vignette also serves as an basic 
#' intro to regular expressions using dates for an example case study
#' 
#' @param form A character value of 'ymd', 'ydm', 'mdy', 'dmy'. See details below for addition info
#' @param yr4 A boolean indicating whether to expect a 4 digit or 2 digit date
#'
#' @export
#' 
fudate <- function(form = c("ymd", "ydm", "mdy", "dmy"), yr4 = TRUE){
    form <- match.arg(form, choices = form, several.ok = FALSE)
    
    return(form)
    yy   <- "(?<yy>(0[1-9])|(1[0-6]))"  # can be 2 or 4 digits
    yyyy <- "(?<yyyy>19|20(?<=\\d\\d)(?:0[1-9]|1[0-6]))"  # can be 2 or 4 digits
    dd   <- "(?<dd>[0-2][0-9]|3[0-1])"  # needs 2 digits
    mm   <- "(?<mm>0[1-9]|1[0-2])"      # needs 2 digits
    sep1 <- "(?<sep>[-\\/]{0,1})??"     # can be one of a few
    sep2 <- "(\\k<sep>)"                # consistent seperator enforced here
    
    if(yr4){
        switch(form,
               ymd = paste0(yyyy, sep1, mm, sep2, dd),
               ydm = paste0(yyyy, sep1, dd, sep2, mm),
               mdy = paste0(mm, sep1, dd, sep2, yyyy),
               dmy = paste0(dd, sep1, mm, sep2, yyyy))
    }else{
        switch(form,
               ymd = paste0(yy, sep1, mm, sep2, dd),
               ydm = paste0(yy, sep1, dd, sep2, mm),
               mdy = paste0(mm, sep1, dd, sep2, yy),
               dmy = paste0(dd, sep1, mm, sep2, yy))
    }
}

#' @describeIn fudate A function that utilizes \code{fudate} to extract elements that are 
#' identified as dates
#' @param x A vector, column, or text which contains dates to extract
#' @param ... NOT IMPLEMENTED
#' @export
extract_date <- function(x, form, yr4, ...){
    argg <- c(as.list(environment()), list(...))[-1]
    
    if(argg$form == "")
        warning("argument 'form' missing. Using default of 'ymd'. see details for more information", call. = FALSE)
    
    pat <- pryr::do_call(fudate, argg)
    
    return(x[grep(pat, x, perl = TRUE)])
}





# 
# allDates <- function(){
#     f <- function(x,y) Reduce(stringr::str_c, CJ(x, y))
#     
#     yy <- c(60:99, stringr::str_c(0, 0:9), stringr::str_c(1,0:9))
#     yyyy <- stringr::str_c(1960:2019)
#     d <- stringr::str_c(1:31)
#     dd <- c(stringr::str_c(0, 1:9), 10:31)
#     m <- stringr::str_c(1:12)
#     mm <- c(stringr::str_c(0, 1:9), 10:12)
#     seps <- c("-", "/")
#     e_all <- substitute(lapply(year, stringr::str_c, seps,
#                                sapply(month, stringr::str_c, sapply(day, function(i) 
#                                    mapply(f, seps, i)))))
#     
#     all <- unlist(eval(e_all, list(year = c(yy, yyyy), month = c(m, mm), day = c(d, dd))))
#     return(all)
# }
# allDates()
# f <- function(lotN, seqN){
#     # ln <- as.character(lotN)
#     # sn <- as.character(seqN)
#     # 
#     # # sn needs to be 4 digits so append 0s until we have 4 
#     # newSn <- Reduce(str_c, c(sn, rep(0, 4 - str_count(sn))))
#     # 
#     # # # regex session: https://regex101.com/r/vY7jB1/1
#     # # yy <- "(20)?(1[1-6])"
#     # # dd <- "([0-2][1-9]|3[0-1])"
#     # # mm <- "(0[1-9]|1[0-2])"
#     # 
#     # # create a pattern with logic that will find yymmdd
#     # # yymmdd <- "(1[1-6])(0[1-9]|1[0-2])([0-2][1-9]|3[0-1])"
#     # dpat <- "(?<yy>1[1-6])(?<mm>0[1-9]|1[0-2])(?<dd>[0-2][1-9]|3[0-1])"
#     # 
#     # return(str_c(str_extract(ln, dpat), newSn))
#     # 
#     # # y1 <- list(yy = 6:9, yy = 0:1, yyyy = 1,   yyyy = 2)
#     # # y2 <- list(yy = 0:9, yy = 0:9, yyyy = 9,   yyyy = 0)
#     # # y3 <- list(yy = "",  yy = "",  yyyy = 6:9, yyyy = 0:1)
#     # # y4 <- list(yy = "",  yy = "",  yyyy = 0:9, yyyy = 0:9)
#     # # 
#     # # m1 <- list(mm = 0,   mm = 1,   m = 1:9)
#     # # m2 <- list(mm = 1:9, mm = 0:2, m = "")
#     # # 
#     # # d1 <- list(a = 0,   b = 1,   c = 2,   d = 3,   e = 1:9)
#     # # d2 <- list(a = 1:9, b = 0:9, c = 0:9, d = 0:1, e = )
#     # # 
#     f <- function(x,y) Reduce(str_c, CJ(x, y))
#     
#     yy <- c(60:99, str_c(0, 0:9), str_c(1,0:9))
#     yyyy <- str_c(1960:2019)
#     d <- str_c(1:31)
#     dd <- c(str_c(0, 1:9), 10:31)
#     m <- str_c(1:12)
#     mm <- c(str_c(0, 1:9), 10:12)
#     
#     e_all <- substitute(lapply(year, str_c, seps,
#                                sapply(month, str_c, sapply(day, function(i) 
#                                    mapply(f, seps, i)))))
#     
#     all <- eval(e_all, list(year = c(yy, yyyy), month = c(m, mm), day = c(d, dd)))
# }