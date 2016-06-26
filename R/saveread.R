#' Import/Export Compressed Data Quickly
#'
#' Utility functions and wrappers for saveRDS/readRDS (for internal use)
#' @param ... Objects to save as compressed rds file or read from rds files 
#'      into the specified environment (default is parent environment of caller)
#' @param env An environment to read object into (default is parent of caller)
#' @describeIn saver A function to save an r object as compressed file
#' @export
saver <- function(...){
    svobj <- pryr::named_dots(...)
    
    objnames <- names(svobj)
    objects <- tryCatch(
        lapply(svobj, eval),
        error = function(c) stop("error with object in arguments. Check names", call. = FALSE)
    )
    #return(objects)
    # check for valid filenames (no * . ” / \ [ ] : ; | = , )
    
    invalid <- c("[\\*\\.\\/\\[\\]\\:\\;\\|\\=\\,]")
    if(any(stringr::str_detect(objnames, invalid)))
        stop("object does not translate to valid file name", call. = FALSE)
    
    workdir <- getwd()
    subDir <- "DataStoreRDS"
    patsd <- "^DataStoreRDS$" #stringr::str_c("(?!",subDir,").*", subDir,"$")
    
    fpath <- list.files(workdir, pattern = patsd, include.dirs = TRUE, recursive = TRUE)
    
    # add a switch statement and user option
    if(length(fpath) > 1){
        warning(paste0("multiple directories named ", subDir, "...saving to first path: ", fpath[1]))
        fpath <- fpath[1]
    }
    
    if(length(fpath)==0)
        newpath <- file.path(workdir, subDir)
    
    if(length(fpath)==1)
        newpath <- paste0(workdir, "/", fpath)
    
    # create dir
    dir.create(newpath, showWarnings = FALSE)
    
    if(any(sapply(objnames, function(i) i %in% list.files(newpath))))
        stop("one or more objects already exists in: ", newpath, ". overwrite comming soon.")
    
    #tmp set working dir
    setwd(newpath)
    mapply(saveRDS, objects, objnames)
    print(stringr::str_c("objects saved here:", newpath))
    setwd(workdir)
    
    return(1)
}

#' @describeIn saver A function to read rds files and save object in specified environment
#' @export
readr <- function(..., env=NULL){
    if(is.null(env))
        env <- parent.frame()
    
    if(!is.environment(env))
        stop("env not environment")
    
    svobj <- pryr::named_dots(...)
    objnames <- names(svobj)
    
    bExists <- sapply(objnames, function(i) exists(i, inherits = FALSE, envir = env))
    if(any(bExists))
        stop(stringr::str_c("\nObject already exists: ", objnames[which(bExists)]))
    
    
    # objects <- tryCatch(
    #     lapply(svobj, eval),
    #     error = function(c) stop("error with object in arguments. Check names", call. = FALSE)
    # )
    #return(objects)
    # check for valid filenames (no * . ” / \ [ ] : ; | = , )
    
    invalid <- c("[\\*\\.\\/\\[\\]\\:\\;\\|\\=\\,]")
    if(any(stringr::str_detect(objnames, invalid)))
        stop("object does not translate to valid file name", call. = FALSE)
    
    workdir <- getwd()
    subDir <- "DataStoreRDS"
    patsd <- "^DataStoreRDS$" #stringr::str_c("(?!",subDir,").*", subDir,"$")
    
    
    fpath <- list.files(workdir, pattern = patsd, include.dirs = TRUE, recursive = TRUE)
    
    
    
    if(length(fpath) > 1 & length(fpath) > 5)
        stop(stringr::str_c("too many directories of name ", subDir, " found"), call. = FALSE)
    
    # add a switch statement and user option
    if(length(fpath) > 1 & length(fpath) < 5){
        warning(paste0("multiple directories named ", subDir, "...reading from first path: ", fpath[1]))
        newpath <- fpath[1]
    }else{
        newpath <- fpath
    }
    
    if(any(sapply(objnames, function(i) !i %in% list.files(newpath))))
        stop("one or more objects requested does not exist in: ", newpath)
    ll <- lapply(file.path(newpath, objnames), readRDS)
    print(stringr::str_c("Objects read from: ", file.path(workdir, newpath)))
    
    mapply(assign, objnames, ll, MoreArgs = list(envir = env))
    return(1)
}
    
    