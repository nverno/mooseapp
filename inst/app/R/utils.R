### utils.R --- 
## Filename: utils.R
## Description: 
## Author: Noah Peart
## Created: Tue Oct 20 13:17:29 2015 (-0400)
## Last-Updated: Mon Feb  1 14:47:22 2016 (-0500)
##           By: Noah Peart
######################################################################

##' Store input values, so when partials are rendered values can be restored
##' 
##' @param prefix UI prefixes
##' @param rvals global reactive values
##' @param input global input
##' @importFrom shiny isolate
##' @export
captureInputs <- function(prefix, rvals, input) {
  shiny::isolate({
    inps <- grep(sprintf("^%s", prefix), names(input), value=TRUE)
    for (i in inps) rvals[[i]] <- input[[i]]
  })
}

##' Return a list of partials/controllers from subdirectories
##' 
##' @param ids Names of directories to search in
##' @return list of partials and controllers
##' @export
findParts <- function(ids=c("partials", "controllers")) {
  dirs <- list.dirs(recursive=TRUE)
  inds <- setNames(lapply(ids, function(id) basename(dirs) == id), ids)
  lapply(inds, function(ind)
    sub("^\\.+[/\\]+", "",
      list.files(dirs[ind], full.names=TRUE, no..=TRUE)))
}

##' Find paths to data in specified diretories
##' 
##' @param locs directories to search
##' @param files names of files to look for
##' @param throw_error if specified, throw errow when not found
##' @return list of two character vectors: found files, missed files
##' @export
findData <- function(locs, files, throw_error=TRUE) {
  found <- c()
  for (loc in locs) {
    if (any((present <- file.exists(file.path(loc, files))))) {
      found <- c(found, file.path(loc, files[present]))
      files <- files[!present]
    }
    if (length(files) == 0)
      break
  }
  if (throw_error && length(files))
    stop(paste("\nMissing:", files, collapse=","))
  return( list(found=found, missed=files) )
}

##' Gathers requires/libraries
##'
##' @param dirs Directories to search
##' @param fnames Filenames to search in (optional)
##' @param pattern File extension of files to search
##' @param recursive If true, search recursively
##' @export
findPacks <- function(dirs='.', fnames=NULL, pattern='.*\\.[Rr]+', recursive=TRUE) {
    files <- list.files(dirs, full.names=TRUE, pattern=pattern, recursive=recursive)
    if (!is.null(fnames)) files <-
        unlist(Vectorize(grep, "pattern")(fnames, files, fixed=TRUE, value=TRUE),
               use.names=FALSE)
    patt <- paste(paste0(c("require", "library"), "*?\\(([^)]+)\\).*"), collapse="|")
    reqs <- sapply(files, function(file) {
        lines <- sub("(^[^#]*).*", "\\1", readLines(file))
        lines <- lines[length(lines)>0 & lines != ""]
        unlist(lapply(lines, function(line)
            regmatches(line, gregexpr(patt, line))))
    })
    reqs <- unlist(reqs, use.names = FALSE)
    reqs <- if(length(reqs)>0) unlist(strsplit(reqs, "\\s+|;"))
    res <- (s <- sub(".*\\(([^)]+)\\).*", "\\1", reqs))[s!=""]
    res
}

##' Grep indices ordered by year
##' 
##' @param coln Columns
##' @param yrs Years
##' @param dat data
##' @export
grepInOrder <- function(coln, yrs, dat) {
    unlist(sapply(paste0(coln, yrs), function(x) grep(x, names(dat))))
}

##' Convert polar to cartesian coordinates
## deg: if input theta is in degrees
##' .. content for \details{} ..
##' @param r Distance
##' @param theta Angle (default radians)
##' @param deg If specified, then theta is in degrees
##' @param recycle If specified, recycles \code{r}
##' @return Matrix of x,y coordinates
##' @export
pol2cart <- function(r, theta, deg = FALSE, recycle = FALSE) {
    if (deg) theta <- theta * pi/180
    if (length(r) > 1 && length(r) != length(theta) && !recycle)
        stop("'r' vector different length than theta, if recycling 'r' values is desired 'recycle' must be TRUE")
    xx <- r * cos(theta)
    yy <- r * sin(theta)
    ## Account for machine error in trig functions
    xx[abs(xx) < 2e-15] <- 0
    yy[abs(yy) < 2e-15] <- 0
    out <- cbind(xx, yy)
    colnames(out) <- c("x", "y")
    return( out )
}

##' General if a null then b function
##' 
##' @param a if a is NULL then b
##' @param b b
##' @export
"%||%" <- function(a, b) if (!is.null(a)) a else b

##' Create empty data.frame with all possible names of supplied arguments
##' 
##' @param ... data.frame types we want the names of
##' @return empty data.frame with union of names of passed objects
##' @export
blankDF <- function(...) {
    ns <- Reduce(union, lapply(list(...), names))
    dat <- setNames(as.list(integer(length(ns))), ns)
    as.data.frame(dat)[0,]
}

##' Remove nulls/empty values from list
##'
##' @param lst List
##' @export
nonEmpty <- function(lst) lst[sapply(lst, function(i) !is.null(i) && length(i))]

##' If the underlying variable is numeric, like PPLOT,
##' lets order the factor numerically, otherwise use a standard
##'
##' @param v Variable
##' @return Order of levels
##' @importFrom stringr str_detect
##' @export
levOrder <- function(v) {
    if (any(stringr::str_detect(v, "[[:alpha:]]"), na.rm=TRUE)) {
        sort(unique(na.omit(v)))
    } else sort(unique(as.numeric(as.character(v))))
}

