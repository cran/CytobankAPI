#' News
#'
#' Get news on CytobankAPI updates
#' @name news
NULL


.onAttach <- function(libname, pkgname) {
    CytobankAPI_version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(paste(pkgname, CytobankAPI_version))
    packageStartupMessage("Type CytobankAPI_news() to see new features/changes/bug fixes.")
}

#' @rdname news
#' @aliases CytobankAPI_news
#'
#' @details \code{CytobankAPI_news} View a log of CytobankAPI updates and release notes.
#' @export
CytobankAPI_news <- function() {
    news_file <- file.path(system.file(package="CytobankAPI"), "NEWS")
    file.show(news_file)
}

