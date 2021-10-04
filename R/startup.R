# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

