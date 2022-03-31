# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @import httr methods
NULL

#' S4 Cytobank UserSession Class
#'
#' @description A Cytobank UserSession object that holds pertinent user information, used to make calls to various Cytobank endpoints.
#' This class should never be called explicitly. If a user would like to create a new Cytobank UserSession object, utilize the \link{authenticate} function.
#' @slot auth_token character representing Cytobank user's authentication token (expires in 8 hours)
#' @slot long_timeout numeric representing long request timeout times
#' @slot short_timeout numeric representing short request timeout times
#' @slot site character representing Cytobank user's site
#' @slot user_id integer representing a Cytobank user's ID
#' @return A Cytobank UserSession object
#' @examples cytobank_user <- new("UserSession", auth_token="my_auth_token", site="premium")
setClass("UserSession",
         representation(auth_token="character",
                        long_timeout="numeric",
                        short_timeout="numeric",
                        site="character",
                        user_id='integer'
         ),
         prototype(auth_token=NA_character_,
                   long_timeout=NA_real_,
                   short_timeout=NA_real_,
                   site=NA_character_,
                   user_id=NA_integer_
         ),
         validity=function(object)
         {
             if (typeof(object@auth_token) != "character" ||
                 typeof(object@long_timeout) != "double" ||
                 typeof(object@short_timeout) != "double" ||
                 typeof(object@site) != "character" ||
                 typeof(object@user_id) != 'integer'
                 )
             {
                 return(FALSE)
             }
             return(TRUE)
         })

