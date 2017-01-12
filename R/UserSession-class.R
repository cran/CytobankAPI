#' @import methods httr
NULL

#' S4 Cytobank UserSession Class
#'
#' @description A Cytobank UserSession object that holds pertinent user information, used to make calls to various Cytobank endpoints.
#' This class should never be called explicitly. If a user would like to create a new Cytobank UserSession object, utilize the \link{authenticate} function.
#' @slot auth_token character representing Cytobank user's authentication token (expires in 8 hours)
#' @slot long_timeout numeric representing long request timeout times
#' @slot short_timeout numeric representing short request timeout times
#' @slot site character representing Cytobank user's site
#' @return A Cytobank UserSession object
#' @examples cytobank_user <- new("UserSession", auth_token="my_auth_token", site="premium")
setClass("UserSession",
         representation(auth_token="character",
                        long_timeout="numeric",
                        short_timeout="numeric",
                        site="character"
         ),
         prototype(auth_token=NA_character_,
                   long_timeout=NA_real_,
                   short_timeout=NA_real_,
                   site=NA_character_
         ),
         validity=function(object)
         {
             if (typeof(object@auth_token) != "character" ||
                 typeof(object@long_timeout) != "double" ||
                 typeof(object@short_timeout) != "double" ||
                 typeof(object@site) != "character"
                 )
             {
                 return(FALSE)
             }
             return(TRUE)
         })

