# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Authentication Endpoints
#'
#' Interact with authentication endpoints. Every call to the Cytobank API
#' must be accompanied by an authentication token. Tokens should be kept secure
#' as they confer access to the data and analyses of an account. Tokens expire
#' after 8 hours by default but this figure my change depending on custom
#' configurations of an Enterprise Cytobank.
#'
#' @name authentication
#' @param auth_token character representing Cytobank user's authentication token
#'   (expires in 8 hours)
#' @param long_timeout numeric representing long request timeout times (default
#'   = 60s) \strong{[optional]}
#' @param password character representing Cytobank user's password
#' @param short_timeout numeric representing short request timeout times
#'   (default = 30s) \strong{[optional]}
#' @param site character representing Cytobank user's site, as in 'site'.cytobank.org. If your Cytobank server does not end in '.org', enter the entire server name, as in 'site.cytobank.cn'.
#' @param timeout integer representing the request timeout time in seconds
#'   \strong{[optional]}
#' @param user_id integer representing Cytobank user's ID
#' @param username character representing Cytobank user's username or email
#' @param UserSession Cytobank UserSession object

#' @rdname authentication
#'
#' @details \code{authenticate} Authenticate a Cytobank user and return a
#'   Cytobank UserSession object that is passed to all other Cytobank API
#'   endpoints.
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry",
#' password="cytobank_rocks!") # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
#' @export
authenticate <- function(site, username=NA, password=NA, auth_token=NA, short_timeout=30, long_timeout=60, timeout=30)
{
    site_fullname = ifelse(grepl('\\.cytobank.\\w', site),
                  site,
                  paste0(site, '.cytobank.org'))

    if (!is.na(auth_token))
    {
        user_id <- c(jsonlite::fromJSON(rawToChar(jose::base64url_decode(unlist(strsplit(auth_token,".",fixed = TRUE))[2]))))$user_id

        return(new("UserSession", auth_token=auth_token, site=paste("https://", site_fullname, "/cytobank/api/v1", sep=""),
                   short_timeout=short_timeout, long_timeout=long_timeout, user_id=as.integer(user_id)))
    }

    # Authenticate user
    resp <- POST(paste("https://", site_fullname, "/cytobank/api/v1/authenticate", sep=""),
                 body=list(username=username, password=password),
                 encode="json",
                 timeout(timeout)
    )

    parsed <- parse(resp, "authentication")

    return(new("UserSession", auth_token=parsed$user$authToken, site=paste("https://", site_fullname, "/cytobank/api/v1", sep=""),
               short_timeout=short_timeout, long_timeout=long_timeout, user_id=as.integer(parsed$user$id)))
}


setGeneric("authentication.logout", function(UserSession, timeout=UserSession@short_timeout)
{
    standardGeneric("authentication.logout")
})
#' @rdname authentication
#' @aliases authentication.logout
#'
#' @details \code{authentication.logout} This function has been deprecated. Logout a Cytobank user.

#' @export
setMethod("authentication.logout", signature(UserSession="UserSession"), function(UserSession, timeout=UserSession@short_timeout)
{

    .Deprecated(msg = 'This function has been removed in the latest release of the Cytobank API. The current Cytobank version can automatically close API connections.')
    # Comment out the following code to remove the logout function.  12/1/2020
    if(0){
        resp <- POST(paste(UserSession@site, "/logout", sep=""),
                     add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                     timeout(timeout)
        )

        return(parse(resp, "authentication"))
    }

})


setGeneric("authentication.revoke_all_tokens", function(UserSession, timeout=UserSession@short_timeout)
{
    standardGeneric("authentication.revoke_all_tokens")
})
#' @rdname authentication
#' @aliases authentication.revoke_all_tokens
#'
#' @details \code{authentication.revoke_all_tokens} This function has been deprecated. Invalidate all existing tokens for the user making this call.

#' @export
setMethod("authentication.revoke_all_tokens", signature(UserSession="UserSession"), function(UserSession, timeout=UserSession@short_timeout)
{
    .Deprecated(msg = 'This function has been removed in the latest release of the Cytobank API. The current Cytobank version can automatically close API connections.')
    # Comment out the following code to remove the logout function.  12/1/2020
    if(0){
    resp <- POST(paste(UserSession@site, "/revoke_tokens", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    return(parse(resp, "authentication"))
    }
})


setGeneric("authentication.revoke_all_tokens_user", function(UserSession, user_id, timeout=UserSession@short_timeout)
{
    standardGeneric("authentication.revoke_all_tokens_user")
})
#' @rdname authentication
#' @aliases authentication.revoke_all_tokens_user
#'
#' @details \code{authentication.revoke_all_tokens_user} This function has been deprecated. Revoke all tokens for a given user. This endpoint only works for admins of the Cytobank site being accessed.

#' @export
setMethod("authentication.revoke_all_tokens_user", signature(UserSession="UserSession"), function(UserSession, user_id, timeout=UserSession@short_timeout)
{
    .Deprecated(msg = 'This function has been removed in the latest release of the Cytobank API. The current Cytobank version can automatically close API connections.')
    # Comment out the following code to remove the logout function.  12/1/2020
    if(0){
    resp <- POST(paste(UserSession@site, "/revoke_user_tokens", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(user_id=user_id),
                 encode="json",
                 timeout(timeout)
    )

    return(parse(resp, "authentication"))
    }
})

