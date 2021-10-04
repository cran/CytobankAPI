# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' User Endpoints
#'
#' Interact with user endpoints. One should never analyze alone...
#' @name users
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- users.list, users.show : \code{("default", "raw")}}
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param user_id integer representing a user ID
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("users.list", function(UserSession, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("users.list")
})
#' @rdname users
#' @aliases users.list
#'
#' @details \code{users.list} List all users from an experiment. Outputs a dataframe [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all users with all fields present
#' users.list(cyto_session)
#'
#' # Raw list of all useres with all fields present
#' users.list(cyto_session, output="raw")
#' }
#' @export
setMethod("users.list", signature(UserSession="UserSession"), function(UserSession, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "users", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/users", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "users")[[1]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "users"))
    }
})


setGeneric("users.show", function(UserSession, user_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("users.show")
})
#' @rdname users
#' @aliases users.show
#'
#' @details \code{users.show} Show user details from an experiment.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{users.show(cyto_session, user_id=2)
#' }
#' @export
setMethod("users.show", signature(UserSession="UserSession"), function(UserSession, user_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "users", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/users/", user_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "users")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "users"))
    }
})

