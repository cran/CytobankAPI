# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Scale Endpoints
#'
#' Interact with scale endpoints. Data are rarely presented exactly as they were acquired on the instrument. \href{https://support.cytobank.org/hc/en-us/categories/200260637-Data-Scaling-and-Transformation}{Learn more about data scaling}.
#' @name scales
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- scales.list, scales.show, scales.update : \code{("default", "raw")}}
#' @param scale dataframe representing a scale
#' @param scale_id integer representing a scale ID
#' @param timeout integer representing the request timeout time in seconds
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("scales.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("scales.list")
})
#' @rdname scales
#' @aliases scales.list
#'
#' @details \code{scales.list} List all scales from an experiment. Outputs a dataframe [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all scales with all fields present
#' scales.list(cyto_session, 22)
#'
#' # Raw list of all scales with all fields present
#' scales.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("scales.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "scales", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/scales", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "scales")[[1]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "scales"))
    }
})


setGeneric("scales.show", function(UserSession, experiment_id, scale_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("scales.show")
})
#' @rdname scales
#' @aliases scales.show
#'
#' @details \code{scales.show} Show scale details from an experiment.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{scales.show(cyto_session, 22, scale_id=2)
#' }
#' @export
setMethod("scales.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, scale_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "scales", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/scales/", scale_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "scales")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "scales"))
    }
})


setGeneric("scales.update", function(UserSession, scale, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("scales.update")
})
#' @rdname scales
#' @aliases scales.update
#'
#' @details \code{scales.update} Update a single scale from an experiment.
#' (all parameters are optional, except for experiment_id and scale_id)\cr
#' \emph{- Scale Types -- }\code{1: Linear, 2: Log, 4: Arcsinh}\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Update any number of parameters (scale_type, cofactor, minimum, maximum)
#' # Scale Types -- 1: Linear, 2: Log, 4: Arcsinh
#' scales.update(cyto_session, scale=cyto_scale)
#' }
#' @export
setMethod("scales.update", signature(UserSession="UserSession"), function(UserSession, scale, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "scales", possible_outputs=c("raw"))
    update_check(scale, "scales")

    # All parameters are optional, except for experiment_id and scale_id
    body <- list(id=scale$id)
    if (!is.na(scale$scaleType))
    {
        body <- c(body, scaleType=scale$scaleType)
    }
    if (!is.na(scale$cofactor))
    {
        body <- c(body, cofactor=scale$cofactor)
    }
    if (!is.na(scale$minimum))
    {
        body <- c(body, minimum=scale$minimum)
    }
    if (!is.na(scale$maximum))
    {
        body <- c(body, maximum=scale$maximum)
    }

    resp <- PUT(paste(UserSession@site, "/experiments/", scale$experimentId, "/scales/", scale$id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(scale=body),
                encode="json",
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "scales")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "scales"))
    }
})

