# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Panel Endpoints
#'
#' Interact with panel endpoints. A collection of channels, the markers being studied on them, and the FCS files this applies to form a panel. \href{https://support.cytobank.org/hc/en-us/articles/206148227-Overview-of-working-with-panels-and-channels}{Learn more about panels in Cytobank}.
#' @name panels
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- panels.list, panels.show : \code{("default", "raw")}}
#' @param panel_id integer representing a panel ID
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("panels.list", function(UserSession, experiment_id, output="full", timeout=UserSession@short_timeout)
{
    standardGeneric("panels.list")
})
#' @rdname panels
#' @aliases panels.list
#'
#' @details \code{panels.list} List all panels from an experiment. Outputs a formatted list [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Full panel list with all fields present, with a dataframe of channels
#' panels.list(cyto_session, 22)
#'
#' # Raw list of all panels with all fields present
#' panels.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("panels.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "panels", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/panels", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_panel_list(parse(resp, "panels")$panels))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "panels"))
    }
})


setGeneric("panels.show", function(UserSession, experiment_id, panel_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("panels.show")
})
#' @rdname panels
#' @aliases panels.show
#'
#' @details \code{panels.show} Show panel details from an experiment. Outputs a full list with all fields present, or an IDs/names list (See \link{attachments} examples section for IDs/names list example).\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Full panel info with all fields present
#' panels.show(cyto_session, 22, panel_id=2)
#' }
#' @export
setMethod("panels.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, panel_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "panels", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/panels/", panel_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_panel(parse(resp, "panels")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "panels"))
    }
})


##########################
# PANELS HELPER FUNCTIONS
##########################

# Need to rework for new dataframes when time comes
# panels.helper.channels_common_in_files(channels_ids_names_list, fcs_files=fcs_files)
# }
panels.helper.channels_common_in_files <- function (channel_ids_names_list, fcs_files)
{
    # Return channel names only present in specified files
    #   - See if all of the fcs files list is in each files field of the short_channel_id/name/files list
    #   - Filter out all short_channel_id/name/files that do not contain the whole fcs files list
    return(channel_ids_names_list[lapply(lapply(channel_ids_names_list,
                                        function(x){fcs_files %in% x[["files"]]}),
                                 function(x){all(x)}) == TRUE])
}


##########
# PRIVATE
##########


# Build panel list in R
build_panel_list <- function(panel_list_info)
{
    panel_list <- list()
    for(panel in panel_list_info)
    {
        panel_list <- c(panel_list, build_panel(panel))
    }

    return(panel_list)
}


# Build panel info in R
build_panel <- function(panel_info)
{
    return(
        setNames(
            list(list(id=panel_info$id,
                      fcs_files=unlist(panel_info$fcsFiles),
                      channels=cyto_dataframe(panel_info$channels))),
            panel_info$name
        )
    )
}

