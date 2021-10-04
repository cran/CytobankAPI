# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Gate Endpoints
#'
#' Interact with gate endpoints. In Cytobank there is a distinction between
#' gates and populations. A gate is simply a shape drawn on a plot. A
#' \link[=populations]{population} is a set of gates and can have parents and
#' children.
#' \href{https://support.cytobank.org/hc/en-us/articles/204765578-The-Difference-Between-a-Gate-and-a-Population-Using-the-Population-Manager-and-considerations-for-deleting-and-renaming-gates}{Learn
#' more about gates and populations}. Currently, gate and population information
#' can only be read and not written to Cytobank via the JSON API. To write gates
#' and populations to Cytobank via the API, the gates.gatingML_upload endpoint
#' should be used.
#' @name gates
#' @param directory character representing a specific directory to which the
#'   file will be downloaded (optional ending directory slash), if left empty,
#'   the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment}
#'   ID
#' @param file_path character representing a file path
#' @param gate_id integer representing a gate ID
#' @param output character representing the output format \strong{[optional]}\cr
#'   \emph{- gates.list, gates.show : \code{("default", "raw")}}
#' @param timeout integer representing the request timeout time in seconds
#'   \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("gates.gatingML_download", function(UserSession, experiment_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("gates.gatingML_download")
})
#' @rdname gates
#' @aliases gates.gatingML_download
#'
#' @details \code{gates.gatingML_download} Download the gatingML from an experiment. \href{https://support.cytobank.org/hc/en-us/articles/204765618-Exporting-and-Importing-Gates-within-Cytobank-and-with-Gating-ML#gatingml}{Learn more about Gating-ML}.
#' @examples \dontrun{gates.gatingML_download(cyto_session, 22, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("gates.gatingML_download", signature(UserSession="UserSession"), function(UserSession, experiment_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/download_gatingml/", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "gates")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("gates.gatingML_upload", function(UserSession, experiment_id, file_path, timeout=UserSession@long_timeout)
{
    standardGeneric("gates.gatingML_upload")
})
#' @rdname gates
#' @aliases gates.gatingML_upload
#'
#' @details \code{gates.gatingML_upload} Upload a gatingML to an experiment. \href{https://support.cytobank.org/hc/en-us/articles/204765618-Exporting-and-Importing-Gates-within-Cytobank-and-with-Gating-ML#gatingml}{Learn more about Gating-ML}.
#' @examples \dontrun{gates.gatingML_upload(cyto_session, 22, file_path="/path/to/my_gatingML.xml")
#' }
#' @export
setMethod("gates.gatingML_upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/upload_gatingml", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(file=upload_file(file_path)),
                 encode="multipart",
                 timeout(timeout)
    )

    return(parse(resp, "gates"))
})

setGeneric("gates.apply", function(UserSession, experiment_id, timeout=UserSession@long_timeout)
{
    standardGeneric("gates.apply")
})
#' @rdname gates
#' @aliases gates.apply
#'
#' @details \code{gates.apply} Apply gates as Experiment Gates. Gates must be applied in order for Scratch Gates to be converted to Experiment gates. Experiment gates are used for generating statistics, illustrations, and advanced analyses. \href{https://support.cytobank.org/hc/en-us/articles/205399487-The-Apply-and-Apply-and-Return-buttons-and-gate-versioning}{Learn more about applying gates}.
#' @examples \dontrun{gates.apply(cyto_session, 22)
#' }
#' @export
setMethod("gates.apply", signature(UserSession="UserSession"), function(UserSession, experiment_id, timeout=UserSession@long_timeout)
{
    gate_version <- get_gate_version(UserSession, experiment_id)

    gating_url <- paste0(gsub('/api/v1', '', UserSession@site), '/experiments/', experiment_id, '/gating')

    requestBody <- paste0(' { "experiment":
                       {
                       "gateVersion": ',
                          gate_version + 1,
                          ' } } ')

    resp <- httr::POST(
        url = paste0(UserSession@site, '/experiments/', experiment_id, '/apply_gates'),
        httr::add_headers('Authorization' = paste0('Bearer ', UserSession@auth_token)),
        httr::add_headers('Content-Type' = 'application/json'),
        body = requestBody,
        encode = "json",
        timeout(timeout))

    new_gate_version <- parse(resp)

    ifelse(new_gate_version$gatingSaveVersion == gate_version + 1,
           return('Gates successfully applied'),
           return(paste0('Gates not applied. Apply gates from gating page: ', gating_url)))

})

setGeneric("gates.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("gates.list")
})
#' @rdname gates
#' @aliases gates.list
#'
#' @details \code{gates.list} List all gates from an experiment. Outputs a dataframe [default] or raw list with all fields present. Currently only the Scratch Gates from the gating interface are returned. These have a version of -1. This is to be contrasted with Experiment Gates, which will have a version number that is a positive integer equal to the number of times the version has been incremented in the gating interface. \href{https://support.cytobank.org/hc/en-us/articles/205399487-The-Apply-and-Apply-and-Return-buttons-and-gate-versioning}{Learn more about gate versioning in Cytobank}.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all gates with all fields present
#' gates.list(cyto_session, 22)
#'
#' # Raw list of all gates with all fields present
#' gates.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("gates.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "gates", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/gates", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_gate_list(parse(resp, "gates")$gate))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "gates"))
    }
})

setGeneric("gates.show", function(UserSession, experiment_id, gate_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("gates.show")
})
#' @rdname gates
#' @aliases gates.show
#'
#' @details \code{gates.show} Show gate details from an experiment.
#' @examples \dontrun{gates.show(cyto_session, 22, gate_id=2)
#' }
#' @export
setMethod("gates.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, gate_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "gates", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/gates/", gate_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_gate(parse(resp, "gates")$gate))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "gates"))
    }
})


#########################
# GATES HELPER FUNCTIONS
#########################


##########
# PRIVATE
##########


# Build gate list in R
build_gate_list <- function(gate_list_info)
{
    gate_list <- list()
    for (gate in gate_list_info)
    {
        # Need to encapsulate built gate within list in order to do.call the rbind
        gate_list <- c(gate_list, list(build_gate(gate)))
    }

    return(do.call(rbind, gate_list))
}


# Build gate info in R
build_gate <- function(gate_info)
{
    # Create dataframe without definition
    gate <- cyto_dataframe(list(gate_info[-which(names(gate_info) == "definition")]))

    # Add yShortName for 1D gates as NA
    if (is.null(gate[["yShortName"]]))
    {
        gate[["yShortName"]] <- NA
    }

    # Add definition as a list
    gate$definition <- apply(gate, 1, function(row) list(gate_info$definition))

    return(gate)
}

# Get gate version
get_gate_version <- function(UserSession, experiment_id)
{
    # Get original gate version
    gate_version <- unlist(experiments.show(UserSession, experiment_id, output = 'default')[['gateVersion']])
    return(gate_version)
}

# Check if gates applied (not used)
check_gates_applied <- function(UserSession, experiment_id, gate_version)
{

    # Check if updated after applying gates
    gating_url <- paste0(gsub('/api/v1', '', UserSession@site), '/experiments/', experiment_id, '/gating')
    applied_gate_version <- unlist(experiments.show(UserSession, experiment_id, output = 'default')[['gateVersion']])

    ifelse(applied_gate_version == gate_version + 1,
           return('Gates successfully applied.'),
           return(paste0('Gates not applied. Apply gates from gating page: ', gating_url))
    )

}
