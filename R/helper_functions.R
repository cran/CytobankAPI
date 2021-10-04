# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Helper Functions
#'
#' Various helper functions to utilize within the Cytobank API.
#' @name helper_functions
#' @param fcs_files vector of integers representing a list of FCS file IDs
#' @param ids_names_df dataframe containing both IDs and their associated names
#' @param long_channel_names vector of character representing long channel names
#' @param names_array vector or list of character regular expressions to use
#' @param panels_list list provided from the \link[=panels]{panels.list} endpoint
NULL


#' @rdname helper_functions
#' @details \code{helper.filter_names_to_ids_from_df} Compile a vector of IDs from an array of regular expressions.
#' @examples \dontrun{helper.filter_names_to_ids_from_df(id_and_names_dataframe,
#' names_list=c("CD.*", "Time", "pp38"))
#' }
#' @export
helper.filter_names_to_ids_from_df <- function(ids_names_df, names_array=c("*"))
{
    ids <- c()

    for (regex in names_array)
    {
        ids <- c(ids, ids_names_df[[1]][grep(regex, ids_names_df[[2]])])
    }

    return(as.numeric(unique(ids)))
}


#' @rdname helper_functions
#' @details \code{helper.channel_ids_from_long_names} Compile a vector of IDs based on long channel names for specific FCS files from an experiment. If no FCS files are provided, IDs will be retrieved based on unique short channel / long channel combinations across all FCS files.
#' @examples \dontrun{helper.channel_ids_from_long_names(panels.list(cyto_session, 22),
#'   long_channel_names=c("long_channel1", "long_channel2"), fcs_files=c(1,2,3,4,5))
#' }
#' @export
helper.channel_ids_from_long_names <- function(panels_list, long_channel_names, fcs_files=c())
{
    unique_channels <- get_unique_channels(panels_list, fcs_files)

    normalized_short_name_ids <- c()
    for (channel in long_channel_names)
    {
        ids <- unique_channels[unique_channels$longName %in% channel,]
        # Error out if multiple Short Names associated with a Long Name
        if (nrow(ids) > 1)
        {
            stop(
                sprintf (
                    paste("Cytobank API 'helper.channel_ids_from_long_names' request failed [client]\n    Same long name ['%s'] for the following short names:\n    - %s\n", sep=""),
                    channel,
                    paste(unlist(unique_channels[unique_channels$longName %in% channel,]$shortName), collapse="\n    - ")
                ),
                call. = FALSE
            )
        }

        normalized_short_name_ids <- c(normalized_short_name_ids, unlist(ids$normalizedShortNameId))
    }

    return(normalized_short_name_ids)
}


# Get panels for specific files chosen
get_unique_channels <- function(panels, fcs_files)
{
    panels_list <- c()
    for (panel in panels)
    {
        if (any(is.element(fcs_files, panel$fcs_files)) || is.null(fcs_files))
        {
            panels_list[[length(panels_list)+1]] <- list(panel$channels[c("shortName", "longName", "normalizedShortNameId")])
        }
    }

    if (length(panels_list)==1)
    {
        concatenated_panels <- panels_list[[1]][[1]]
    }
    else
    {
        concatenated_panels <- Reduce(function(...) merge(..., all=TRUE, sort=FALSE), panels_list)
    }

    return(unique(concatenated_panels[c("shortName", "longName", "normalizedShortNameId")]))
}

