# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' FlowSOM Endpoints
#'
#' Interact with FlowSOM advanced analyses using these endpoints.
#' @name flowsom
#' @param flowsom Cytobank FlowSOM object
#' @param flowsom_id integer representing a FlowSOM ID
#' @param flowsom_name character representing a new FlowSOM name
#' @param directory character representing a specific directory to which the file will be downloaded (optional ending directory slash), if left empty, the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- flowsom.list, flowsom.run, flowsom.status : \code{("default", "raw")}}
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#'
#' # cyto_flowsom refers to a FlowSOM object that is created from FlowSOM endpoints
#' #   examples: flowsom.new, flowsom.show (see details section for more)
#' }
NULL


######################
# FlowSOM class methods
######################


setGeneric("flowsom.copy_settings", function(UserSession, flowsom, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "FlowSOM", possible_outputs=c("raw"))

    standardGeneric("flowsom.copy_settings")
})
#' @rdname flowsom
#' @aliases flowsom.copy_settings
#'
#' @details \code{flowsom.copy_settings} Copy FlowSOM advanced analysis settings from an experiment and returns a FlowSOM object.
#' @examples \dontrun{flowsom.copy_settings(cyto_session, flowsom=cyto_flowsom)
#' }
#' @export
setMethod("flowsom.copy_settings", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, output="default", timeout=UserSession@short_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", flowsom@source_experiment, "/advanced_analyses/flowsom/", flowsom@flowsom_id, "/copy_settings", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "FlowSOM")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "FlowSOM"))
    }
})


setGeneric("flowsom.delete", function(UserSession, flowsom, timeout=UserSession@short_timeout)
{
    standardGeneric("flowsom.delete")
})
#' @rdname flowsom
#' @aliases flowsom.delete
#'
#' @details \code{flowsom.delete} Delete a FlowSOM advanced analysis from an experiment.
#' @examples \dontrun{flowsom.delete(cyto_session, flowsom=cyto_flowsom)
#' }
#' @export
setMethod("flowsom.delete", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", flowsom@source_experiment, "/advanced_analyses/flowsom/", flowsom@flowsom_id, sep=""),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "experiments")
    }

    return(paste("FlowSOM (ID=", flowsom@flowsom_id, ") successfully deleted.", sep=""))
})


setGeneric("flowsom.download", function(UserSession, flowsom, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("flowsom.download")
})
#' @rdname flowsom
#' @aliases flowsom.download
#'
#' @details \code{flowsom.download} Download a FlowSOM analysis from an experiment.
#' @examples \dontrun{# Download a FlowSOM analysis to the current working directory
#' flowsom.download(cyto_session, flowsom)
#'
#' # Download a FlowSOM analysis to a new directory
#' flowsom.download(cyto_session, flowsom, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("flowsom.download", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, directory=getwd(), timeout=UserSession@long_timeout)
{
    if (is.na(flowsom@attachment_id))
    {
        stop(
            sprintf(
                paste("Cytobank API 'FlowSOM' request failed [client]\n    Must provide a valid FlowSOM attachment ID, cannot be NA", sep="")
            ),
            call. = FALSE
        )
    }

    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", flowsom@created_experiment, "/attachments/", flowsom@attachment_id, "/download", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "flowsom")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("flowsom.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("flowsom.list")
})
#' @rdname flowsom
#' @aliases flowsom.list
#'
#' @details \code{flowsom.list} List all FlowSOM advanced analyses from an experiment. Outputs a dataframe [default] or list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all FlowSOM advanced analyses with all fields present
#' flowsom.list(cyto_session, 22)
#'
#' # Raw list of all FlowSOM advanced analyses with all fields present
#' flowsom.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("flowsom.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "FlowSOM", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/flowsom", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "FlowSOM")$flowsom))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "FlowSOM"))
    }
})


setGeneric("flowsom.new", function(UserSession, experiment_id, flowsom_name, timeout=UserSession@long_timeout)
{
    standardGeneric("flowsom.new")
})
#' @rdname flowsom
#' @aliases flowsom.new
#'
#' @details \code{flowsom.new} Create a new FlowSOM advanced analysis from an experiment and returns a FlowSOM object.
#' @examples \dontrun{flowsom.new(cyto_session, 22, flowsom_name="My new FlowSOM analysis")
#' }
#' @export
setMethod("flowsom.new", signature(UserSession="UserSession"), function(UserSession, experiment_id, flowsom_name, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/flowsom/", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(name=flowsom_name),
                 encode="json",
                 timeout(timeout)
    )

    return(create_flowsom_object(UserSession, parse(resp, "FlowSOM")))
})


setGeneric("flowsom.rename", function(UserSession, flowsom, flowsom_name, timeout=UserSession@short_timeout)
{
    standardGeneric("flowsom.rename")
})
#' @rdname flowsom
#' @aliases flowsom.rename
#'
#' @details \code{flowsom.rename} Rename a FlowSOM advanced analysis from an experiment and returns a FlowSOM object.
#' @examples \dontrun{flowsom.rename(cyto_session, flowsom=cyto_flowsom, flowsom_name=
#' "My updated FlowSOM name")
#' }
#' @export
setMethod("flowsom.rename", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, flowsom_name, timeout=UserSession@short_timeout)
{
    resp <- PUT(paste(UserSession@site, "/experiments/", flowsom@source_experiment, "/advanced_analyses/flowsom/", flowsom@flowsom_id, "/rename", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(name=flowsom_name),
                encode="json",
                timeout(timeout)
    )

    flowsom@name <- parse(resp, "FlowSOM")$flowsom$name
    return(flowsom)
})


setGeneric("flowsom.run", function(UserSession, flowsom, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("flowsom.run")
})
#' @rdname flowsom
#' @aliases flowsom.run
#'
#' @details \code{flowsom.run} Run a FlowSOM advanced analysis from an experiment.
#' @examples \dontrun{flowsom.run(cyto_session, flowsom=cyto_flowsom)
#' }
#' @export
setMethod("flowsom.run", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "FlowSOM", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", flowsom@source_experiment, "/advanced_analyses/flowsom/", flowsom@flowsom_id, "/run", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "FlowSOM")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "FlowSOM"))
    }
})


setGeneric("flowsom.show", function(UserSession, experiment_id, flowsom_id, timeout=UserSession@short_timeout)
{
    standardGeneric("flowsom.show")
})
#' @rdname flowsom
#' @aliases flowsom.show
#'
#' @details \code{flowsom.show} Show FlowSOM advanced analysis details from an experiment and returns a FlowSOM object.
#' @examples \dontrun{flowsom.show(cyto_session, 22, flowsom_id=2)
#' }
#' @export
setMethod("flowsom.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, flowsom_id, timeout=UserSession@short_timeout)
{
    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/flowsom/", flowsom_id, "?include_settings=1", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    return(create_flowsom_object(UserSession, parse(resp, "FlowSOM")))
})


setGeneric("flowsom.status", function(UserSession, flowsom, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("flowsom.status")
})
#' @rdname flowsom
#' @aliases flowsom.status
#'
#' @details \code{flowsom.status} Show the status of a FlowSOM advanced analysis from an experiment.
#' @examples \dontrun{flowsom.status(cyto_session, flowsom=cyto_flowsom)
#' }
#' @export
setMethod("flowsom.status", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "FlowSOM", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", flowsom@source_experiment, "/advanced_analyses/flowsom/", flowsom@flowsom_id, "/status", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "FlowSOM")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "FlowSOM"))
    }
})


setGeneric("flowsom.update", function(UserSession, flowsom, timeout=UserSession@long_timeout)
{
    standardGeneric("flowsom.update")
})
#' @rdname flowsom
#' @aliases flowsom.update
#'
#' @details \code{flowsom.update} Update a FlowSOM advanced analysis from an experiment and returns the new FlowSOM object.
#' @examples \dontrun{flowsom.update(cyto_session, flowsom=cyto_flowsom)
#' }
#' @export
setMethod("flowsom.update", signature(UserSession="UserSession", flowsom="FlowSOM"), function(UserSession, flowsom, timeout=UserSession@long_timeout)
{
    if (length(flowsom@channels) && is.character(flowsom@channels[[1]]))
    {
        flowsom@channels <- as.list(helper.channel_ids_from_long_names(flowsom@.available_channels, flowsom@channels))
        if (!length(flowsom@channels_to_plot))
        {
            flowsom@channels_to_plot <- flowsom@channels
        }
    }
    if (length(flowsom@channels_to_plot) && is.character(flowsom@channels_to_plot[[1]]))
    {
        flowsom@channels_to_plot <- as.list(helper.channel_ids_from_long_names(flowsom@.available_channels, flowsom@channels_to_plot))
    }

    # required settings for running FlowSOM
    if (!length(flowsom@fcs_files))
    {
        stop(
            sprintf (
                "Cytobank API 'flowsom.update' request failed [client]\n    Must specify FCS file IDs in order to update."
            ),
            call. = FALSE
        )
    }
    if (!length(flowsom@channels))
    {
        stop(
            sprintf (
                "Cytobank API 'flowsom.update' request failed [client]\n    Must specify clustering channels in order to update."
            ),
            call. = FALSE
        )
    }

    resp <- PUT(paste(UserSession@site, "/experiments/", flowsom@source_experiment, "/advanced_analyses/flowsom/", flowsom@flowsom_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(options=list(
                              clusteringChannels=flowsom@channels,
                              clusteringMethod=flowsom@clustering_method,
                              compensationId=flowsom@compensation_id,
                              desiredEventsPerFile=flowsom@desired_events_per_file,
                              desiredTotalEvents=flowsom@desired_total_events,
                              eventSamplingMethod=flowsom@event_sampling_method,
                              expectedClusters=flowsom@expected_clusters,
                              expectedMetaclusters=flowsom@expected_metaclusters,
                              fcsFileIds=flowsom@fcs_files,
                              gateSetId=flowsom@population_id,
                              iterations=flowsom@iterations,
                              normalizeScales=flowsom@normalize_scales,
                              randomSeed=flowsom@random_seed,
                              SOMCreationMethod=flowsom@som_creation_method,
                              externalSOMAnalysisId=flowsom@external_som_analysis_id,
                              channelsToPlot=flowsom@channels_to_plot,
                              clusterSizeType=flowsom@cluster_size_type,
                              fixedClusterSize=flowsom@fixed_cluster_size,
                              gateSetNamesToLabe=flowsom@gate_set_names_to_label,
                              maxRelativeClusterSize=flowsom@max_relative_cluster_size,
                              outputFileType=flowsom@output_file_type,
                              showBackgroundOnLegend=flowsom@show_background_on_legend,
                              showBackgroundOnChannelColoredMSTs=flowsom@show_background_on_channel_colored_msts,
                              showBackgroundOnPopulationPies=flowsom@show_background_on_population_pies
                          )
                ),
                encode="json",
                timeout(timeout)
    )

    return(create_flowsom_object(UserSession, parse(resp, "FlowSOM")))
})


##########################
# FlowSOM HELPER FUNCTIONS
##########################


##########
# PRIVATE
##########


# Create FlowSOM object from FlowSOM json response
create_flowsom_object <- function(UserSession, flowsom_response)
{
    fcs_files <- fcs_files.list(UserSession, flowsom_response$flowsom$sourceExperiment)

    return(
        new("FlowSOM",
            attachment_id=if (!is.integer(flowsom_response$flowsom$settings$attachmentId)) NA_integer_ else flowsom_response$flowsom$settings$attachmentId,
            author=flowsom_response$flowsom$author,
            auto_seed=if (!is.logical(flowsom_response$flowsom$settings$random_seed)) TRUE else flowsom_response$flowsom$settings$autoSeed,
            canceled=flowsom_response$flowsom$canceled,
            channels=flowsom_response$flowsom$settings$clusteringChannels,
            channels_to_plot=flowsom_response$flowsom$settings$channelsToPlot,
            clustering_method=flowsom_response$flowsom$settings$clusteringMethod,
            cluster_size_type=flowsom_response$flowsom$settings$clusterSizeType,
            completed=flowsom_response$flowsom$completed,
            compensation_id=flowsom_response$flowsom$settings$compensationId,
            created_experiment=if (!is.integer(flowsom_response$flowsom$createdExperiment)) NA_integer_ else flowsom_response$flowsom$createdExperiment,
            desired_events_per_file=flowsom_response$flowsom$settings$desiredEventsPerFile,
            desired_total_events=flowsom_response$flowsom$settings$desiredTotalEvents,
            event_sampling_method=flowsom_response$flowsom$settings$eventSamplingMethod,
            expected_clusters=flowsom_response$flowsom$settings$expectedClusters,
            expected_metaclusters=flowsom_response$flowsom$settings$expectedMetaclusters,
            external_som_analysis_info=flowsom_response$flowsom$settings$externalSOMAnalysisInfo,
            external_som_analysis_id=flowsom_response$flowsom$settings$externalSOMAnalysisId,
            fcs_files=flowsom_response$flowsom$settings$fcsFileIds,
            final_result=flowsom_response$flowsom$settings$finalResult,
            fixed_cluster_size=flowsom_response$flowsom$settings$fixedClusterSize,
            flowsom_id=flowsom_response$flowsom$id,
            gate_set_names_to_label=flowsom_response$flowsom$settings$gateSetNamesToLabel,
            iterations=flowsom_response$flowsom$settings$iterations,
            max_relative_cluster_size=flowsom_response$flowsom$settings$maxRelativeClusterSize,
            name=flowsom_response$flowsom$name,
            normalize_scales=flowsom_response$flowsom$settings$normalizeScales,
            num_events_to_actually_sample=if (!is.integer(flowsom_response$flowsom$settings$numEventsToActuallySample)) 0 else flowsom_response$flowsom$settings$numEventsToActuallySample,
            num_fcs_files=if (!is.integer(flowsom_response$flowsom$settings$numFcsFiles)) 0 else flowsom_response$flowsom$settings$numFcsFiles,
            output_file_type=flowsom_response$flowsom$settings$outputFileType,
            population_id=flowsom_response$flowsom$settings$gateSetId,
            random_seed=if (!is.integer(flowsom_response$flowsom$settings$randomSeed)) NA_integer_ else flowsom_response$flowsom$settings$randomSeed,
            show_background_on_legend=flowsom_response$flowsom$settings$showBackgroundOnLegend,
            show_background_on_channel_colored_msts=flowsom_response$flowsom$settings$showBackgroundOnChannelColoredMSTs,
            show_background_on_population_pies=flowsom_response$flowsom$settings$showBackgroundOnPopulationPies,
            som_creation_method=flowsom_response$flowsom$settings$SOMCreationMethod,
            source_experiment=flowsom_response$flowsom$sourceExperiment,
            status=flowsom_response$flowsom$status,
            type=flowsom_response$flowsom$type,
            .available_channels=panels.list(UserSession, flowsom_response$flowsom$sourceExperiment),
            .available_files=fcs_files,
            .available_populations=populations.list(UserSession, flowsom_response$flowsom$sourceExperiment))
    )
}

