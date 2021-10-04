# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' DimensionalityReduction Endpoints
#'
#' Interact with DimensionalityReduction advanced analyses using these endpoints.
#' @name dimensionality_reduction
#' @param dimensionality_reduction Cytobank DimensionalityReduction object
#' @param analysis_id integer representing the Dimensionality Reduction analysis ID
#' @param analysis_name character the name of the Dimensionality Reduction analysis
#' @param analysis_type character representing the Dimensionality Reduction type (tSNE-CUDA, opt-SNE, UMAP, or viSNE)
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- dimensionality_reduction.list, dimensionality_reduction.run, dimensionality_reduction.status : \code{("default", "raw")}}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#'
#' # cyto_dimensionality_reduction refers to a DimensionalityReduction object that is created from
#' DimensionalityReduction endpoints
#' #   examples: dimensionality_reduction.new, dimensionality_reduction.show (see details section for
#' more)
#' }
NULL


######################
# DimensionalityReduction class methods
######################


setGeneric("dimensionality_reduction.copy_settings", function(UserSession, dimensionality_reduction, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "DimensionalityReduction", possible_outputs=c("raw"))

    standardGeneric("dimensionality_reduction.copy_settings")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.copy_settings
#'
#' @details \code{dimensionality_reduction.copy_settings} Copy DimensionalityReduction advanced analysis
#' settings from an experiment and returns a DimensionalityReduction object.
#' @examples \dontrun{dimensionality_reduction.copy_settings(cyto_session,
#'  dimensionality_reduction=cyto_dimensionality_reduction)
#' }
#' @export
setMethod("dimensionality_reduction.copy_settings", signature(UserSession="UserSession", dimensionality_reduction="DimensionalityReduction"), function(UserSession, dimensionality_reduction, output="default", timeout=UserSession@short_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", dimensionality_reduction@source_experiment, "/advanced_analyses/", dimensionality_reduction@type, "/", dimensionality_reduction@analysis_id, "/copy_settings", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "DimensionalityReduction")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "DimensionalityReduction"))
    }
})


setGeneric("dimensionality_reduction.delete", function(UserSession, dimensionality_reduction, timeout=UserSession@short_timeout)
{
    standardGeneric("dimensionality_reduction.delete")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.delete
#'
#' @details \code{dimensionality_reduction.delete} Delete a DimensionalityReduction advanced analysis
#' from an experiment.
#' @examples \dontrun{dimensionality_reduction.delete(cyto_session,
#' dimensionality_reduction=cyto_dimensionality_reduction)
#' }
#' @export
setMethod("dimensionality_reduction.delete", signature(UserSession="UserSession", dimensionality_reduction="DimensionalityReduction"), function(UserSession, dimensionality_reduction, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", dimensionality_reduction@source_experiment, "/advanced_analyses/", dimensionality_reduction@type, "/", dimensionality_reduction@analysis_id, sep=""),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "experiments")
    }

    return(paste("DimensionalityReduction (ID=", dimensionality_reduction@analysis_id, ") successfully deleted.", sep=""))
})


setGeneric("dimensionality_reduction.list", function(UserSession, experiment_id, analysis_type, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("dimensionality_reduction.list")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.list
#'
#' @details \code{dimensionality_reduction.list} List all DimensionalityReduction advanced analyses from an experiment.
#' Outputs a dataframe [default] or list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all DimensionalityReduction advanced analyses with all fields present
#' dimensionality_reduction.list(cyto_session, 22, "viSNE")
#'
#' # Raw list of all DimensionalityReduction advanced analyses with all fields present
#' dimensionality_reduction.list(cyto_session, 22, "viSNE", output="raw")
#' }
#' @export
setMethod("dimensionality_reduction.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, analysis_type, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "DimensionalityReduction", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_type, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        pased_resp <- parse(resp, "DimensionalityReduction")

        analysis_type <- attributes(pased_resp)$names

        return(cyto_dataframe(pased_resp[[analysis_type]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "DimensionalityReduction"))
    }
})


setGeneric("dimensionality_reduction.new", function(UserSession, experiment_id, analysis_name, analysis_type, timeout=UserSession@long_timeout)
{
    standardGeneric("dimensionality_reduction.new")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.new
#'
#' @details \code{dimensionality_reduction.new} Create a new DimensionalityReduction advanced analysis
#' from an experiment and returns a DimensionalityReduction object.
#' @examples \dontrun{dimensionality_reduction.new(cyto_session, 22,
#' analysis_name="My new DimensionalityReduction analysis", "UMAP")
#' }
#' @export
setMethod("dimensionality_reduction.new", signature(UserSession="UserSession"), function(UserSession, experiment_id, analysis_name, analysis_type, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_type , sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=if (tolower(analysis_type) == "visne") list(visne=list(name=analysis_name)) else list(name=analysis_name),
                 encode="json",
                 timeout(timeout)
    )

    return(create_dimensionality_reduction_object(UserSession, parse(resp, "DimensionalityReduction")))
})


setGeneric("dimensionality_reduction.rename", function(UserSession, dimensionality_reduction, analysis_name, timeout=UserSession@short_timeout)
{
    standardGeneric("dimensionality_reduction.rename")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.rename
#'
#' @details \code{dimensionality_reduction.rename} Rename a DimensionalityReduction advanced analysis
#' from an experiment and returns the new name.
#' @examples \dontrun{dimensionality_reduction.rename(cyto_session,
#' dimensionality_reduction=cyto_dimensionality_reduction,
#' analysis_name="My updated DimensionalityReduction name")
#' }
#' @export
setMethod("dimensionality_reduction.rename", signature(UserSession="UserSession", dimensionality_reduction="DimensionalityReduction"), function(UserSession, dimensionality_reduction, analysis_name, timeout=UserSession@short_timeout)
{
    resp <- PUT(paste(UserSession@site, "/experiments/", dimensionality_reduction@source_experiment, "/advanced_analyses/", dimensionality_reduction@type, "/", dimensionality_reduction@analysis_id, "/rename", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=if (tolower(dimensionality_reduction@type) == "visne") list(visne=list(name=analysis_name)) else list(name=analysis_name),
                encode="json",
                timeout(timeout)
    )

    dimensionality_reduction_response <- parse(resp, "DimensionalityReduction")
    analysis_type <- attributes(dimensionality_reduction_response)$names
    dimensionality_reduction_response <- dimensionality_reduction_response[[analysis_type]]

    return(dimensionality_reduction_response$name)
})


setGeneric("dimensionality_reduction.run", function(UserSession, dimensionality_reduction, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("dimensionality_reduction.run")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.run
#'
#' @details \code{dimensionality_reduction.run} Run a DimensionalityReduction advanced analysis from an experiment.
#' @examples \dontrun{dimensionality_reduction.run(cyto_session,
#' dimensionality_reduction=cyto_dimensionality_reduction)
#' }
#' @export
setMethod("dimensionality_reduction.run", signature(UserSession="UserSession", dimensionality_reduction="DimensionalityReduction"), function(UserSession, dimensionality_reduction, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "DimensionalityReduction", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", dimensionality_reduction@source_experiment, "/advanced_analyses/", dimensionality_reduction@type, "/", dimensionality_reduction@analysis_id, "/run", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "DimensionalityReduction")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "DimensionalityReduction"))
    }
})


setGeneric("dimensionality_reduction.show", function(UserSession, experiment_id, analysis_id, analysis_type, timeout=UserSession@short_timeout)
{
    standardGeneric("dimensionality_reduction.show")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.show
#'
#' @details \code{dimensionality_reduction.show} Show DimensionalityReduction advanced analysis details
#' from an experiment and returns a DimensionalityReduction object.
#' @examples \dontrun{dimensionality_reduction.show(cyto_session, 22, analysis_id=2, "opt-SNE")
#' }
#' @export
setMethod("dimensionality_reduction.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, analysis_id, analysis_type, timeout=UserSession@short_timeout)
{
    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_type, "/", analysis_id, "?include_settings=1", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    return(create_dimensionality_reduction_object(UserSession, parse(resp, "DimensionalityReduction")))
})


setGeneric("dimensionality_reduction.status", function(UserSession, dimensionality_reduction, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("dimensionality_reduction.status")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.status
#'
#' @details \code{dimensionality_reduction.status} Show the status of a DimensionalityReduction advanced analysis from an experiment.
#' @examples \dontrun{dimensionality_reduction.status(cyto_session,
#' dimensionality_reduction=cyto_dimensionality_reduction)
#' }
#' @export
setMethod("dimensionality_reduction.status", signature(UserSession="UserSession", dimensionality_reduction="DimensionalityReduction"), function(UserSession, dimensionality_reduction, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "DimensionalityReduction", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", dimensionality_reduction@source_experiment, "/advanced_analyses/", dimensionality_reduction@type, "/", dimensionality_reduction@analysis_id, "/status", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "DimensionalityReduction")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "DimensionalityReduction"))
    }
})


setGeneric("dimensionality_reduction.update", function(UserSession, dimensionality_reduction, timeout=UserSession@long_timeout)
{
    standardGeneric("dimensionality_reduction.update")
})
#' @rdname dimensionality_reduction
#' @aliases dimensionality_reduction.update
#'
#' @details \code{dimensionality_reduction.update} Update a DimensionalityReduction advanced analysis
#' from an experiment and returns the new DimensionalityReduction object.
#' @examples \dontrun{dimensionality_reduction.update(cyto_session,
#' dimensionality_reduction=cyto_dimensionality_reduction)
#' }
#' @export
setMethod("dimensionality_reduction.update", signature(UserSession="UserSession", dimensionality_reduction="DimensionalityReduction"), function(UserSession, dimensionality_reduction, timeout=UserSession@long_timeout)
{
    # Convert population selections dataframe -> list readable by update endpoint
    if (dimensionality_reduction@type == "viSNE")
    {
        population_selections <- population_selections_dataframe_to_list(dimensionality_reduction@population_selections)

        if (length(dimensionality_reduction@channels) && is.character(dimensionality_reduction@channels[[1]]))
        {
            dimensionality_reduction@channels <- as.list(helper.channel_ids_from_long_names(dimensionality_reduction@.available_channels, dimensionality_reduction@channels))
        }
    } else {
        if (length(dimensionality_reduction@clustering_channels) && is.character(dimensionality_reduction@clustering_channels[[1]]))
        {
            dimensionality_reduction@clustering_channels <- as.list(helper.channel_ids_from_long_names(dimensionality_reduction@.available_channels, dimensionality_reduction@clustering_channels))
        }
    }

    dim_body <- list()

    switch(
        dimensionality_reduction@type,
        "viSNE"={
            dim_body$visne=list(
                name=dimensionality_reduction@name,
                compensationId=dimensionality_reduction@compensation_id,
                samplingTotalCount=dimensionality_reduction@sampling_total_count,
                samplingTargetType=dimensionality_reduction@sampling_target_type,
                iterations=dimensionality_reduction@iterations,
                perplexity=dimensionality_reduction@perplexity,
                theta=dimensionality_reduction@theta,
                seed=dimensionality_reduction@seed,
                channels=if (length(dimensionality_reduction@channels)) dimensionality_reduction@channels else list(),
                populationSelections=population_selections
            )
        },
        "tSNE-CUDA"={
            dim_body$options=list(
                name=dimensionality_reduction@name,
                iterations=dimensionality_reduction@iterations,
                perplexity=dimensionality_reduction@perplexity,
                autoIterations=dimensionality_reduction@auto_iterations,
                autoLearningRate=dimensionality_reduction@auto_learning_rate,
                clusteringChannels=dimensionality_reduction@clustering_channels,
                desiredEventsPerFile=dimensionality_reduction@desired_events_per_file,
                desiredTotalEvents=dimensionality_reduction@desired_total_events,
                earlyExaggeration=dimensionality_reduction@early_exaggeration,
                eventSamplingMethod=dimensionality_reduction@event_sampling_method,
                fcsFileIds=dimensionality_reduction@fcsfile_ids,
                gateSetId=dimensionality_reduction@gateset_id,
                learningRate=dimensionality_reduction@learning_rate,
                normalizeScales=dimensionality_reduction@normalize_scales
            )
        },
        "UMAP"={
            dim_body$options=list(
                name=dimensionality_reduction@name,
                clusteringChannels=dimensionality_reduction@clustering_channels,
                collapseOutliers=dimensionality_reduction@collapse_outliers,
                desiredEventsPerFile=dimensionality_reduction@desired_events_per_file,
                desiredTotalEvents=dimensionality_reduction@desired_total_events,
                eventSamplingMethod=dimensionality_reduction@event_sampling_method,
                fcsFileIds=dimensionality_reduction@fcsfile_ids,
                gateSetId=dimensionality_reduction@gateset_id,
                minDistance=dimensionality_reduction@min_distance,
                numNeighbors=dimensionality_reduction@num_neighbors,
                normalizeScales=dimensionality_reduction@normalize_scales
            )
        },
        "opt-SNE"={
            dim_body$options=list(
                name=dimensionality_reduction@name,
                perplexity=dimensionality_reduction@perplexity,
                autoLearningRate=dimensionality_reduction@auto_learning_rate,
                clusteringChannels=dimensionality_reduction@clustering_channels,
                desiredEventsPerFile=dimensionality_reduction@desired_events_per_file,
                desiredTotalEvents=dimensionality_reduction@desired_total_events,
                earlyExaggeration=dimensionality_reduction@early_exaggeration,
                eventSamplingMethod=dimensionality_reduction@event_sampling_method,
                fcsFileIds=dimensionality_reduction@fcsfile_ids,
                gateSetId=dimensionality_reduction@gateset_id,
                learningRate=dimensionality_reduction@learning_rate,
                maxIterations=dimensionality_reduction@max_iterations,
                normalizeScales=dimensionality_reduction@normalize_scales,
                randomSeed=dimensionality_reduction@random_seed
            )
        }
    )


    resp <- PUT(paste(UserSession@site, "/experiments/", dimensionality_reduction@source_experiment, "/advanced_analyses/", tolower(dimensionality_reduction@type), "/", dimensionality_reduction@analysis_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=dim_body,
                encode="json",
                timeout(timeout)
    )

    return(create_dimensionality_reduction_object(UserSession, parse(resp, "DimensionalityReduction")))
})


##########
# PRIVATE
##########


# Create DimensionalityReduction object from DimensionalityReduction json response
create_dimensionality_reduction_object <- function(UserSession, dimensionality_reduction_response)
{
    analysis_type <- attributes(dimensionality_reduction_response)$names

    dimensionality_reduction_response <- dimensionality_reduction_response[[analysis_type]]

    specific_type <- get_specific_analysis_type(dimensionality_reduction_response)

    switch(
        specific_type,
        "viSNE"={
            return(
                new("viSNE",
                    analysis_id=dimensionality_reduction_response$id,
                    visne_id=dimensionality_reduction_response$id,
                    name=dimensionality_reduction_response$name,
                    status=dimensionality_reduction_response$status,
                    type=specific_type,
                    source_experiment=dimensionality_reduction_response$sourceExperiment,
                    created_experiment=if (!is.null(dimensionality_reduction_response$createdExperiment)) dimensionality_reduction_response$createdExperiment else NA_integer_,
                    iterations=dimensionality_reduction_response$settings$iterations,
                    perplexity=dimensionality_reduction_response$settings$perplexity,
                    sampling_total_count=dimensionality_reduction_response$settings$samplingTotalCount,
                    sampling_target_type=dimensionality_reduction_response$settings$samplingTargetType,
                    compensation_id=dimensionality_reduction_response$settings$compensationId,
                    channels=dimensionality_reduction_response$settings$channelIds,
                    theta=dimensionality_reduction_response$settings$theta,
                    seed=dimensionality_reduction_response$settings$seed,
                    population_selections=create_population_selections(dimensionality_reduction_response$settings$populationSelections),
                    .available_channels=panels.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_files=fcs_files.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_populations=populations.list(UserSession, dimensionality_reduction_response$sourceExperiment))
            )
        },
        "tSNE-CUDA"={
            return(
                new("tSNE",
                    analysis_id=dimensionality_reduction_response$id,
                    type=specific_type,
                    name=dimensionality_reduction_response$name,
                    status=dimensionality_reduction_response$status,
                    source_experiment=dimensionality_reduction_response$sourceExperiment,
                    created_experiment=if (!is.null(dimensionality_reduction_response$createdExperiment)) dimensionality_reduction_response$createdExperiment else NA_integer_,
                    iterations=dimensionality_reduction_response$settings$iterations,
                    perplexity=dimensionality_reduction_response$settings$perplexity,
                    auto_iterations=dimensionality_reduction_response$settings$autoIterations,
                    auto_learning_rate=dimensionality_reduction_response$settings$autoLearningRate,
                    clustering_channels=dimensionality_reduction_response$settings$clusteringChannels,
                    num_events_to_actually_sample=dimensionality_reduction_response$settings$numEventsToActuallySample,
                    desired_events_per_file=dimensionality_reduction_response$settings$desiredEventsPerFile,
                    desired_total_events=dimensionality_reduction_response$settings$desiredTotalEvents,
                    early_exaggeration=dimensionality_reduction_response$settings$earlyExaggeration,
                    event_sampling_method=dimensionality_reduction_response$settings$eventSamplingMethod,
                    fcsfile_ids=dimensionality_reduction_response$settings$fcsFileIds,
                    gateset_id=dimensionality_reduction_response$settings$gateSetId,
                    learning_rate=dimensionality_reduction_response$settings$learningRate,
                    normalize_scales=dimensionality_reduction_response$settings$normalizeScales,
                    .available_channels=panels.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_files=fcs_files.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_populations=populations.list(UserSession, dimensionality_reduction_response$sourceExperiment))
            )
        },
        "UMAP"={
            return(
                new("UMAP",
                    analysis_id=dimensionality_reduction_response$id,
                    type=specific_type,
                    name=dimensionality_reduction_response$name,
                    status=dimensionality_reduction_response$status,
                    source_experiment=dimensionality_reduction_response$sourceExperiment,
                    created_experiment=if (!is.null(dimensionality_reduction_response$createdExperiment)) dimensionality_reduction_response$createdExperiment else NA_integer_,
                    clustering_channels=dimensionality_reduction_response$settings$clusteringChannels,
                    collapse_outliers=dimensionality_reduction_response$settings$collapseOutliers,
                    num_events_to_actually_sample=dimensionality_reduction_response$settings$numEventsToActuallySample,
                    desired_events_per_file=dimensionality_reduction_response$settings$desiredEventsPerFile,
                    desired_total_events=dimensionality_reduction_response$settings$desiredTotalEvents,
                    event_sampling_method=dimensionality_reduction_response$settings$eventSamplingMethod,
                    fcsfile_ids=dimensionality_reduction_response$settings$fcsFileIds,
                    gateset_id=dimensionality_reduction_response$settings$gateSetId,
                    min_distance=dimensionality_reduction_response$settings$minDistance,
                    num_neighbors=dimensionality_reduction_response$settings$numNeighbors,
                    normalize_scales=dimensionality_reduction_response$settings$normalizeScales,
                    .available_channels=panels.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_files=fcs_files.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_populations=populations.list(UserSession, dimensionality_reduction_response$sourceExperiment))
            )
        },
        "opt-SNE"={
            return(
                new("optSNE",
                    analysis_id=dimensionality_reduction_response$id,
                    type=specific_type,
                    name=dimensionality_reduction_response$name,
                    status=dimensionality_reduction_response$status,
                    source_experiment=dimensionality_reduction_response$sourceExperiment,
                    created_experiment=if (!is.null(dimensionality_reduction_response$createdExperiment)) dimensionality_reduction_response$createdExperiment else NA_integer_,
                    perplexity=dimensionality_reduction_response$settings$perplexity,
                    auto_learning_rate=dimensionality_reduction_response$settings$autoLearningRate,
                    clustering_channels=dimensionality_reduction_response$settings$clusteringChannels,
                    num_events_to_actually_sample=dimensionality_reduction_response$settings$numEventsToActuallySample,
                    desired_events_per_file=dimensionality_reduction_response$settings$desiredEventsPerFile,
                    desired_total_events=dimensionality_reduction_response$settings$desiredTotalEvents,
                    early_exaggeration=dimensionality_reduction_response$settings$earlyExaggeration,
                    event_sampling_method=dimensionality_reduction_response$settings$eventSamplingMethod,
                    fcsfile_ids=dimensionality_reduction_response$settings$fcsFileIds,
                    gateset_id=dimensionality_reduction_response$settings$gateSetId,
                    learning_rate=dimensionality_reduction_response$settings$learningRate,
                    max_iterations=dimensionality_reduction_response$settings$maxIterations,
                    normalize_scales=dimensionality_reduction_response$settings$normalizeScales,
                    random_seed=dimensionality_reduction_response$settings$randomSeed,
                    .available_channels=panels.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_files=fcs_files.list(UserSession, dimensionality_reduction_response$sourceExperiment),
                    .available_populations=populations.list(UserSession, dimensionality_reduction_response$sourceExperiment))
            )
        }
    )
}

# determine analysis type
get_specific_analysis_type <- function(dimensionality_reduction_response)
{
    if(is.null(dimensionality_reduction_response$type)) return("viSNE")
    return(switch(dimensionality_reduction_response$type, "TsneCudaAnalysis"="tSNE-CUDA", "OptSneAnalysis"="opt-SNE", "UmapAnalysis"="UMAP"))
}

# Converts population selections output -> population selections dataframe
create_population_selections <- function(population_selections_output)
{
    population_selections_list <- list()

    # for each population
    for (population in population_selections_output)
    {
        # Create a dataframe
        temp_data <- do.call(rbind.data.frame, population[[4]])
        temp_data["name"] <- lapply(temp_data["name"], as.character) # Convert 'names' as factors -> characters
        # Add population ID and name to population-file selection
        temp_data$populationId <- apply(temp_data, 1, function(row) population[[1]])
        temp_data$populationName <- apply(temp_data, 1, function(row) population[[2]])
        population_selections_list <- c(population_selections_list, list(temp_data))
    }

    # Combine and return one dataframe of fold change group data
    population_selections <- do.call(rbind, population_selections_list)
    return(if (nrow(population_selections) != 0) population_selections
           else data.frame(id=integer(),
                           name=character(),
                           samplingCount=integer(),
                           eventCount=integer(),
                           populationId=integer(),
                           populationName=character(), stringsAsFactors=FALSE))
}


# Convert population selections dataframe -> population selections list for update
population_selections_dataframe_to_list <- function(population_selections_dataframe)
{
    # Check if empty population_selections_dataframe
    if (nrow(population_selections_dataframe) == 0)
    {
        return(list())
    }

    # Dataframe -> list with population[fcs_files]
    population_selections_list <- list()
    for (x in seq(nrow(population_selections_dataframe)))
    {
        population_selections_list[[as.character(population_selections_dataframe$populationId[[x]])]]$fcsFiles <- c(
            population_selections_list[[as.character(population_selections_dataframe$populationId[[x]])]]$fcsFiles,
            list(list(id=population_selections_dataframe$id[[x]])
            )
        )
    }

    # population[fcs_files] -> [population, selected, fcs_files]
    population_selections <- list()
    for (population in names(population_selections_list))
    {
        population_selections <- c(population_selections, list(list(id=as.integer(population), selected=TRUE, fcsFiles=population_selections_list[[population]]$fcsFiles)))
    }

    return(population_selections)
}

