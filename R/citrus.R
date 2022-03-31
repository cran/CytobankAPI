# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' CITRUS Endpoints
#'
#' Interact with CITRUS advanced analyses using these endpoints.
#' @name citrus
#' @param citrus Cytobank CITRUS object
#' @param citrus_id integer representing a CITRUS ID
#' @param citrus_name character representing a new CITRUS name
#' @param directory character representing a specific directory to which the file will be downloaded (optional ending directory slash), if left empty, the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- citrus.list, citrus.run, citrus.status : \code{("default", "raw")}}
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#'
#' # cyto_citrus refers to a CITRUS object that is created from CITRUS endpoints
#' #   examples: citrus.new, citrus.show (see details section for more)
#' }
NULL


######################
# CITRUS class methods
######################


setGeneric("citrus.copy_settings", function(UserSession, citrus, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "CITRUS", possible_outputs=c("raw"))

    standardGeneric("citrus.copy_settings")
})
#' @rdname citrus
#' @aliases citrus.copy_settings
#'
#' @details \code{citrus.copy_settings} Copy CITRUS advanced analysis settings from an experiment and returns a CITRUS object.
#' @examples \dontrun{citrus.copy_settings(cyto_session, citrus=cyto_citrus)
#' }
#' @export
setMethod("citrus.copy_settings", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, output="default", timeout=UserSession@short_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/advanced_analyses/citrus/", citrus@citrus_id, "/copy_settings", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "CITRUS")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "CITRUS"))
    }
})


setGeneric("citrus.delete", function(UserSession, citrus, timeout=UserSession@short_timeout)
{
    standardGeneric("citrus.delete")
})
#' @rdname citrus
#' @aliases citrus.delete
#'
#' @details \code{citrus.delete} Delete a CITRUS advanced analysis from an experiment.
#' @examples \dontrun{citrus.delete(cyto_session, citrus=cyto_citrus)
#' }
#' @export
setMethod("citrus.delete", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/advanced_analyses/citrus/", citrus@citrus_id, sep=""),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "experiments")
    }

    return(paste("CITRUS (ID=", citrus@citrus_id, ") successfully deleted.", sep=""))
})


setGeneric("citrus.download", function(UserSession, citrus, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("citrus.download")
})
#' @rdname citrus
#' @aliases citrus.download
#'
#' @details \code{citrus.download} Download a CITRUS analysis from an experiment.
#' @examples \dontrun{# Download a CITRUS analysis to the current working directory
#' citrus.download(cyto_session, citrus)
#'
#' # Download a CITRUS analysis to a new directory
#' citrus.download(cyto_session, citrus, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("citrus.download", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, directory=getwd(), timeout=UserSession@long_timeout)
{
    if (is.na(citrus@attachment_id))
    {
        stop(
            sprintf(
                paste("Cytobank API 'CITRUS' request failed [client]\n    Must provide a valid CITRUS attachment ID, cannot be NA", sep="")
            ),
            call. = FALSE
        )
    }

    temp_directory <- directory_file_join(directory, "tmp.part")

    # resp <- GET(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/attachments/", citrus@attachment_id, "/download", sep=""),
    #             add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
    #             write_disk(temp_directory, overwrite=TRUE),
    #             timeout(timeout)
    # )

    baseURL = get_base_url(UserSession)

    citrus_info <- attachments.show(UserSession, citrus@source_experiment, citrus@attachment_id)
    file_hashkey <- unlist(citrus_info$uniqueHash)
    file_name <- unlist(citrus_info$filename)
    file_type <- unlist(citrus_info$type)

    resp <- GET(paste(baseURL,'/download/url?', "experimentId=", citrus@source_experiment, "&hashKey=", file_hashkey,
                      "&fileName=", utils::URLencode(file_name),
                      "&fileType=",determine_file_type(file_type), sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token))
    )

    download_status<-utils::download.file(url=parse(resp)$downloadUrl,
                                          destfile=file.path(directory,file_name),
                                          method = 'auto', quiet = FALSE)


    if(download_status!=0){
        print('Can not download the file.')
        return(FALSE)
    }else{
        print(paste('File has been downloaded and saved to: ',file.path(directory,file_name),sep=""))
        return(TRUE)
    }

    if (http_error(resp))
    {
        error_parse(resp, "citrus")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("citrus.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("citrus.list")
})
#' @rdname citrus
#' @aliases citrus.list
#'
#' @details \code{citrus.list} List all CITRUS advanced analyses from an experiment. Outputs a dataframe [default] or list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all CITRUS advanced analyses with all fields present
#' citrus.list(cyto_session, 22)
#'
#' # Raw list of all CITRUS advanced analyses with all fields present
#' citrus.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("citrus.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "CITRUS", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/citrus", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "CITRUS")$citrus))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "CITRUS"))
    }
})


setGeneric("citrus.new", function(UserSession, experiment_id, citrus_name, timeout=UserSession@long_timeout)
{
    standardGeneric("citrus.new")
})
#' @rdname citrus
#' @aliases citrus.new
#'
#' @details \code{citrus.new} Create a new CITRUS advanced analysis from an experiment and returns a CITRUS object.
#' @examples \dontrun{citrus.new(cyto_session, 22, citrus_name="My new CITRUS analysis")
#' }
#' @export
setMethod("citrus.new", signature(UserSession="UserSession"), function(UserSession, experiment_id, citrus_name, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/citrus/", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(name=citrus_name),
                 encode="json",
                 timeout(timeout)
    )

    return(create_citrus_object(UserSession, parse(resp, "CITRUS")))
})


setGeneric("citrus.rename", function(UserSession, citrus, citrus_name, timeout=UserSession@short_timeout)
{
    standardGeneric("citrus.rename")
})
#' @rdname citrus
#' @aliases citrus.rename
#'
#' @details \code{citrus.rename} Rename a CITRUS advanced analysis from an experiment and returns a CITRUS object.
#' @examples \dontrun{citrus.rename(cyto_session, citrus=cyto_citrus, citrus_name="My updated CITRUS name")
#' }
#' @export
setMethod("citrus.rename", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, citrus_name, timeout=UserSession@short_timeout)
{
    resp <- PUT(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/advanced_analyses/citrus/", citrus@citrus_id, "/rename", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(name=citrus_name),
                encode="json",
                timeout(timeout)
    )

    citrus@name <- parse(resp, "CITRUS")$citrus$name
    return(citrus)
})


setGeneric("citrus.run", function(UserSession, citrus, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("citrus.run")
})
#' @rdname citrus
#' @aliases citrus.run
#'
#' @details \code{citrus.run} Run a CITRUS advanced analysis from an experiment.
#' @examples \dontrun{citrus.run(cyto_session, citrus=cyto_citrus)
#' }
#' @export
setMethod("citrus.run", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "CITRUS", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/advanced_analyses/citrus/", citrus@citrus_id, "/run", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "CITRUS")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "CITRUS"))
    }
})


setGeneric("citrus.show", function(UserSession, experiment_id, citrus_id, timeout=UserSession@short_timeout)
{
    standardGeneric("citrus.show")
})
#' @rdname citrus
#' @aliases citrus.show
#'
#' @details \code{citrus.show} Show CITRUS advanced analysis details from an experiment and returns a CITRUS object.
#' @examples \dontrun{citrus.show(cyto_session, 22, citrus_id=2)
#' }
#' @export
setMethod("citrus.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, citrus_id, timeout=UserSession@short_timeout)
{
    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/citrus/", citrus_id, "?include_settings=1", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    return(create_citrus_object(UserSession, parse(resp, "CITRUS")))
})


setGeneric("citrus.status", function(UserSession, citrus, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("citrus.status")
})
#' @rdname citrus
#' @aliases citrus.status
#'
#' @details \code{citrus.status} Show the status of a CITRUS advanced analysis from an experiment.
#' @examples \dontrun{citrus.status(cyto_session, citrus=cyto_citrus)
#' }
#' @export
setMethod("citrus.status", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "CITRUS", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/advanced_analyses/citrus/", citrus@citrus_id, "/status", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "CITRUS")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "CITRUS"))
    }
})


setGeneric("citrus.update", function(UserSession, citrus, timeout=UserSession@long_timeout)
{
    standardGeneric("citrus.update")
})
#' @rdname citrus
#' @aliases citrus.update
#'
#' @details \code{citrus.update} Update a CITRUS advanced analysis from an experiment and returns the new CITRUS object.
#' @examples \dontrun{citrus.update(cyto_session, citrus=cyto_citrus)
#' }
#' @export
setMethod("citrus.update", signature(UserSession="UserSession", citrus="CITRUS"), function(UserSession, citrus, timeout=UserSession@long_timeout)
{
    # Convert file grouping dataframe -> list readable by update endpoint
    file_group_names <- unique(citrus@file_grouping$group_name[citrus@file_grouping$group_name != "Unassigned"]) # Must remove "Unassigned" from file group names
    file_grouping <- file_grouping_dataframe_to_list(citrus@file_grouping, file_group_names)

    if (length(citrus@channels) && is.character(citrus@channels[[1]]))
    {
        citrus@channels <- as.list(helper.channel_ids_from_long_names(citrus@.available_channels, citrus@channels))
    }

    resp <- PUT(paste(UserSession@site, "/experiments/", citrus@source_experiment, "/advanced_analyses/citrus/", citrus@citrus_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(name=citrus@name,
                          options=list(
                              gateSetId=citrus@population_id,
                              clusteringChannels=citrus@channels,
                              fileGroupNames=file_group_names,
                              fileGrouping=file_grouping,
                              compensationId=citrus@compensation_id,
                              associationModels=citrus@association_models,
                              clusteringCharacterization=citrus@cluster_characterization,
                              statisticsChannels=citrus@statistics_channels,
                              eventSamplingMethod=citrus@event_sampling_method,
                              eventsPerFile=citrus@events_per_file,
                              minimumClusterSize=citrus@minimum_cluster_size,
                              crossValidationFolds=citrus@cross_validation_folds,
                              falseDiscoveryRate=citrus@false_discovery_rate,
                              normalizeScales=citrus@normalize_scales,
                              plotTheme=citrus@plot_theme
                          )
                ),
                encode="json",
                timeout(timeout)
    )

    return(create_citrus_object(UserSession, parse(resp, "CITRUS")))
})


##########################
# CITRUS HELPER FUNCTIONS
##########################


##########
# PRIVATE
##########


# Create CITRUS object from CITRUS json response
create_citrus_object <- function(UserSession, citrus_response)
{
    fcs_files <- fcs_files.list(UserSession, citrus_response$citrus$sourceExperiment)

    return(
        new("CITRUS",
            association_models=citrus_response$citrus$settings$associationModels,
            attachment_id=if (!is.integer(citrus_response$citrus$settings$attachmentId)) NA_integer_ else citrus_response$citrus$settings$attachmentId,
            channels=citrus_response$citrus$settings$clusteringChannels,
            citrus_id=citrus_response$citrus$id,
            cluster_characterization=citrus_response$citrus$settings$clusteringCharacterization,
            compensation_id=citrus_response$citrus$settings$compensationId,
            cross_validation_folds=citrus_response$citrus$settings$crossValidationFolds,
            event_sampling_method=citrus_response$citrus$settings$eventSamplingMethod,
            events_per_file=citrus_response$citrus$settings$eventsPerFile,
            false_discovery_rate=citrus_response$citrus$settings$falseDiscoveryRate,
            file_grouping=create_file_grouping(citrus_response$citrus$settings$fileGrouping, fcs_files$filename, citrus_response$citrus$settings$fileGroupNames),
            minimum_cluster_size=citrus_response$citrus$settings$minimumClusterSize,
            name=citrus_response$citrus$name,
            normalize_scales=citrus_response$citrus$settings$normalizeScales,
            plot_theme=citrus_response$citrus$settings$plotTheme,
            population_id=citrus_response$citrus$settings$gateSetId,
            source_experiment=citrus_response$citrus$sourceExperiment,
            statistics_channels=citrus_response$citrus$settings$statisticsChannels,
            status=citrus_response$citrus$status,
            .available_channels=panels.list(UserSession, citrus_response$citrus$sourceExperiment),
            .available_files=fcs_files,
            .available_populations=populations.list(UserSession, citrus_response$citrus$sourceExperiment))
    )
}


# Converts file grouping output -> file grouping dataframe
create_file_grouping <- function(file_grouping_output, fcs_file_names, file_group_names)
{
    # Static creation of file grouping dataframe
    file_grouping_length <- length(file_grouping_output)
    file_grouping <- data.frame(id=integer(file_grouping_length),
                                name=character(file_grouping_length),
                                group_name=character(file_grouping_length),
                                stringsAsFactors=FALSE)

    # Fill dataframe data in
    for (file in seq(file_grouping_length))
    {
        file_grouping[file,] <- list(file_grouping_output[[file]][[1]],
                                     fcs_file_names[[file]],
                                     if (file_grouping_output[[file]][[2]]+1 == 0) "Unassigned"
                                     else file_group_names[[file_grouping_output[[file]][[2]]+1]])
    }

    return(file_grouping)
}


file_grouping_dataframe_to_list <- function(file_grouping_dataframe, file_group_names)
{
    file_grouping <- list()

    for (x in seq(nrow(file_grouping_dataframe)))
    {
        file_grouping <- c(file_grouping, list(list(
            file_grouping_dataframe$id[[x]],
            if (file_grouping_dataframe$group_name[[x]] == "Unassigned") -1
            else match(file_grouping_dataframe$group_name[[x]], file_group_names)-1
            )))
    }

    return(file_grouping)
}

