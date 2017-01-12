#' Statistic Endpoints
#'
#' Interact with statistic endpoints. Gather data about event counts and general channel statistics. Create dataframes of statistics to help with visualization and downstream analysis.
#' @name statistics
#' @param channels vector/list of integers representing a list of channel IDs
#' @param compensation_id integer representing a \link[=compensations]{compensation} ID (use -2 for file-internal compensation, -1 for uncompensated)
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param experiment_version integer representing an experiment version, must be set to the current version of the experiment, which can be seen as the \strong{version} attribute returned from a call to the \link[=experiments]{Show Experiment Details} endpoint
#' @param fcs_files vector/list of integers representing a list of \link[=fcs_files]{FCS file} IDs
#' @param gate_version integer representing an experiment gate version, an integer of -1 corresponds to the state of \link{gates} and \link{populations} in the gating interface. Faster performance can be achieved by using the maximum gate version from the experiment \href{https://support.cytobank.org/hc/en-us/articles/205399487-The-Apply-and-Apply-and-Return-buttons-and-gate-versioning}{(learn more about gate versions)}. Maximum gate version can be seen as the \strong{gateVersion} attribute returned from a call to the \link[=experiments]{Show Experiment Details} endpoint \strong{[optional]}
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- statistics.event_counts: \code{("default" [default], "dataframe")}}\cr
#' \emph{- statistics.general: \code{("default", "dataframe_col", "dataframe_row")}}\cr
#' \emph{- \code{dataframe}: converts the output to a dataframe for the event count statistics}\cr
#' \emph{- \code{dataframe_col}: for statistics data on multiple channels, proliferate channel statistics as columns}\cr
#' \emph{- \code{dataframe_row}: for statistics data on multiple channels, proliferate channel statistics as rows}
#' @param populations vector/list of integers representing a list of population IDs to calculate statistics for. This is the \strong{gateSetId} attribute of a \link[=populations]{population} object. Another term for a population is a "gate set". If not specified, all population statistics will be fetched \strong{[optional]}
#' @param timeout integer representing the request timeout time in seconds
#' @param UserSession Cytobank UserSession object
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("statistics.event_counts", function(UserSession, experiment_id, gate_version=-1, experiment_version, compensation_id, fcs_files, populations=c(), output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("statistics.event_counts")
})
#' @rdname statistics
#' @aliases statistics.event_counts
#'
#' @details \code{statistics.event_counts} Get event count statistics from an experiment. In the absence of channel information, only event count data are returned. If only event count data are needed, this approach can be faster than retrieving all statistics by avoiding unnecessary computation.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("full" [default], "dataframe")}}\cr
#' \emph{- \code{dataframe}: converts the output to a dataframe for the event count statistics}\cr
#' @examples \donttest{statistics.event_counts(cyto_session, 22, experiment_version=22, compensation_id=-2,
#'   fcs_files=c(12, 13, 14), channels=c(53, 54, 55), populations=c(32, 33, 34))
#' }
#' @export
setMethod("statistics.event_counts", signature(UserSession="UserSession"), function(UserSession, experiment_id, gate_version=-1, experiment_version, compensation_id, fcs_files, populations=c(), output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "statistics", possible_outputs=c("dataframe"))

    resp <- GET(paste(UserSession@site, "/statistics?experimentId=", experiment_id,
                      "&gateVersion=", gate_version,
                      "&experimentVersion=", experiment_version,
                      "&compensationId=", compensation_id,
                      array_extension_builder("fcsFileIds", fcs_files),
                      array_extension_builder("gateSetIds", populations),
                      sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "dataframe")
    {
        # Get all necessary information to return a statistics dataframe
        statistics <- parse(resp, "statistics")
        files <- filter_ids_names(fcs_files.list(UserSession, experiment_id, output="raw"), "filename")
        populations <- filter_ids_names(populations.list(UserSession, experiment_id, output="raw"), "name")
        return(statistics_to_dataframe_proliferate_channel_statistics_by_col(statistics, files, populations, c()))
    }

    # Different parsing mechanism because there is no informative output for statistics endpoint
    return(parse(resp, "statistics"))
})


setGeneric("statistics.general", function(UserSession, experiment_id, gate_version=-1, experiment_version, compensation_id, fcs_files, channels, populations=c(), output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("statistics.general")
})
#' @rdname statistics
#' @aliases statistics.general
#'
#' @details \code{statistics.general} Get a batch of common statistics for specific channels on populations from an experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("full" [default], "dataframe_col", "dataframe_row")}}\cr
#' \emph{- \code{dataframe_col}: for statistics data on multiple channels, proliferate channel statistics as columns}\cr
#' \emph{- \code{dataframe_row}: for statistics data on multiple channels, proliferate channel statistics as rows}
#' @examples \donttest{# Full list with all fields present
#' statistics.general(cyto_session, 22, experiment_version=22, compensation_id=-2,
#'   fcs_files=c(12, 13, 14), channels=c(53, 54, 55), populations=c(32, 33, 34))
#'
#' # Statistics list transformed into a dataframe, proliferating channel statistics by column
#' statistics.general(cyto_session, 22, experiment_version=22, compensation_id=-2,
#'   fcs_files=c(12, 13, 14), channels=c(53, 54, 55), populations=c(32, 33), output="dataframe_col")
#'
#' # Statistics list transformed into a dataframe, proliferating channel statistics by row
#' statistics.general(cyto_session, 22, experiment_version=22, compensation_id=-2,
#'   fcs_files=c(12, 13, 14), channels=c(53, 54, 55), populations=c(32, 33), output="dataframe_row")
#'
#' # Statistics list transformed into a dataframe, using helper functions (names_to_ids)
#' # Get FCS files that match 'pbmc' in their filename
#' fcs_files <- fcs_files.list(cyto_session, 22)
#' fcs_files <- fcs_files[,c("id", "filename")]
#' fcs_files <- unlist(fcs_files$id[grep("pbmc", fcs_files$filename)])
#'
#' # Get channels that match 'pp' or 'pStat' as their longName
#' channels <- panels.list(cyto_session, 22)$`Panel 1`$channels
#' channels <- channels[,c("normalizedShortNameId", "shortName", "longName")]
#' channels <- channels$normalizedShortNameId[grep("pp.*|pStat.*", channels$longName)]
#'
#' # Get populations that match 'CD' as their population name
#' populations <- populations.list(cyto_session, 22)
#' populations <- populations[,c("gateSetId", "name")]
#' populations <- populations$id[grep("CD.*", populations$name)]
#'
#' statistics.general(cyto_session, 22, experiment_version=61, compensation_id=-2,
#'   fcs_files=fcs_files, channels=channels, populations=populations, output="dataframe_row")
#' }
#' @export
setMethod("statistics.general", signature(UserSession="UserSession"), function(UserSession, experiment_id, gate_version=-1, experiment_version, compensation_id, fcs_files, channels, populations=c(), output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "statistics", possible_outputs=c("dataframe_col", "dataframe_row"))

    resp <- GET(paste(UserSession@site, "/statistics?experimentId=", experiment_id,
                      "&gateVersion=", gate_version,
                      "&experimentVersion=", experiment_version,
                      "&compensationId=", compensation_id,
                      array_extension_builder("fcsFileIds", fcs_files),
                      array_extension_builder("channelIds", channels),
                      array_extension_builder("gateSetIds", populations),
                      sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (is.element(output, c("dataframe_col", "dataframe_row")))
    {
        # Get all necessary information to return a statistics dataframe
        statistics <- parse(resp, "statistics")
        files <- filter_ids_names(fcs_files.list(UserSession, experiment_id, output="raw"), "filename")
        populations <- filter_ids_names(populations.list(UserSession, experiment_id, output="raw"), "name")
        channels <- filter_ids_names(panels.list(UserSession, experiment_id, output="raw"), "ids_names")
        if (output == "dataframe_col")
        {
            return(statistics_to_dataframe_proliferate_channel_statistics_by_col(statistics, files, populations, channels))
        }
        else if (output == "dataframe_row")
        {
            return(statistics_to_dataframe_proliferate_channel_statistics_by_row(statistics, files, populations, channels))
        }
    }

    # Different parsing mechanism because there is no informative output for statistics endpoint
    return(parse(resp, "statistics"))
})


########################################
# STATISTICS DATAFRAME HELPER FUNCTIONS
########################################


##########
# PRIVATE
##########


# Create a dataframe for row proliferating channel stats
create_statistics_dataframe <- function(num_channels)
{
    labels <- c("experimentId", "gateVersion", "compensationId", "fcsFileId", "fcsFileName", "gateSetId", "gateSetName", "eventCounts")
    stat_labels <- c("channelShortNameId", "shortChannelName", "longChannelName", "minimums", "maximums", "means", "medians", "variances", "standardDeviations", "secondPercentiles", "ninetyEighthPercentiles")

    if (num_channels > 0)
    {
        for (stats in 1:num_channels)
        {
            labels <- c(labels, stat_labels)
        }
    }

    return(data.frame(matrix(vector(), 0, 8+(11*num_channels),
                             dimnames=list(c(), labels))))
}


# Returns a vector of the general file/population statistics, neglecting individual channel statistics, representing the beginning of a row
add_file_population_statistics <- function(file_population_statistics, fcs_files_list, populations_list)
{
    new_row <- c(file_population_statistics[["experimentId"]],
                 file_population_statistics[["gateVersion"]],
                 file_population_statistics[["compensationId"]])

    file_name <- get_name_from_list(file_population_statistics, fcs_files_list, "fcsFileId")
    population_name <- get_name_from_list(file_population_statistics, populations_list, "gateSetId")

    new_row <- c(new_row, c(file_population_statistics[["fcsFileId"]], file_name,
                            file_population_statistics[["gateSetId"]], population_name,
                            file_population_statistics[["eventCount"]]))

    return(new_row)
}


# Get name from generic list
get_name_from_list <- function(file_population_statistics, generic_list, name_keyword, channel_index=NA)
{
    if (name_keyword != "panel")
    {
        for (element in generic_list)
        {
            if (element[["id"]] == file_population_statistics[[name_keyword]])
            {
                return(element[["name"]])
            }
        }
    }
    # Get name for panel differently due to channel_name/panel/file combination
    else
    {
        for (element in generic_list)
        {
            # Find the specific channel name (short::long) that pertains to the specific file (different short/long channel combinations could mean different panels)
            if (element[["id"]] == file_population_statistics[["channelShortNameIds"]][[channel_index]] && is.element(file_population_statistics[["fcsFileId"]], element[["files"]]))
            {
                return(unlist(strsplit(element[["name"]], "::")))
            }
        }
    }
}


# Create statistics for file/population combinations row by row (proliferate channel stats by column)
statistics_to_dataframe_proliferate_channel_statistics_by_col <- function(statistics, fcs_files_list, populations_list, channels_list)
{
    num_channels <- if (length(channels_list) > 0) length(statistics[[1]][["channelShortNameIds"]]) else 0
    short_channel_ids <- unlist(statistics[[1]][["channelShortNameIds"]])
    statistics_dataframe <- create_statistics_dataframe(num_channels)

    # Iterate through each file/population combination statistics set
    for (file_population in 1:length(statistics))
    {
        file_population_statistics <- statistics[[file_population]]
        new_row <- add_file_population_statistics(file_population_statistics, fcs_files_list, populations_list)
        # Iterate through channels for a specific file/population combination
        if (num_channels > 0)
        {
            for (channel_index in 1:length(short_channel_ids))
            {
                # Add channel data
                short_channel_id <- short_channel_ids[[channel_index]]
                channel_name <- get_name_from_list(file_population_statistics, channels_list, "panel", channel_index)
                new_row <- c(new_row, short_channel_id, channel_name[1], channel_name[2])
                # Add each statistic based off of the short_channel_id
                for (statistic in file_population_statistics[8:15])
                {
                    new_row <- c(new_row, statistic[[toString(short_channel_id)]])
                }
            }
        }

        statistics_dataframe[nrow(statistics_dataframe)+1,] <- new_row
    }

    # # Convert numeric columns to computable values
    statistics_dataframe[, 1:4] <- sapply(statistics_dataframe[, 1:4], as.integer)
    statistics_dataframe[, 5] <- sapply(statistics_dataframe[, 5], as.factor)
    statistics_dataframe[, 6] <- sapply(statistics_dataframe[, 6], as.integer)
    statistics_dataframe[, 8] <- sapply(statistics_dataframe[, 8], as.integer)
    if (num_channels > 0)
    {
        for (i in 0:(num_channels-1))
        {
            statistics_dataframe[, (9+(11*i))] <- sapply(statistics_dataframe[, (9+(11*i))], as.integer)
            statistics_dataframe[, (10+(11*i)):(11+(11*i))] <- lapply(statistics_dataframe[, (10+(11*i)):(11+(11*i))], as.factor)
            statistics_dataframe[, (12+(11*i)):(19+(11*i))] <- sapply(statistics_dataframe[, (12+(11*i)):(19+(11*i))], as.numeric)
        }
    }

    return(statistics_dataframe)
}

# Proliferate by row
statistics_to_dataframe_proliferate_channel_statistics_by_row <- function(statistics, fcs_files_list, populations_list, channels_list)
{
    num_channels <- length(statistics[[1]][["channelShortNameIds"]])
    short_channel_ids <- unlist(statistics[[1]][["channelShortNameIds"]])
    statistics_dataframe <- create_statistics_dataframe(1)

    # Iterate through each file/population combination statistics set
    for (file_population in 1:length(statistics))
    {
        file_population_statistics <- statistics[[file_population]]
        # Iterate through channels for a specific file/population combination
        for (channel_index in 1:length(short_channel_ids))
        {
            short_channel_id <- short_channel_ids[[channel_index]]
            new_row <- add_file_population_statistics(file_population_statistics, fcs_files_list, populations_list)
            # Add channel data
            channel_name <- get_name_from_list(file_population_statistics, channels_list, "panel", channel_index)
            new_row <- c(new_row, short_channel_id, channel_name[1], channel_name[2])
            # Add each statistic based off of the short_channel_id
            for (statistic in file_population_statistics[8:15])
            {
                new_row <- c(new_row, statistic[[toString(short_channel_id)]])
            }
            statistics_dataframe[nrow(statistics_dataframe)+1,] <- new_row
        }
    }

    # Convert numeric columns to computable values
    statistics_dataframe[, 1:4] <- sapply(statistics_dataframe[, 1:4], as.integer) # experiment_id, gate_version, compensation_id, fcs_file_id
    statistics_dataframe[, 5] <- sapply(statistics_dataframe[, 5], as.factor) # fcs_file_name
    statistics_dataframe[, 6] <- sapply(statistics_dataframe[, 6], as.integer) # gate_set_id
    statistics_dataframe[, 8:9] <- sapply(statistics_dataframe[, 8:9], as.integer) # event_counts, short_channel_id
    statistics_dataframe[, 10:11] <- lapply(statistics_dataframe[, 10:11], as.factor) # short_channel_name, long_channel_name
    statistics_dataframe[, 12:19] <- sapply(statistics_dataframe[, 12:19], as.numeric) # minimums, maximums, means, medians, variances, standard_deviations, 2nd_percentiles, 98th_percentiles

    return(statistics_dataframe)
}

