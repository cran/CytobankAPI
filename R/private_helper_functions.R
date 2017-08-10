# Internal helper functions

##########
# PARSING
##########

# Default parsing for JSON response
parse <- function(resp, endpoint)
{
    if (http_type(resp) != "application/json")
    {
        stop("API did not return json", call. = FALSE)
    }

    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector=FALSE)

    if (http_error(resp))
    {
        # Different parsing mechanism because there is no informative output for statistics endpoint
        if (endpoint == "statistics")
        {
            stop(
                sprintf(
                    paste("Cytobank API '", endpoint, "' request failed [%s]\n    %s\n", sep=""),
                    status_code(resp),
                    parsed$msg
                ),
                call. = FALSE
            )
        }
        else
        {
            stop(
                sprintf(
                    paste("Cytobank API '", endpoint, "' request failed [%s]\n    %s\n", sep=""),
                    status_code(resp),
                    parsed$errors
                ),
                call. = FALSE
            )
        }
    }

    return(parsed)
}


# Error parsing for JSON response dealing with downloads
error_parse <- function(resp, endpoint)
{
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector=FALSE)
    stop(
        sprintf(
            paste("Cytobank API '", endpoint, "' request failed [%s]\n    %s\n", sep=""),
            status_code(resp),
            parsed$errors
        ),
        call. = FALSE
    )
}


# Convert a parsed endpoint list -> Cytobank dataframe
cyto_dataframe <- function(parsed_list)
{
    return(
        setNames(
            data.frame(
                # transpose of the matrix
                t(matrix(unlist(rbind(
                    # lapply to set NULL -> NA
                    lapply(
                        lapply(parsed_list, rbind),
                        function(x) ifelse(x == "NULL", NA, x))), recursive=FALSE),
                    ncol=length(parsed_list))), stringsAsFactors=FALSE),
            labels(parsed_list[[1]]))
    )
}


# Convert a compensation list to dataframe -- transforms dataframe to what is seen in Cytobank GUI
compensation_to_dataframe <- function(compensation)
{
    channel_names <- list()
    compensation_values <- list()
    for (channel_index in 1:length(compensation[[1]]$sources))
    {
        channel_names <- c(channel_names, compensation[[1]]$sources[[channel_index]]$shortName)
        compensation_values[[channel_index]] <- list()
    }

    for (channel_x_index in 1:length(compensation[[1]]$compensationMatrix))
    {
        for (channel_y_index in 1:length(compensation[[1]]$compensationMatrix[[channel_x_index]]))
        {
            compensation_values[[channel_y_index]] <- c(compensation_values[[channel_y_index]], compensation[[1]]$compensationMatrix[[channel_x_index]][[channel_y_index]])
        }
    }

    compensation_matrix <- data.frame(matrix(unlist(compensation_values), length(channel_names)), row.names=channel_names)
    colnames(compensation_matrix) <- channel_names
    return(compensation_matrix)
}


# Convert a compensation list to dataframe -- leaves dataframe as data comes in (flipped diagonally from what is seen in Cytobank GUI)
# compensation_to_dataframe <- function(compensation)
# {
#     channel_names <- list()
#     compensation_values <- list()
#
#     for (x in 1:length(compensation[[1]]$sources))
#     {
#         channel_names <- c(channel_names, compensation[[1]]$sources[[generic_list_index]]$name)
#     }
#
#     compensation_matrix <- data.frame(matrix(unlist(compensation[[1]]$compensationMatrix), length(channel_names)), row.names=channel_names)
#     colnames(compensation_matrix) <- channel_names
#     return(compensation_matrix)
# }


##########
# UTILITY
##########

# Download file helper function
rename_temp_file <- function(resp, directory_path)
{
    temp_file <- directory_file_join(directory_path, "tmp.part")
    file_name <- directory_file_join(directory_path, gsub("\\\"", "", gsub("^.*=", "", headers(resp)$`content-disposition`)))
    file.rename(temp_file, file_name)
    return(file_name)
}


# Parameter extensions builder -- Used for statistics
extension_builder <- function(extension, value)
{
    # if (is.na(value))
    # {
    #     return("")
    # }
    return (paste("&", extension, "=", value, sep=""))
}

array_extension_builder <- function(extension, values_array)
{
    text <- ""

    for (value in values_array)
    {
        text <- paste(text, extension_builder(extension, value), sep="")
    }

    return(text)
}


# File path helper function -- Ensure path ends in "directory separator"
directory_file_join <- function(directory_path, file_name)
{
    if (regexpr("[^/\\]$", directory_path) != -1)
    {
        directory_path <- file.path(directory_path, file_name)
    }
    else
    {
        directory_path <- paste(directory_path, file_name, sep="")
    }
}


# Compile IDs/names list
filter_ids_names <- function(generic_list, name_keyword)
{
    endpoint <- names(generic_list)
    # Include 'Ungated' set IDs for populations, otherwise, initialize empty
    info_list <- if (endpoint != "populations") list() else list(list(id=0, name="Ungated"))
    reference_list <- list()

    for (generic_list_index in 1:length(generic_list[[endpoint]]))
    {
        # Generic  -- doesn't work for Panels and Scales
        if (endpoint != "panels")
        {
            info_list[[length(info_list)+1]] <- add_generic_id_name(generic_list, name_keyword, endpoint, generic_list_index)
        }
        else
        {
            # Panels -- Compile short_channel_id/names list for panels differently
            for (channel_index in 1:length(generic_list[[endpoint]][[generic_list_index]]$channels))
            {
                channel <- generic_list[[endpoint]][[generic_list_index]]$channels[[channel_index]]

                id_name <- build_id_name_item(channel, name_keyword)
                files <- sort(unlist(generic_list[[endpoint]][[generic_list_index]]$fcsFiles))

                # Check the reference list to see if the short_channel_id/name combination is already present
                #   If it isn't, add the short_channel_id/name/files combination to the reference list and add it to the info_list
                #   If it is, add the files associated with that short_channel_id/name combination
                if (!is.element(list(id_name), reference_list))
                {
                    reference_list[[length(reference_list)+1]] <- id_name
                    info_list[[length(info_list)+1]] <- list(id=id_name[["id"]], name=id_name[["name"]], files=files)
                }
                else
                {
                    for (element in 1:length(info_list))
                    {
                        # Match short_channel_id/name combination
                        if (info_list[[element]][["id"]] == id_name[["id"]] && info_list[[element]][["name"]] == id_name[["name"]])
                        {
                            # Only add unique fcs files
                            info_list[[element]][["files"]] <- sort(unique(c(info_list[[element]][["files"]], files)))
                            break
                        }
                    }
                }
            }
        }
    }

    return(info_list)
}

add_generic_id_name <- function(generic_list, name_keyword, endpoint, generic_list_index)
{
    # Use gate set IDs for populations, rather than the actual population ID, otherwise, get normal ID
    id <- if (endpoint != "populations") generic_list[[endpoint]][[generic_list_index]]$id else if (length(generic_list[[endpoint]])) generic_list[[endpoint]][[generic_list_index]]$gateSetId else c()
    name <- if (length(generic_list[[endpoint]])) generic_list[[endpoint]][[generic_list_index]][[name_keyword]] else c()
    return(list(id=id, name=name))
}

# Build single id/name item to use for comparison to reference_list
build_id_name_item <- function(channel, name_keyword)
{
    id <- channel$normalizedShortNameId
    if (name_keyword == "ids_names" || name_keyword == "ids_names_common_in_all")
    {
        return(list(id=id, name=paste(channel$shortName, channel$longName, sep="::")))
    }
    else if (name_keyword == "ids_names_short")
    {
        return(list(id=id, name=channel$shortName))
    }
    else if (name_keyword == "ids_names_long")
    {
        return(list(id=id, name=channel$longName))
    }
}


#############
# VALIDATION
#############

# Check if path is either a directory or file
is_directory <- function(path)
{
    is_path <- file.info(path)$isdir
    if (!is.na(is_path) && is_path == TRUE)
    {
        return(TRUE)
    }
    else
    {
        return(FALSE)
    }
}


output_check <- function(output, endpoint, possible_outputs, panels=FALSE, statistics=FALSE)
{
    if (is.element(output, c("default", possible_outputs)))
    {
        return(TRUE)
    }
    # Base error message for all ouput checks
    error_message <- paste(paste("Cytobank API '", endpoint, "' request failed [client]", sep=""),
                           "    Invalid output format. Please specify one from the list:",
                           "        - default: default output for the specific endpoint",
                           sep="\n")

    base_dataframe <- "        - dataframe: dataframe representation of the output"

    specific_output_errors <- list(dataframe = base_dataframe,
                                   # Statistics specific dataframe error messages
                                   dataframe_col = paste(base_dataframe,
                                                         "             - dataframe_col (general statistics only): proliferate channel statistics via columns",
                                                         sep="\n"),
                                   dataframe_row = "             - dataframe_row (general statistics only): proliferate channel statistics via rows",
                                   raw = "        - raw: raw list with all fields present"
    )
    for (output in possible_outputs)
    {
        error_message <- paste(error_message,
                               specific_output_errors[[output]],
                               sep="\n")
    }
    stop(
        sprintf(
            error_message
        ),
        call. = FALSE
    )
}


update_check <- function(endpoint_dataframe, endpoint)
{
    if (nrow(endpoint_dataframe) > 1)
    {
        stop(
            sprintf(
                paste("Cytobank API '", endpoint, "' request failed [client]\n    Must provide a single row ", endpoint, " dataframe\n", sep="")
            ),
            call. = FALSE
        )
    }
}

