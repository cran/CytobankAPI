# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Internal helper functions ----

# PARSING ========

# Default parsing for JSON response ####
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


# Error parsing for JSON response dealing with downloads ####
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


# Convert a parsed endpoint list -> Cytobank dataframe ####
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


# Convert a compensation list to dataframe -- transforms dataframe to what is seen in Cytobank GUI ####
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

# UTILITY ====

# Download file helper function ####
rename_temp_file <- function(resp, directory_path)
{
    temp_file <- directory_file_join(directory_path, "tmp.part")
    file_name <- directory_file_join(directory_path, gsub("\\\"", "", gsub("^.*=", "", headers(resp)$`content-disposition`)))
    file.rename(temp_file, file_name)
    return(file_name)
}


# Parameter extensions builder -- Used for statistics ####
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

# File path helper function -- Ensure path ends in "directory separator" ####
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


# Compile IDs/names list ####
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

# Build single id/name item to use for comparison to reference_list ####
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


# UPLOAD ========


# Generalized file upload function. This function can upload one file per run. ####
file_upload<-function(UserSession, experiment_id, file_path, output = "default", timeout=UserSession@long_timeout, upload_type = 'fcs', drop_data_matrix_start_row = NULL, drop_data_matrix_start_column = NULL)
{

    if(UserSession@user_id==0){
        print("Can't find Cytobank User ID. Please rerun authenticate function using your Cytobank username and password, or API token and Cytobank User ID.")
        return(FALSE)
    }

    if(upload_type == 'drop'){
        if( (is.null(drop_data_matrix_start_row)|is.null(drop_data_matrix_start_column)) ){
            print("Missing input values for data_matrix_start_row or data_matrix_start_column argument")
            return(FALSE)
        }
    }

    baseURL = get_base_url(UserSession)

    resp <- GET(paste(baseURL,'/upload/token?','userId=',UserSession@user_id,'&experimentId=',experiment_id,'&acs=','false',sep = ''),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token))
    )

    parsed_api_return<-parse(resp, "fcs_files")

    tid <- uuid::UUIDgenerate()

    if(upload_type == 'fcs'){
        upload_output <- try(aws.s3::put_object(file = file_path,
                                                headers = c('x-amz-meta-tid'= tid),
                                                key = parsed_api_return$accessKeyId,
                                                secret = parsed_api_return$secretAccessKey,
                                                session_token = parsed_api_return$sessionToken,
                                                region = parsed_api_return$awsRegion,
                                                object = paste('experiments/',as.character(experiment_id),'/',utils::URLencode(basename(file_path)),sep=''),
                                                bucket = parsed_api_return$uploadBucketName,
                                                show_progress = TRUE,
                                                verbose=FALSE,
                                                multipart = FALSE)
        )
    }else if (upload_type == 'attachment'){
        upload_output <- try(aws.s3::put_object(file = file_path,
                                                headers =  list('x-amz-meta-tid' = tid, 'x-amz-meta-attachment' = 'true'),
                                                key = parsed_api_return$accessKeyId,
                                                secret = parsed_api_return$secretAccessKey,
                                                session_token = parsed_api_return$sessionToken,
                                                region = parsed_api_return$awsRegion,
                                                object = paste('experiments/',as.character(experiment_id),'/',utils::URLencode(basename(file_path)),sep=''),
                                                bucket = parsed_api_return$uploadBucketName,
                                                show_progress = TRUE,
                                                verbose=FALSE,
                                                multipart = TRUE)
        )
    }else if (upload_type == 'drop'){
        upload_output <- try(aws.s3::put_object(file = file_path,
                                                headers = list('x-amz-meta-tid' = tid, 'x-amz-meta-drop' = 'true', 'x-amz-meta-startrow' = drop_data_matrix_start_row, 'x-amz-meta-startcol' = drop_data_matrix_start_column),
                                                key = parsed_api_return$accessKeyId,
                                                secret = parsed_api_return$secretAccessKey,
                                                session_token = parsed_api_return$sessionToken,
                                                region = parsed_api_return$awsRegion,
                                                object = paste('experiments/',as.character(experiment_id),'/',utils::URLencode(basename(file_path)),sep=''),
                                                bucket = parsed_api_return$uploadBucketName,
                                                show_progress = TRUE,
                                                verbose=FALSE,
                                                multipart = TRUE)
        )
    }else{
        print('Invalid value for the upload type argument')
        return(FALSE)
    }

    if (inherits(upload_output, "try-error")) {
        print(paste('Can not upload file: ',file_path,"---",upload_output,sep=''))
        return(FALSE)
    }else{
        print('The file has been uploaded into Cytobank.')
        return(TRUE)
    }

}


# QC input file ####
# check input file extension again the string of the required_format.
# determine whether input file exists or not.
check_file<-function(file_path,required_format){
    if(toupper(tools::file_ext(file_path))!=required_format){
        print("The input file does not have the right file extension.")
        return(FALSE)
    }

    if( !(file.exists(file_path)&(file.size(file_path)>0)) ){
        print("The input file does not exist or is empty.")
        return(FALSE)
    }

    notallowed <- paste(c('\\!', '@', '#', '\\$', '%', '\\^', '&', '\\*', '\\{', '\\}',
                          '<', '>', '/', '\\', '\\?', '\\[', '\\]', '\\(', '\\)'),
                        sep = '',
                        collapse = '|')
    if(length(grep(notallowed, basename(file_path)))>0){
        print("The filename contains special characters. Please remove characters other than letters, numbers, spaces, _, -, or . from the filename.")
        return(FALSE)
    }

    # return TRUE if no error
    return(TRUE)
}

# FILE STATUS ========

# Get base URL
get_base_url<-function(UserSession){
    #if(UserSession@site=="https://dev02.cytobank.org/cytobank/api/v1"){
    #    #baseURL="https://3d6cj3nhjh.execute-api.us-west-2.amazonaws.com/public-dev"
    #    baseURL='https://7ni4jp59uf.execute-api.us-east-1.amazonaws.com/public-dev'
    #} else if(UserSession@site=="https://qa-titan.cytobank.org/cytobank/api/v1"){
    #    baseURL="https://cpzavln0hb.execute-api.us-east-1.amazonaws.com/public-dev"
    #}

    splited_string = unlist(strsplit(UserSession@site,'\\.'))
    site_name = paste0(grep('https:\\/\\/.*',splited_string,value = TRUE),'-api',collapse = '')
    end_name = sub('cytobank\\/','',grep('\\/cytobank\\/api',splited_string,value = TRUE))
    baseURL = paste0(c(site_name,splited_string[2],end_name),collapse = '.')
    return(baseURL)
    }

# new file status endpoint ####
check_file_status<-function(UserSession,experiment_id){

    # baseURL = 'https://cpzavln0hb.execute-api.us-east-1.amazonaws.com/public-dev' # Need to be updated before the production release
    baseURL = get_base_url(UserSession)

    file_status <- GET(paste(baseURL,'/experiments/',experiment_id,'/upload_status',sep = ''),
                       add_headers(Authorization=paste("Bearer", UserSession@auth_token))
    )
    parsed_file_status<-parse(file_status, "fcs_files")

    if(parsed_file_status$Count==0){
        print("Can't find any files in the specified experiment! Please make sure the experiment id is correct.")
        return(FALSE)
    }

    extracted_info<-sapply(parsed_file_status$Items,function(x){
        c(basename(x$FileName),x$HashKey,x$ItemId,x$FileType,x$Status)
    })

    extracted_info<-t(extracted_info)
    extracted_info<-as.data.frame(extracted_info,stringsAsFactors=FALSE)
    names(extracted_info)<-c('FileName','HashKey','ItemId','FileType','Status')
    return(extracted_info)

}

# DOWNLOAD ========

# new file download function ####
# file_download<-function(UserSession, experiment_id, file_hashkey, directory=getwd(), timeout=UserSession@long_timeout)
# {
#
#     temp_directory <- directory_file_join(directory, "tmp.part")
#
#     baseURL = get_base_url(UserSession)
#
#     # file_status<-check_file_status(UserSession,experiment_id)
#
#     # file_info<-file_status[c(file_status$HashKey==file_hashkey),,drop=FALSE]
#
#     # if(dim(file_info)[1]>1){
#     #     print("Mutiple files have the same file hash key!")
#     #     return(FALSE)
#     # }
#
#     resp <- GET(paste(baseURL,'/download/url?', "experimentId=", experiment_id, "&hashKey=", file_hashkey,
#                       "&fileName=", utils::URLencode(file_info$FileName),
#                       "&fileType=",determine_file_type(file_info$FileType), sep=""),
#                 add_headers(Authorization=paste("Bearer", UserSession@auth_token))
#     )
#
#     download_status<-utils::download.file(url=parse(resp)$downloadUrl,
#                                           destfile=file.path(directory,file_info$FileName),
#                                           method = 'auto', quiet = FALSE)
#
#     if(download_status!=0){
#         print('Can not download the file.')
#         return(FALSE)
#     }else{
#         print(paste('File has been downloaded and saved to: ',file.path(directory,file_info$FileName),sep=""))
#         return(TRUE)
#     }
#
#     return(rename_temp_file(resp, directory))
# }

# determine_file_type<-function(fileType_chr){
#     if(toupper(fileType_chr)=='FCS'){
#         return('FcsFile')
#     }else{
#         return('Attachment')
#     }
# }

determine_file_type<-function(fileType_chr){
    if(is.na(fileType_chr)|toupper(fileType_chr)!='FCS'){
        return('Attachment')
    }else{
        return('FcsFile')
    }
}

#################
# VALIDATION ====
#################

# Check if path is either a directory or file ####
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

