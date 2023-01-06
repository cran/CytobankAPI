# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @importFrom stats setNames
NULL

#' FCS File Endpoints
#'
#' Interact with FCS file endpoints.
#' @name fcs_files
#' @param directory character representing a specific directory to which the file will be downloaded (optional ending directory slash), if left empty, the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param fcs_files vector/list of integers representing a list of FCS file IDs
#' @param fcs_file_id integer representing an FCS file ID
#' @param file_path character representing a file path
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- fcs_files.file_internal_comp_show : \code{("default", "dataframe", "raw")}}\cr
#' \emph{- fcs_files.list, fcs_files.show, fcs_files.upload, fcs_files.upload_zip : \code{("default", "raw")}}\cr
#' \emph{- \code{dataframe}: converts the file internal compensation matrix output to a dataframe}
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("fcs_files.download", function(UserSession, experiment_id, fcs_file_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("fcs_files.download")
})
#' @rdname fcs_files
#' @aliases fcs_files.download
#'
#' @details \code{fcs_files.download} Download an FCS file from an experiment.
#' @examples \dontrun{# Download an FCS file to the current working directory
#' fcs_files.download(cyto_session, experiment_id = 22, fcs_file_id = 4)
#'
#' # Download an FCS file to a new directory
#' fcs_files.download(cyto_session, 22, experiment_id = 22, fcs_file_id = 4,
#' directory="/my/new/download/directory/")
#' }
#' @export
setMethod("fcs_files.download", signature(UserSession="UserSession"), function(UserSession, experiment_id, fcs_file_id, directory=getwd(), timeout=UserSession@long_timeout)
{


    {

        temp_directory <- directory_file_join(directory, "tmp.part")

        baseURL = get_base_url(UserSession)

        file_info <- fcs_files.show(UserSession, experiment_id, fcs_file_id)
        file_hashkey <- unlist(file_info$uniqueHash)
        file_name <- unlist(file_info$filename)
        file_type <- sub('.*\\.', '', file_name)

        resp <- GET(paste(baseURL,'/download/url?', "experimentId=", experiment_id, "&hashKey=", file_hashkey,
                          "&fileName=", utils::URLencode(file_name),
                          "&fileType=",determine_file_type(file_type), sep=""),
                    add_headers(Authorization=paste("Bearer", UserSession@auth_token))
        )

        download_status<-utils::download.file(url=parse(resp)$downloadUrl,
                                              destfile=file.path(directory,file_name),
                                              method = 'auto', quiet = FALSE, mode = 'wb')

        if(download_status!=0){
            print('Cannot download the file.')
            return(FALSE)
        }else{
            print(paste('File has been downloaded and saved to: ',file.path(directory,file_name),sep=""))
            return(TRUE)
        }

        return(rename_temp_file(resp, directory))
    }

})

setGeneric("fcs_files.download_zip", function(UserSession, experiment_id, fcs_files, timeout=UserSession@long_timeout)
{
    standardGeneric("fcs_files.download_zip")
})
#' @rdname fcs_files
#' @aliases fcs_files.download_zip
#'
#' @details \code{fcs_files.download_zip} Download all or a select set of FCS files as a zip file from an experiment. The download link of the zip file will be sent to the user's registered email address.
#' @examples \dontrun{# Download all FCS files as a zip file
#' fcs_files.download_zip(cyto_session, experiment_id=22)
#'
#' # Download a select set of FCS files as a zip file
#' fcs_files.download_zip(cyto_session, experiment_id=22, fcs_files=c(22, 23, 24, 25))
#' }
#' @export
setMethod("fcs_files.download_zip", signature(UserSession="UserSession"), function(UserSession, experiment_id, fcs_files, timeout=UserSession@long_timeout)
{

    baseURL = get_base_url(UserSession)

    # Create a file list
    if (missing(fcs_files))
    {
        # download all files in a experiment
        fcs_files <- ""
        file_info <- fcs_files.list(UserSession, experiment_id)

        if(length(file_info$id)==0){stop("Error: Can't find any files in your experiment!")}

        fcs_files <- file_info$id

        fileList_body <- apply(file_info,1, function(x){
            return(list(fileName = x$filename,
                        keyPrefix = 'fcs_files',
                        hashKey = x$uniqueHash))
        })

    }else if (length(fcs_files) == 0){
        stop(
            sprintf(
                "Cytobank API 'fcs_files' request failed [client]\n    Please provide a vector/list of FCS file IDs or leave blank to download all FCS files as a zip.\n", sep=""),
            call. = FALSE
        )

    }else{
        #fcs_files <- paste("?fcs_file_ids=", gsub(" ", "", toString(fcs_files)), sep="")
        file_info <- fcs_files.list(UserSession, experiment_id)
        if(length(file_info$id)==0){stop("Error: Can't find any files in your experiment!")}

        fileList_body <- apply(file_info,1, function(x){
           return(list(fileName = x$filename,
                        keyPrefix = 'fcs_files',
                        hashKey = x$uniqueHash))
        })

        file_index <- c(unlist(file_info$id) %in% fcs_files)
        if(all(!file_index)){stop("Error: Can't find the FCS file in your experiment!")}
        fileList_body<-fileList_body[file_index]
}

    resp <- POST(paste(baseURL, "/download/zip",sep=''),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body = list(zipFileName = paste("experiment_", experiment_id, "_fcs_files.zip", sep=""),
                            experimentId = experiment_id,
                            userId = UserSession@user_id,
                            fileList = fileList_body),
                encode="json",
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "fcs_files")
    }else{
        print('Your zip download request has been processed by the Cytobank server. Cytobank will send an email to your registered email address with a download link of the zip file. It may take several minutes for Cytobank to zip large data files. Thanks for your patience!')
        return(TRUE)
    }


})


setGeneric("fcs_files.file_internal_comp_show", function(UserSession, experiment_id, fcs_file_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("fcs_files.file_internal_comp_show")
})
#' @rdname fcs_files
#' @aliases fcs_files.file_internal_comp_show
#'
#' @details \code{fcs_files.file_internal_comp_show} Show FCS file internal compensation (aka spillover matrix, spill matrix, spill string) details from an experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "dataframe", "raw")}}
#' @examples \dontrun{# List of a file internal compensation, containing a file internal compensation matrix
#' fcs_files.file_internal_comp_show(cyto_session, 22, fcs_file_id=2)
#'
#' # Dataframe only of a file internal compensation
#' fcs_files.file_internal_comp_show(cyto_session, 22, fcs_file_id=2, output="dataframe")
#'
#' # Raw list of a file internal compensation
#' fcs_files.file_internal_comp_show(cyto_session, 22, fcs_file_id=2, output="raw")
#' }
#' @export
setMethod("fcs_files.file_internal_comp_show", signature(UserSession="UserSession"), function(UserSession, experiment_id, fcs_file_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "fcs_files", possible_outputs=c("dataframe", "raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/fcs_files/", fcs_file_id, "/compensation/", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_fi_compensation(parse(resp, "fcs_files")))
    }
    else if (output == "dataframe")
    {
        return(compensation_to_dataframe(parse(resp, "fcs_files")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "fcs_files"))
    }
})


setGeneric("fcs_files.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("fcs_files.list")
})
#' @rdname fcs_files
#' @aliases fcs_files.list
#'
#' @details \code{fcs_files.list} List all FCS files from an experiment. Outputs a dataframe [default] or raw full list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all FCS files with all fields present
#' fcs_files.list(cyto_session, 22)
#'
#' # Raw list of all FCS files with all fields present
#' fcs_files.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("fcs_files.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "fcs_files", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/fcs_files", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "fcs_files")[[1]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "fcs_files"))
    }
})


setGeneric("fcs_files.show", function(UserSession, experiment_id, fcs_file_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("fcs_files.show")
})
#' @rdname fcs_files
#' @aliases fcs_files.show
#'
#' @details \code{fcs_files.show} Show FCS file details from an experiment.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{fcs_files.show(cyto_session, 22, fcs_file_id=2)
#' }
#' @export
setMethod("fcs_files.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, fcs_file_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "fcs_files", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/fcs_files/", fcs_file_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "fcs_files")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "fcs_files"))
    }
})


setGeneric("fcs_files.upload", function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("fcs_files.upload")
})
#' @rdname fcs_files
#' @aliases fcs_files.upload
#'
#' @details \code{fcs_files.upload} Upload an FCS file to an experiment. Cytobank User ID has to be attached to the UserSession object. See the help document of authenticate function for details.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{fcs_files.upload(cyto_session, 22, file_path="/path/to/my_fcs_file.fcs")
#' }
#' @export
setMethod("fcs_files.upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{

    if(!check_file(file_path,'FCS')){return(FALSE)}

    return(file_upload(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout))

})


setGeneric("fcs_files.upload_zip", function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("fcs_files.upload_zip")
})
#' @rdname fcs_files
#' @aliases fcs_files.upload_zip
#'
#' @details \code{fcs_files.upload_zip} Upload a zip of FCS file(s) to an experiment.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{fcs_files.upload_zip(cyto_session, 22, file_path="/path/to/my_fcs_files.zip")
#' }
#' @export
setMethod("fcs_files.upload_zip", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{

    if(!check_file(file_path,'ZIP')){return(FALSE)}

    return(file_upload(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout))

})

setGeneric("fcs_files.status", function(UserSession, experiment_id, timeout=UserSession@long_timeout)
{
    standardGeneric("fcs_files.status")
})
#' @rdname fcs_files
#' @aliases fcs_files.status
#'
#' @details \code{fcs_files.status} Check status of file(s) in an experiment. Return FALSE and print out an warming message if it fail. Otherwise, return a R dataframe object with file status information.
#' @examples \dontrun{fcs_files.status(cyto_session, 22)
#' }
#' @export
setMethod("fcs_files.status", signature(UserSession="UserSession"), function(UserSession, experiment_id, timeout=UserSession@long_timeout)
{

    return(check_file_status(UserSession,experiment_id))

})


#############################
# FCS FILES HELPER FUNCTIONS
#############################




##########
# PRIVATE
##########


# Build compensation info in R
build_fi_compensation <- function(fi_compensation_info)
{
    return(
        setNames(
            list(list(id=fi_compensation_info[[1]]$id,
                      compensation_matrix=compensation_to_dataframe(fi_compensation_info))),
            gsub(" - spill string", "", fi_compensation_info[[1]]$name)
        )
    )
}

