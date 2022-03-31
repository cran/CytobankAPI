# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Attachment Endpoints
#'
#' Interact with attachments using these endpoints. Only FCS files can be analyzed in Cytobank, but any file can be uploaded as an attachment. Exported PDFs, statistics, and files also automatically attach themselves to the Experiment they are exported from. \href{https://support.cytobank.org/hc/en-us/articles/206145297-Any-type-of-data-can-be-uploaded-and-stored-in-Cytobank}{Learn more about attachments in Cytobank}.
#' @name attachments
#' @param attachment dataframe representing an attachment (can retrieve via the attachments.show endpoint)
#' @param attachment_id integer representing an attachment ID
#' @param directory character representing a specific directory to which the file will be downloaded (optional ending directory slash), if left empty, the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param file_path character representing a file path
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- attachments.list, attachments.show, attachments.update : \code{("default", "raw")}}
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL

setGeneric("attachments.delete", function(UserSession, experiment_id, attachment_id, timeout=UserSession@short_timeout)
{
    standardGeneric("attachments.delete")
})
#' @rdname attachments
#' @aliases attachments.delete
#'
#' @details \code{attachments.delete} Permanently delete an attachment.
#' @examples \dontrun{attachments.delete(cyto_session, 22, attachment_id=2)
#' }
#' @export
setMethod("attachments.delete", signature(UserSession="UserSession"), function(UserSession, experiment_id, attachment_id, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", experiment_id, "/attachments/", attachment_id, sep=""),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "attachments")
    }

    return(paste("attachment (ID=", attachment_id, ") successfully deleted.", sep=""))
})


setGeneric("attachments.download", function(UserSession, experiment_id, attachment_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("attachments.download")
})
#' @rdname attachments
#' @aliases attachments.download
#'
#' @details \code{attachments.download} Download an attachment from an experiment.
#' @examples \dontrun{# Download an attachment to the current working directory
#' attachments.download(cyto_session, 22)
#'
#' # Download an attachment to a new directory
#' attachments.download(cyto_session, 22, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("attachments.download", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, attachment_id, directory=getwd(), timeout=UserSession@long_timeout)

{
    temp_directory <- directory_file_join(directory, "tmp.part")

    baseURL = get_base_url(UserSession)

    attachment_info <- attachments.show(UserSession, experiment_id, attachment_id)
    file_hashkey <- unlist(attachment_info$uniqueHash)
    file_name <- unlist(attachment_info$filename)
    file_type <- unlist(attachment_info$type)

    resp <- GET(paste(baseURL,'/download/url?', "experimentId=", experiment_id, "&hashKey=", file_hashkey,
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

    return(rename_temp_file(resp, directory))
})


setGeneric("attachments.download_zip", function(UserSession, experiment_id, attachment_id, timeout=UserSession@long_timeout)
{
    standardGeneric("attachments.download_zip")
})
#' @rdname attachments
#' @aliases attachments.download_zip
#'
#' @details \code{attachments.download_zip} Download all or a select set of attachments as a zip file from an experiment. The download link of the zip file will be sent to the user's registered email address.
#' @examples \dontrun{# Download the all attachment files as a zip file
#' attachments.download_zip(cyto_session, experiment_id=22)
#'
#' # Download a select set of attachment files as a zip file
#' attachments.download_zip(cyto_session, experiment_id=22, attachment_id=2)
#' }
#' @export
setMethod("attachments.download_zip", signature(UserSession="UserSession"), function(UserSession, experiment_id, attachment_id, timeout=UserSession@long_timeout)
{
    baseURL = get_base_url(UserSession)

    # Create a file list
    if (missing(attachment_id))
{
        # download all files in a experiment
        attachment_files <- ""
        file_info <- attachments.list(UserSession, experiment_id)

        if(length(file_info$id)==0){stop("Error: Can't find any attachment files in your experiment!")}

        fileList_body <- apply(file_info,1, function(x){
            return(list(fileName = x$filename,
                        keyPrefix = 'attachments',
                        hashKey = x$uniqueHash))
        })

    }else if (length(attachment_id) == 0){
        stop(
            sprintf(
                "Cytobank API 'fcs_files' request failed [client]\n    Please provide a vector/list of attachment file IDs or leave blank to download all FCS files as a zip.\n", sep=""),
            call. = FALSE
        )

    }else{

        file_info <- attachments.list(UserSession, experiment_id)
        if(length(file_info$id)==0){stop("Error: Can't find any attachment files in your experiment!")}
        fileList_body <- apply(file_info,1, function(x){
            return(list(fileName = x$filename,
                        keyPrefix = 'attachments',
                        hashKey = x$uniqueHash))
        })

        file_index <- c(unlist(file_info$id) %in% attachment_id)
        if(all(!file_index)){stop("Error: Can't find the attachment file in your experiment!")}
        fileList_body<-fileList_body[file_index]
    }

    resp <- POST(paste(baseURL, "/download/zip",sep=''),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body = list(zipFileName = paste("experiment_", experiment_id, "_attachments.zip", sep=""),
                             experimentId = experiment_id,
                             userId = UserSession@user_id,
                             fileList = fileList_body),
                 encode="json",
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "attachments")
    }else{
        print('Your zip download request has been processed by the Cytobank server. Cytobank will send an email to your registered email address with a download link of the zip file. It may take several minutes for Cytobank to zip large files. Thanks for your patience!')
        return(TRUE)
    }
})


setGeneric("attachments.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("attachments.list")
})
#' @rdname attachments
#' @aliases attachments.list
#'
#' @details \code{attachments.list} List all attachments from an experiment. Outputs a dataframe [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all attachments with all fields present
#' attachments.list(cyto_session, 22)
#'
#' # Raw list of all attachments with all fields present
#' attachments.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("attachments.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "attachments", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/attachments", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "attachments")[[1]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "attachments"))
    }
})


setGeneric("attachments.show", function(UserSession, experiment_id, attachment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("attachments.show")
})
#' @rdname attachments
#' @aliases attachments.show
#'
#' @details \code{attachments.show} Show attachment details from an experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{attachments.show(cyto_session, 22, attachment_id=2)
#' }
#' @export
setMethod("attachments.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, attachment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "attachments", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/attachments/", attachment_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "attachments")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "attachments"))
    }
})

setGeneric("attachments.update", function(UserSession, attachment, timeout=UserSession@short_timeout)
{
    standardGeneric("attachments.update")
})
#' @rdname attachments
#' @aliases attachments.update
#'
#' @details \code{attachments.update} Update an attachment description from an experiment.
#' @examples \dontrun{attachments.update(cyto_session, attachment=cyto_attachment)
#' }
#' @export
setMethod("attachments.update", signature(UserSession="UserSession"), function(UserSession, attachment, timeout=UserSession@short_timeout)
{
    update_check(attachment, "attachments")

    resp <- PUT(paste(UserSession@site, "/experiments/", attachment$experimentId, "/attachments/", attachment$id, sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(attachments=list(description=attachment$description)),
                 encode="json",
                 timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "attachments")
    }

    return(sprintf(paste("Attachment description (ID=", attachment$id, ") successfully updated. ",
                 "New description: '", attachment$description, "'", sep="")))
})


setGeneric("attachments.upload", function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("attachments.upload")
})
#' @rdname attachments
#' @aliases attachments.upload
#'
#' @details \code{attachments.upload} Upload an attachment to an experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{attachments.upload(cyto_session, 22, file_path="/path/to/my_attachment.txt")
#' }
#' @export
setMethod("attachments.upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{

    file_upload(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)

})


#############################
# ATTACHMENT HELPER FUNCTIONS
#############################

get_attachment_hashkey <- function(UserSession, experiment_id, attachment_id){
   return(unlist(attachments.show(UserSession, experiment_id, attachment_id)$uniqueHash))
}

##########
# PRIVATE
##########




