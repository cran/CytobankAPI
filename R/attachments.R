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
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
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
#' @details \code{attachments.delete} Permenantly delete an attachment.
#' @examples \donttest{attachments.delete(cyto_session, 22, attachment_id=2)
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
#' @examples \donttest{# Download an attachment to the current working directory
#' attachments.download(cyto_session, 22, attachment_id=2)
#'
#' # Download an attachment to a new directory
#' attachments.download(cyto_session, 22, attachment_id=2, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("attachments.download", signature(UserSession="UserSession"), function(UserSession, experiment_id, attachment_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/attachments/", attachment_id, "/download", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "attachments")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("attachments.download_zip", function(UserSession, experiment_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("attachments.download_zip")
})
#' @rdname attachments
#' @aliases attachments.download_zip
#'
#' @details \code{attachments.download_zip} Download all attachments as a zip file from an experiment.
#' @examples \donttest{# Download the attachment zip to the current working directory
#' attachments.download_zip(cyto_session, 22, attachment_id=2)
#'
#' # Download the attachment zip to a new directory
#' attachments.download_zip(cyto_session, 22, attachment_id=2, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("attachments.download_zip", signature(UserSession="UserSession"), function(UserSession, experiment_id, directory=getwd(), timeout=UserSession@long_timeout)
{
    directory <- directory_file_join(directory, paste("experiment_", experiment_id, "_attachments.zip", sep=""))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/attachments/download_zip", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "attachments")
    }

    return(directory)
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
#' @examples \donttest{# Dataframe of all attachments with all fields present
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
#' @examples \donttest{attachments.show(cyto_session, 22, attachment_id=2)
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
#' @examples \donttest{attachments.update(cyto_session, attachment=cyto_attachment)
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
#' @examples \donttest{attachments.upload(cyto_session, 22, file_path="/path/to/my_attachment.txt")
#' }
#' @export
setMethod("attachments.upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "attachments", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/attachments/upload", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(file=upload_file(file_path)),
                 encode="multipart",
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

