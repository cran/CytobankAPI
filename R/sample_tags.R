#' Sample Tag Endpoints
#'
#' Interact with sample tag endpoints. (More descriptive text goes here)
#' @name sample_tags
#' @param directory character representing a specific directory to which the file will be downloaded (optional ending directory slash), if left empty, the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param file_path character representing a file path
#' @param timeout integer representing the request timeout time in seconds
#' @param UserSession Cytobank UserSession object
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("sample_tags.download", function(UserSession, experiment_id, directory=getwd(), timeout=UserSession@short_timeout)
{
    standardGeneric("sample_tags.download")
})
#' @rdname sample_tags
#' @aliases sample_tags.download
#'
#' @details \code{sample_tags.download} Download the sample tags from an experiment.
#' @examples \donttest{# Download the experiment sample tags TSV to the current working directory
#' sample_tags.download(cyto_session, 22)
#'
#' # Download the experiment sample tags TSV to a new directory
#' sample_tags.download(cyto_session, 22, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("sample_tags.download", signature(UserSession="UserSession"), function(UserSession, experiment_id, directory=getwd(), timeout=UserSession@short_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/download_sample_tags", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "sample_tags")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("sample_tags.upload", function(UserSession, experiment_id, file_path, timeout=UserSession@long_timeout)
{
    standardGeneric("sample_tags.upload")
})
#' @rdname sample_tags
#' @aliases sample_tags.upload
#'
#' @details \code{sample_tags.upload} Upload sample tag annotation data TSV to an experiment.
#' @examples \donttest{sample_tags.upload(cyto_session, 22, file_path="/path/to/my_annotations.tsv")
#' }
#' @export
setMethod("sample_tags.upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/upload_sample_tags", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(file=upload_file(file_path)),
                 encode="multipart",
                 timeout(timeout)
    )

    return(parse(resp, "sample_tags"))
})

