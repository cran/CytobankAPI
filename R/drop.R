#' @importFrom curl curl handle_setform handle_setheaders new_handle
NULL

#' DROP File Endpoints
#'
#' Upload DROP file(s) into Cytobank. A DROP file consists of any CSV, TSV, TXT, or FCS file. If the DROP file is of the type CSV, TSV, or TXT, the file will be converted to an FCS file to be used within Cytobank. \href{https://support.cytobank.org/hc/en-us/articles/206252468-How-to-analyze-gene-expression-or-imaging-data-in-Cytobank-CSV-to-FCS-}{Learn more about DROP}.
#' @name drop
#' @param data_matrix_start_column integer representing the start column of the DROP file(s)
#' @param data_matrix_start_row integer representing the start row of the DROP file(s)
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param file_path character representing a file path
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- drop.upload : \code{("default", "raw")}}\cr
#' \emph{- \code{dataframe}: converts the file internal compensation matrix output to a dataframe}
#' @param skipped_columns integer vector representing the channels to skip within the DROP file(s)
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("drop.upload", function(UserSession, experiment_id, file_path, data_matrix_start_row=2, data_matrix_start_column=1, skipped_columns=c(), output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("drop.upload")
})
#' @rdname drop
#' @aliases drop.upload
#'
#' @details \code{drop.upload} Upload a DROP file (CSV, TSV, TXT, FCS) to an experiment.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \donttest{drop.upload(cyto_session, 22, file_path="/path/to/my_drop_file.type",
#'   data_matrix_start_row=2, data_matrix_start_column=1, skipped_columns=c(4,8))
#' }
#' @export
setMethod("drop.upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, data_matrix_start_row=2, data_matrix_start_column=1, skipped_columns=c(), output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "fcs_files", possible_outputs=c("raw"))

    skipped_columns <- add_skipped_columns(skipped_columns)
    form_data <- list(convertDelimitedFiles="true", dataMatrixStartRow=as.character(data_matrix_start_row), dataMatrixStartColumn=as.character(data_matrix_start_column))
    form_data <- c(form_data, skipped_columns)

    options(warn=-1)
    # Have to use lower level 'curl' package because httr cannot send both multipart form file AND form data
    # - Set form directly via the curl::handle_setform function, which allows for adding form file and form data together (form file + json)
    url <- paste(UserSession@site, "/experiments/", experiment_id, "/fcs_files/upload", sep="")
    h <- curl::new_handle()
    curl::handle_setheaders(h, "Authorization"=paste("Bearer", UserSession@auth_token))
    curl::handle_setform(h, file=curl::form_file(file_path), .list=form_data)
    con <- curl::curl(url, handle = h)
    resp <- paste(readLines(con), sep = "\n")[[1]]
    close(con)
    options(warn=0)

    if (output == "default")
    {
        return(jsonlite::fromJSON(resp[[1]])[[1]])
    }
    else # if (output == "raw")
    {
        return(resp[[1]])
    }
})


########################
# DROP HELPER FUNCTIONS
########################


##########
# PRIVATE
##########


add_skipped_columns <- function(skipped_columns_vector)
{
    skipped_columns <- list()
    for (column in skipped_columns_vector)
    {
        skipped_columns <- c(skipped_columns, list("skippedColumns[]"=as.character(column)))
    }

    return(skipped_columns)
}

