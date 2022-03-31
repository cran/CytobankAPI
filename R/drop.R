# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
#' @param skipped_columns vector/list of integer(s) representing column(s) of the DROP file to skip
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
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
#' @examples \dontrun{drop.upload(cyto_session, 22, file_path="/path/to/my_drop_file.type",
#'   data_matrix_start_row=2, data_matrix_start_column=1)
#' }
#' @export
setMethod("drop.upload", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, data_matrix_start_row=2, data_matrix_start_column=1, skipped_columns=c(), output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "fcs_files", possible_outputs=c("raw"))

    return(file_upload(UserSession,
                       experiment_id,
                       file_path,
                       output = "default",
                       timeout,
                       upload_type = 'drop',
                       drop_data_matrix_start_row=data_matrix_start_row,
                       drop_data_matrix_start_column=data_matrix_start_column
                      ))

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

