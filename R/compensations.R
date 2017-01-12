#' @importFrom stats setNames
NULL

#' Compensation Endpoints
#'
#' Interact with compensation endpoints. Get information about compensations stored in Cytobank. For information about file-internal compensation for an individual FCS file, consult the \link[=fcs_files]{FCS files} endpoints. \href{https://support.cytobank.org/hc/en-us/sections/200867447-Articles-on-Compensation}{Learn more about compensation in Cytobank}.
#' @name compensations
#' @param compensation_id integer representing a compensation ID
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param file_path character representing a file path
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- compensations.list : \code{("default", "raw")}}
#' \emph{- compensations.show : \code{("default", "dataframe", "raw")}}
#' \emph{- \code{dataframe}: converts the compensation matrix output to a dataframe}
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("compensations.upload_csv", function(UserSession, experiment_id, file_path, timeout=UserSession@long_timeout)
{
    standardGeneric("compensations.upload_csv")
})
#' @rdname compensations
#' @aliases compensations.upload_csv
#'
#' @details \code{compensations.upload_csv} Upload a compensation CSV to an experiment.
#' @examples \donttest{compensations.upload_csv(cyto_session, 22, file_path="/path/to/my_compensation.csv")
#' }
#' @export
setMethod("compensations.upload_csv", signature(UserSession="UserSession"), function(UserSession, experiment_id, file_path, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/upload_compensation_matrix", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(file=upload_file(file_path)),
                 encode="multipart",
                 timeout(timeout)
    )

    return(parse(resp, "compensations"))
})


setGeneric("compensations.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("compensations.list")
})
#' @rdname compensations
#' @aliases compensations.list
#'
#' @details \code{compensations.list} List all compensations from an experiment. Outputs a formatted list [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \donttest{# List of all compensations with all fields present, with a compensation matrix dataframe list item
#' compensations.list(cyto_session, 22)
#'
#' # Raw list of all compensations with all fields present
#' compensations.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("compensations.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "compensations", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/compensations", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_compensation_list(parse(resp, "compensations")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "compensations"))
    }

})


setGeneric("compensations.show", function(UserSession, experiment_id, compensation_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("compensations.show")
})
#' @rdname compensations
#' @aliases compensations.show
#'
#' @details \code{compensations.show} Show compensation details from an experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "dataframe", "raw")}}\cr
#' \emph{- \code{dataframe}: converts the compensation matrix output to a dataframe}
#' @examples \donttest{# List form of a compensation
#' compensations.show(cyto_session, 22, compensation_id=2)
#'
#' # Compensation dataframe only
#' compensations.show(cyto_session, 22, compensation_id=2, output="dataframe")
#' }
#' @export
setMethod("compensations.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, compensation_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "compensations", possible_outputs=c("dataframe", "raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/compensations/", compensation_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_compensation(parse(resp, "compensations")))
    }
    else if (output == "dataframe")
    {
        return(compensation_to_dataframe(parse(resp, "compensations")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "compensations"))
    }
})


##################################
# COMPENSATIONS HELPER FUJNCTIONS
##################################


##########
# PRIVATE
##########


# Build panel list in R
build_compensation_list <- function(compensation_list_info)
{
    compensation_list <- list()
    for(compensation in compensation_list_info[[1]])
    {
        # need to encapsulate compensation info in a list for compensation_to_dataframe to work correctly
        compensation_list <- c(compensation_list, build_compensation(list(compensation)))
    }

    return(compensation_list)
}


# Build compensation info in R
build_compensation <- function(compensation_info)
{
    return(
        setNames(
            list(list(id=compensation_info[[1]]$id,
                      experiment_id=compensation_info[[1]]$experimentId,
                      created_at=compensation_info[[1]]$createdAt,
                      updated_at=compensation_info[[1]]$updatedAt,
                      compensation_matrix=compensation_to_dataframe(compensation_info))),
            compensation_info[[1]]$name
        )
    )
}

