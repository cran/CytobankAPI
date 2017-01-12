#' Helper Functions
#'
#' Various helper functions to utilize within the Cytobank API.
#' @name helper_functions
#' @param ids_names_list list of lists containing both IDs and their associated names
#' @param ids_names_df dataframe containing both IDs and their associated names
#' @param key character representing a key to sort by, specify one of the following: ("id", "name")
#' @param names_array vector or list of character regular expressions to use
#' @param reverse boolean to sort in descending order instead of ascending
NULL


#' @rdname helper_functions
#' @details \code{helper.filter_names_to_ids} Compile a vector of IDs from an array of regular expressions.
#' @examples \donttest{helper.filter_to_ids(ids_and_names_list, names_list=c("CD.*", "Time", "pp38"))
#' }
#' @export
helper.filter_names_to_ids <- function(ids_names_list, names_array=c("*"))
{
    ids <- c()

    for (regex in names_array)
    {
        for(item in grep(regex, ids_names_list))
        {
            ids <- c(ids, ids_names_list[[item]]$id)
        }
    }

    return(unique(ids))
}


#' @rdname helper_functions
#' @details \code{helper.filter_names_to_ids_from_df} Compile a vector of IDs from an array of regular expressions.
#' @examples \donttest{helper.filter_names_to_ids_from_df(id_and_names_dataframe, names_list=c("CD.*", "Time", "pp38"))
#' }
#' @export
helper.filter_names_to_ids_from_df <- function(ids_names_df, names_array=c("*"))
{
    ids <- c()

    for (regex in names_array)
    {
        ids <- c(ids, ids_names_df[[1]][grep(regex, ids_names_df[[2]])])
    }

    return(as.numeric(unique(ids)))
}


#' @rdname helper_functions
#' @details \code{helper.print_ids_names_list} Print a list of IDs and their associated names. Optional ability to sort based off of IDs or names.
#' @examples \donttest{# Print IDs and associated names as is
#' helper.print_ids_names_list(arbitrary_endpoint_ids_names_list)
#' # Print IDs and associated names, sorted by ID, in descending order
#' helper.print_ids_names_list(arbitrary_endpoint_ids_names_list, key="id", reverse=TRUE)
#' }
#' @export
helper.print_ids_names <- function(ids_names_list, key=NA, reverse=FALSE)
{
    # Sorting
    if (!is.na(key))
    {
        ids_names_list <- ids_names_list[order(sapply(ids_names_list, "[[", key), decreasing=reverse)]
    }

    for (pair in ids_names_list)
    {
        print(paste(pair$id, ": ", pair$name, sep=""))
    }
}

