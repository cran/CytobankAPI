#' SPADE Endpoints
#'
#' Interact with SPADE advanced analyses using these endpoints.
#' @name spade
#' @param bubbles vector/list of characters representing bubbles within a SPADE analysis, \href{https://support.cytobank.org/hc/en-us/articles/115000600148-Analysis-and-Interpretation-of-SPADE-Results#Consolidate-Similar-Clusters-into-Bubbles}{learn more about SPADE bubbles}
#' @param directory character representing a specific directory (optional ending directory slash), default will be current working directory \strong{\strong{[optional]}}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- spade.list, spade.run, spade.status : \code{("default", "raw")}}
#' @param spade Cytobank SPADE object
#' @param spade_id integer representing a SPADE ID
#' @param spade_name character representing a new SPADE name
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


######################
# SPADE class methods
######################


setGeneric("spade.bubbles_export", function(UserSession, spade, bubbles, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("spade.bubbles_export")
})
#' @rdname spade
#' @aliases spade.bubbles_export
#'
#' @details \code{spade.bubbles_export} Export SPADE advanced analysis bubbles from an experiment to a new experiment.
#' @examples \donttest{spade.bubbles_export(cyto_session, spade=cyto_spade, bubbles=c("bubble1", "bubble2"))
#' }
#' @export
setMethod("spade.bubbles_export", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, bubbles, output="default", timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/export_bubbles_to_new_experiment", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(spade=list(bubbles=bubbles)),
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "SPADE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.bubbles_set", function(UserSession, spade, bubbles, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("spade.bubbles_set")
})
#' @rdname spade
#' @aliases spade.bubbles_set
#'
#' @details \code{spade.bubbles_set} Set SPADE advanced analysis bubbles from an experiment.
#' @examples \donttest{spade.bubbles_set(cyto_session, spade=cyto_spade, bubbles=named_bubble_list_of_node_vectors)
#' }
#' @export
setMethod("spade.bubbles_set", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, bubbles, output="default", timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/add_bubbles", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(spade=list(bubbles=unformat_bubbles(bubbles))),
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        # Format bubbles to named list of vectors
        return(format_bubbles(parse(resp, "SPADE")$spade$bubbles))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.bubbles_show", function(UserSession, spade, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("spade.bubbles_show")
})
#' @rdname spade
#' @aliases spade.bubbles_show
#'
#' @details \code{spade.bubbles_show} Show SPADE advanced analysis bubbles from an experiment.
#' @examples \donttest{spade.bubbles_show(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.bubbles_show", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, output="default", timeout=UserSession@short_timeout)
{
    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/show_bubbles", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        # Format bubbles to named list of vectors
        return(format_bubbles(parse(resp, "SPADE")$spade$bubbles))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.copy_results", function(UserSession, spade, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("spade.copy_results")
})
#' @rdname spade
#' @aliases spade.copy_results
#'
#' @details \code{spade.copy_results} Copy SPADE advanced analysis results from an experiment to a new experiment.
#' @examples \donttest{spade.copy_results(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.copy_results", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, output="default", timeout=UserSession@short_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/copy_settings", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "SPADE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.copy_settings", function(UserSession, spade, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("spade.copy_settings")
})
#' @rdname spade
#' @aliases spade.copy_settings
#'
#' @details \code{spade.copy_settings} Copy SPADE advanced analysis settings from an experiment.
#' @examples \donttest{spade.copy_settings(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.copy_settings", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, output="default", timeout=UserSession@short_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/copy_settings", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "SPADE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.delete", function(UserSession, spade, timeout=UserSession@short_timeout)
{
    standardGeneric("spade.delete")
})
#' @rdname spade
#' @aliases spade.delete
#'
#' @details \code{spade.delete} Delete a SPADE advanced analysis from an experiment.
#' @examples \donttest{spade.delete(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.delete", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, sep=""),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "experiments")
    }

    return(paste("SPADE (ID=", spade@spade_id, ") successfully deleted.", sep=""))
})


setGeneric("spade.download_clusters_table", function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("spade.download_clusters_table")
})
#' @rdname spade
#' @aliases spade.download_clusters_table
#'
#' @details \code{spade.download_clusters_table} Download a SPADE advanced analysis global clusters table from an experiment.
#' @examples \donttest{spade.download_clusters_table(cyto_session, spade=cyto_spade,
#'   directory="/my/new/download/directory/")
#' }
#' @export
setMethod("spade.download_clusters_table", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/download?item=clusters_table", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "SPADE")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("spade.download_global_boundaries_table", function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("spade.download_global_boundaries_table")
})
#' @rdname spade
#' @aliases spade.download_global_boundaries_table
#'
#' @details \code{spade.download_global_boundaries_table} Download a SPADE advanced analysis global boundaries table from an experiment.
#' @examples \donttest{spade.download_global_boundaries_table(cyto_session,
#'   spade=cyto_spade, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("spade.download_global_boundaries_table", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/download?item=global_boundaries_table", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "SPADE")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("spade.download_gml", function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("spade.download_gml")
})
#' @rdname spade
#' @aliases spade.download_gml
#'
#' @details \code{spade.download_gml} Download a SPADE advanced analysis GML from an experiment.
#' @examples \donttest{spade.download_gml(cyto_session, spade=cyto_spade, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("spade.download_gml", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/download?item=gml", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "SPADE")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("spade.download_layout_table", function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("spade.download_layout_table")
})
#' @rdname spade
#' @aliases spade.download_layout_table
#'
#' @details \code{spade.download_layout_table} Download a SPADE advanced analysis layout table from an experiment.
#' @examples \donttest{spade.download_layout_table(cyto_session, spade=cyto_spade, directory="/my/new/download/directory/")
#' }
#' @export
setMethod("spade.download_layout_table", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/download?item=layout_table", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "SPADE")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("spade.download_statistics_tables", function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    standardGeneric("spade.download_statistics_tables")
})
#' @rdname spade
#' @aliases spade.download_statistics_tables
#'
#' @details \code{spade.download_statistics_tables} Download a SPADE advanced analysis statistics table from an experiment.
#' @examples \donttest{spade.download_statistics_tables(cyto_session, spade=cyto_spade,
#'   directory="/my/new/download/directory/")
#' }
#' @export
setMethod("spade.download_statistics_tables", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, directory=getwd(), timeout=UserSession@long_timeout)
{
    temp_directory <- directory_file_join(directory, "tmp.part")

    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/download?item=statistics_table", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                write_disk(temp_directory, overwrite=TRUE),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "SPADE")
    }

    return(rename_temp_file(resp, directory))
})


setGeneric("spade.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("spade.list")
})
#' @rdname spade
#' @aliases spade.list
#'
#' @details \code{spade.list} List all SPADE advanced analyses from an experiment. Outputs a dataframe [default] or list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \donttest{# Dataframe of all SPADE advanced analyses with all fields present
#' spade.list(cyto_session, 22)
#'
#' # Raw list of all SPADE advanced analyses with all fields present
#' spade.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("spade.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "SPADE", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/spade", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "SPADE")$spade))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.new", function(UserSession, experiment_id, spade_name, timeout=UserSession@long_timeout)
{
    standardGeneric("spade.new")
})
#' @rdname spade
#' @aliases spade.new
#'
#' @details \code{spade.new} Create a new SPADE advanced analysis from an experiment and returns a SPADE object.
#' @examples \donttest{spade.new(cyto_session, 22, spade_name="My new SPADE analysis")
#' }
#' @export
setMethod("spade.new", signature(UserSession="UserSession"), function(UserSession, experiment_id, spade_name, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/spade/", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(spade=list(name=spade_name)),
                 encode="json",
                 timeout(timeout)
    )

    return(create_spade_object(UserSession, parse(resp, "SPADE")))
})


setGeneric("spade.rename", function(UserSession, spade, spade_name, timeout=UserSession@short_timeout)
{
    standardGeneric("spade.rename")
})
#' @rdname spade
#' @aliases spade.rename
#'
#' @details \code{spade.rename} Rename a SPADE advanced analysis from an experiment and returns a SPADE object.
#' @examples \donttest{spade.rename(cyto_session, spade=cyto_spade, spade_name="My updated SPADE name")
#' }
#' @export
setMethod("spade.rename", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, spade_name, timeout=UserSession@short_timeout)
{
    resp <- PUT(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/rename", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(spade=list(name=spade_name)),
                encode="json",
                timeout(timeout)
    )

    spade@name <- parse(resp, "SPADE")$name
    return(spade)
})


setGeneric("spade.run", function(UserSession, spade, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("spade.run")
})
#' @rdname spade
#' @aliases spade.run
#'
#' @details \code{spade.run} Run a SPADE advanced analysis from an experiment.
#' @examples \donttest{spade.run(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.run", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "SPADE", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/run", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "SPADE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.show", function(UserSession, experiment_id, spade_id, timeout=UserSession@short_timeout)
{
    standardGeneric("spade.show")
})
#' @rdname spade
#' @aliases spade.show
#'
#' @details \code{spade.show} Show SPADE advanced analysis details from an experiment and returns a SPADE object.
#' @examples \donttest{spade.show(cyto_session, 22, spade_id=2)
#' }
#' @export
setMethod("spade.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, spade_id, timeout=UserSession@short_timeout)
{
    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/spade/", spade_id, "?include_settings=1", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    return(create_spade_object(UserSession, parse(resp, "SPADE")))
})


setGeneric("spade.status", function(UserSession, spade, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("spade.status")
})
#' @rdname spade
#' @aliases spade.status
#'
#' @details \code{spade.status} Run a SPADE advanced analysis from an experiment.
#' @examples \donttest{spade.status(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.status", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "SPADE", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, "/status", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "SPADE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "SPADE"))
    }
})


setGeneric("spade.update", function(UserSession, spade, timeout=UserSession@long_timeout)
{
    standardGeneric("spade.update")
})
#' @rdname spade
#' @aliases spade.update
#'
#' @details \code{spade.update} Update a SPADE advanced analysis from an experiment and returns the new SPADE object.
#' @examples \donttest{spade.update(cyto_session, spade=cyto_spade)
#' }
#' @export
setMethod("spade.update", signature(UserSession="UserSession", spade="SPADE"), function(UserSession, spade, timeout=UserSession@long_timeout)
{
    # Convert fold change groups dataframe -> list readable by update endpoint
    fold_change_groups <- fold_change_groups_dataframe_to_list(spade@fold_change_groups)

    resp <- PUT(paste(UserSession@site, "/experiments/", spade@source_experiment, "/advanced_analyses/spade/", spade@spade_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(spade=list(
                    name=spade@name,
                    compensationId=spade@compensation_id,
                    targetNumberOfNodes=spade@target_number_nodes,
                    population=spade@population_id,
                    clusteringChannels=spade@channels,
                    downSampledEventsTarget=list(percent=spade@down_sampled_events_target),
                    foldChangeGroups=fold_change_groups
                )
                ),
                encode="json",
                timeout(timeout)
    )

    return(create_spade_object(UserSession, parse(resp, "SPADE")))
})


#########################
# SPADE HELPER FUNCTIONS
#########################


##########
# PRIVATE
##########


# Create SPADE object from SPADE json response
create_spade_object <- function(UserSession, spade_response)
{
    return(
        new("SPADE", name=spade_response$spade$name,
            target_number_nodes=spade_response$spade$settings$targetNumberOfNodes,
            population_id=spade_response$spade$settings$population,
            down_sampled_events_target=spade_response$spade$settings$downSampledEventsTarget$percent,
            fold_change_groups=create_fold_change_groups(spade_response$spade$settings$foldChangeGroups),
            spade_id=spade_response$spade$id,
            channels=spade_response$spade$settings$clusteringChannels,
            compensation_id=spade_response$spade$settings$compensation,
            source_experiment=spade_response$spade$sourceExperiment,
            status=spade_response$spade$status,
            .available_channels=panels.list(UserSession, spade_response$spade$sourceExperiment),
            .available_files=fcs_files.list(UserSession, spade_response$spade$sourceExperiment),
            .available_populations=populations.list(UserSession, spade_response$spade$sourceExperiment))
    )
}


# Converts fold change groups output -> fold change groups dataframe
create_fold_change_groups <- function(fold_change_groups_output)
{
    fold_change_groups_list <- list()

    # for each group
    for (group in fold_change_groups_output)
    {
        # Create a dataframe
        temp_data <- do.call(rbind.data.frame, group[[2]])
        temp_data["name"] <- lapply(temp_data["name"], as.character) # Convert 'names' as factors -> characters
        # Add group name to group
        temp_data$group_name <- apply(temp_data, 1, function(row) group[[1]])
        fold_change_groups_list <- c(fold_change_groups_list, list(temp_data))
    }

    # Combine and return one dataframe of fold change group data
    return(do.call(rbind, fold_change_groups_list))
}


# Convert fold change groups dataframe -> fold change groups list for update
fold_change_groups_dataframe_to_list <- function(fold_change_groups_dataframe)
{
    # Dataframe -> list with group[fcs_files]
    fold_change_groups_list <- list()
    for (x in seq(nrow(fold_change_groups_dataframe)))
    {
        fold_change_groups_list[[fold_change_groups_dataframe$group_name[[x]]]]$fcsFiles <- c(
            fold_change_groups_list[[fold_change_groups_dataframe$group_name[[x]]]]$fcsFiles,
            list(list(id=fold_change_groups_dataframe$id[[x]],
                      baseline=fold_change_groups_dataframe$baseline[[x]])
            )
        )
    }

    # group[fcs_files] -> [group, fcs_files]
    fold_change_groups <- list()
    for (group in names(fold_change_groups_list))
    {
        fold_change_groups <- c(fold_change_groups, list(list(name=group, fcsFiles=fold_change_groups_list[[group]]$fcsFiles)))
    }

    return(fold_change_groups)
}


# Format bubbles output from spade.bubbles_show
format_bubbles <- function(spade_bubbles)
{
    bubble_list <- list()
    for (bubble in spade_bubbles)
    {
        for (node in bubble[["nodes"]])
        {
            bubble_list[[bubble[["name"]]]] <- c(bubble_list[[bubble[["name"]]]], node)
        }
    }

    return(bubble_list)
}


# Unformat formatted bubbles output from spade.bubbles_show
unformat_bubbles <- function(spade_bubbles)
{
    bubble_list <- list()
    for (bubble in names(spade_bubbles))
    {
        bubble_list <- c(bubble_list, list(list(name=bubble, nodes=spade_bubbles[[bubble]])))
    }

    return(bubble_list)
}


