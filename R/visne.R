#' viSNE Endpoints
#'
#' Interact with viSNE advanced analyses using these endpoints.
#' @name visne
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param fcs_files vector/list of integers representing a list of FCS file IDs
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- visne.list, visne.run, visne.status : \code{("default", "raw")}}
#' @param population_id integer representing a population \strong{gate set ID}
#' @param visne Cytobank viSNE object
#' @param visne_id integer representing a viSNE ID
#' @param visne_name character representing a new viSNE name
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#'
#' # cyto_visne refers to a viSNE object that is created from viSNE endpoints
#' #   examples: visne.new, visne.show (see details section for more)
#' }
NULL


######################
# viSNE class methods
######################


setGeneric("visne.copy_settings", function(UserSession, visne, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "viSNE", possible_outputs=c("raw"))

    standardGeneric("visne.copy_settings")
})
#' @rdname visne
#' @aliases visne.copy_settings
#'
#' @details \code{visne.copy_settings} Copy viSNE advanced analysis settings from an experiment and returns a viSNE object.
#' @examples \donttest{visne.copy_settings(cyto_session, visne=cyto_visne)
#' }
#' @export
setMethod("visne.copy_settings", signature(UserSession="UserSession", visne="viSNE"), function(UserSession, visne, output="default", timeout=UserSession@short_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", visne@source_experiment, "/advanced_analyses/visne/", visne@visne_id, "/copy_settings", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "viSNE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "viSNE"))
    }
})


setGeneric("visne.delete", function(UserSession, visne, timeout=UserSession@short_timeout)
{
    standardGeneric("visne.delete")
})
#' @rdname visne
#' @aliases visne.delete
#'
#' @details \code{visne.delete} Delete a viSNE advanced analysis from an experiment.
#' @examples \donttest{visne.delete(cyto_session, visne=cyto_visne)
#' }
#' @export
setMethod("visne.delete", signature(UserSession="UserSession", visne="viSNE"), function(UserSession, visne, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", visne@source_experiment, "/advanced_analyses/visne/", visne@visne_id, sep=""),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "experiments")
    }

    return(paste("viSNE (ID=", visne@visne_id, ") successfully deleted.", sep=""))
})


setGeneric("visne.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("visne.list")
})
#' @rdname visne
#' @aliases visne.list
#'
#' @details \code{visne.list} List all viSNE advanced analyses from an experiment. Outputs a dataframe [default] or list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \donttest{# Dataframe of all viSNE advanced analyses with all fields present
#' visne.list(cyto_session, 22)
#'
#' # Raw list of all viSNE advanced analyses with all fields present
#' visne.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("visne.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "viSNE", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/visne", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "viSNE")$visne))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "viSNE"))
    }
})


setGeneric("visne.new", function(UserSession, experiment_id, visne_name, timeout=UserSession@long_timeout)
{
    standardGeneric("visne.new")
})
#' @rdname visne
#' @aliases visne.new
#'
#' @details \code{visne.new} Create a new viSNE advanced analysis from an experiment and returns a viSNE object.
#' @examples \donttest{visne.new(cyto_session, 22, visne_name="My new viSNE analysis")
#' }
#' @export
setMethod("visne.new", signature(UserSession="UserSession"), function(UserSession, experiment_id, visne_name, timeout=UserSession@long_timeout)
{
    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/visne/", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(visne=list(name=visne_name)),
                 encode="json",
                 timeout(timeout)
    )

    return(create_visne_object(UserSession, parse(resp, "viSNE")))
})


setGeneric("visne.rename", function(UserSession, visne, visne_name, timeout=UserSession@short_timeout)
{
    standardGeneric("visne.rename")
})
#' @rdname visne
#' @aliases visne.rename
#'
#' @details \code{visne.rename} Rename a viSNE advanced analysis from an experiment and returns a viSNE object.
#' @examples \donttest{visne.rename(cyto_session, visne=cyto_visne, visne_name="My updated viSNE name")
#' }
#' @export
setMethod("visne.rename", signature(UserSession="UserSession", visne="viSNE"), function(UserSession, visne, visne_name, timeout=UserSession@short_timeout)
{
    resp <- PUT(paste(UserSession@site, "/experiments/", visne@source_experiment, "/advanced_analyses/visne/", visne@visne_id, "/rename", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(visne=list(name=visne_name)),
                encode="json",
                timeout(timeout)
    )

    visne@name <- parse(resp, "viSNE")$visne$name
    return(visne)
})


setGeneric("visne.run", function(UserSession, visne, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("visne.run")
})
#' @rdname visne
#' @aliases visne.run
#'
#' @details \code{visne.run} Run a viSNE advanced analysis from an experiment.
#' @examples \donttest{visne.run(cyto_session, visne=cyto_visne)
#' }
#' @export
setMethod("visne.run", signature(UserSession="UserSession", visne="viSNE"), function(UserSession, visne, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "viSNE", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", visne@source_experiment, "/advanced_analyses/visne/", visne@visne_id, "/run", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "viSNE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "viSNE"))
    }
})


setGeneric("visne.show", function(UserSession, experiment_id, visne_id, timeout=UserSession@short_timeout)
{
    standardGeneric("visne.show")
})
#' @rdname visne
#' @aliases visne.show
#'
#' @details \code{visne.show} Show viSNE advanced analysis details from an experiment and returns a viSNE object.
#' @examples \donttest{visne.show(cyto_session, 22, visne_id=2)
#' }
#' @export
setMethod("visne.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, visne_id, timeout=UserSession@short_timeout)
{
    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/visne/", visne_id, "?include_settings=1", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    return(create_visne_object(UserSession, parse(resp, "viSNE")))
})


setGeneric("visne.status", function(UserSession, visne, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("visne.status")
})
#' @rdname visne
#' @aliases visne.status
#'
#' @details \code{visne.status} Show the status of a viSNE advanced analysis from an experiment.
#' @examples \donttest{visne.status(cyto_session, visne=cyto_visne)
#' }
#' @export
setMethod("visne.status", signature(UserSession="UserSession", visne="viSNE"), function(UserSession, visne, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "viSNE", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", visne@source_experiment, "/advanced_analyses/visne/", visne@visne_id, "/status", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "viSNE")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "viSNE"))
    }
})


setGeneric("visne.update", function(UserSession, visne, timeout=UserSession@long_timeout)
{
    standardGeneric("visne.update")
})
#' @rdname visne
#' @aliases visne.update
#'
#' @details \code{visne.update} Update a viSNE advanced analysis from an experiment and returns the new viSNE object.
#' @examples \donttest{visne.update(cyto_session, visne=cyto_visne)
#' }
#' @export
setMethod("visne.update", signature(UserSession="UserSession", visne="viSNE"), function(UserSession, visne, timeout=UserSession@long_timeout)
{
    # Convert population selections dataframe -> list readable by update endpoint
    population_selections <- population_selections_dataframe_to_list(visne@population_selections)

    if (length(visne@channels) && is.character(visne@channels[[1]]))
    {
        visne@channels <- as.list(helper.channel_ids_from_long_names(visne@.available_channels, visne@channels))
    }

    resp <- PUT(paste(UserSession@site, "/experiments/", visne@source_experiment, "/advanced_analyses/visne/", visne@visne_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(visne=list(
                    name=visne@name,
                    compensationId=visne@compensation_id,
                    samplingTotalCount=visne@sampling_total_count,
                    samplingTargetType=visne@sampling_target_type,
                    iterations=visne@iterations,
                    perplexity=visne@perplexity,
                    theta=visne@theta,
                    seed=visne@seed,
                    channels=if (length(visne@channels)) visne@channels else list(),
                    populationSelections=population_selections
                )),
                encode="json",
                timeout(timeout)
    )

    return(create_visne_object(UserSession, parse(resp, "viSNE")))
})


#########################
# viSNE HELPER FUNCTIONS
#########################


#' @rdname visne
#' @aliases visne.helper.set_populations
#'
#' @details \code{visne.helper.set_populations} Set viSNE advanced analysis populations to be selected from an experiment and returns the new viSNE object with the new population selections. The population provided will be overwritten by the newly selected FCS files provided.
#' @examples \donttest{visne.helper.set_populations(visne=cyto_visne, population_id=1, fcs_files=c(1,2,3))
#' }
#' @export
visne.helper.set_populations <- function(visne, population_id=NA, fcs_files=NA)
{
    # Reset population selections
    if (is.na(population_id) && is.na(fcs_files))
    {
        visne@population_selections <- visne@population_selections[0,]
        return(visne)
    }

    visne@population_selections <- visne@population_selections[visne@population_selections$populationId != population_id,]
    row <- nrow(visne@population_selections)

    # Get population name from cached populations dataframe in visne object
    population_name <- visne@.available_populations[visne@.available_populations$gateSetId == population_id,]$name

    # Build the population selection row
    for (file in fcs_files)
    {
        row <- row+1
        file_name <- visne@.available_files[visne@.available_files$id == file,]$filename
        visne@population_selections[row,] <- list(as.integer(file), file_name, NA_integer_, NA_integer_, as.integer(population_id), population_name)
    }

    return(visne)
}


##########
# PRIVATE
##########


# Create viSNE object from viSNE json response
create_visne_object <- function(UserSession, visne_response)
{
    return(
        new("viSNE",
            visne_id=visne_response$visne$id,
            name=visne_response$visne$name,
            status=visne_response$visne$status,
            source_experiment=visne_response$visne$sourceExperiment,
            created_experiment=if (!is.null(visne_response$visne$createdExperiment)) visne_response$visne$createdExperiment else NA_integer_,
            sampling_total_count=visne_response$visne$settings$samplingTotalCount,
            sampling_target_type=visne_response$visne$settings$samplingTargetType,
            compensation_id=visne_response$visne$settings$compensationId,
            channels=visne_response$visne$settings$channelIds,
            iterations=visne_response$visne$settings$iterations,
            perplexity=visne_response$visne$settings$perplexity,
            theta=visne_response$visne$settings$theta,
            seed=visne_response$visne$settings$seed,
            population_selections=create_population_selections(visne_response$visne$settings$populationSelections),
            .available_channels=panels.list(UserSession, visne_response$visne$sourceExperiment),
            .available_files=fcs_files.list(UserSession, visne_response$visne$sourceExperiment),
            .available_populations=populations.list(UserSession, visne_response$visne$sourceExperiment))
    )
}


# Converts population selections output -> population selections dataframe
create_population_selections <- function(population_selections_output)
{
    population_selections_list <- list()

    # for each population
    for (population in population_selections_output)
    {
        # Create a dataframe
        temp_data <- do.call(rbind.data.frame, population[[4]])
        temp_data["name"] <- lapply(temp_data["name"], as.character) # Convert 'names' as factors -> characters
        # Add population ID and name to population-file selection
        temp_data$populationId <- apply(temp_data, 1, function(row) population[[1]])
        temp_data$populationName <- apply(temp_data, 1, function(row) population[[2]])
        population_selections_list <- c(population_selections_list, list(temp_data))
    }

    # Combine and return one dataframe of fold change group data
    population_selections <- do.call(rbind, population_selections_list)
    return(if (nrow(population_selections) != 0) population_selections
           else data.frame(id=integer(),
                           name=character(),
                           samplingCount=integer(),
                           eventCount=integer(),
                           populationId=integer(),
                           populationName=character(), stringsAsFactors=FALSE))
}


# Convert population selections dataframe -> population selections list for update
population_selections_dataframe_to_list <- function(population_selections_dataframe)
{
    # Check if empty population_selections_dataframe
    if (nrow(population_selections_dataframe) == 0)
    {
        return(list())
    }

    # Dataframe -> list with population[fcs_files]
    population_selections_list <- list()
    for (x in seq(nrow(population_selections_dataframe)))
    {
        population_selections_list[[as.character(population_selections_dataframe$populationId[[x]])]]$fcsFiles <- c(
            population_selections_list[[as.character(population_selections_dataframe$populationId[[x]])]]$fcsFiles,
            list(list(id=population_selections_dataframe$id[[x]])
            )
        )
    }

    # population[fcs_files] -> [population, selected, fcs_files]
    population_selections <- list()
    for (population in names(population_selections_list))
    {
        population_selections <- c(population_selections, list(list(id=as.integer(population), selected=TRUE, fcsFiles=population_selections_list[[population]]$fcsFiles)))
    }

    return(population_selections)
}

