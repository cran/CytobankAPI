# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Automatic gating Endpoints
#' , , , , , , , ,
#' @name autogating
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param analysis_type character representing the type of ananlysis: auto_gate_train or auto_gate_inference
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- drop.upload : \code{("default", "raw")}}\cr
#' \emph{- \code{dataframe}: converts the file internal compensation matrix output to a dataframe}
#' @param analysis_id integer representing the id of an autogating analysis
#' @param name character representing the name of an autogating analysis
#' @param fcsFileIds vector/list representing the id list of FCS files
#' @param gateSetIds vector/list representing the id list of Cytobank gate set
#' @param createBlindTestExperiment boolean A child experiment will be automatically created,
#'                                          containing the subset of FCS files that were assigned to the blind test set.
#'                                          For every predicted population, the files now contain one additional
#'                                          parameter following the naming convention of auto_gate_Population name.
#' @param desiredEventsPerFile integer Only applies if eventSamplingMethod is set to equal. Defaults to 50,000.
#'                                     It is the number of desired events to sample per file, but if the selected
#'                                     population for any selected file has less total events than the specified number,
#'                                     that quantity will be used instead.
#' @param desiredTotalEvents integer Only applies if eventSamplingMethod is set to proportional. Defaults to 5,000,000.
#'                                   Represents the total desired number of events to sample amongst all selected files,
#'                                   whilst keeping the numbers per file proportional to the total number of events in
#'                                   the selected population for that file. If any file has less events in the selected
#'                                   population than possible to make a perfectly proportional sampling to add up to the
#'                                   desired total, all of the events in the file will be used instead.
#' @param eventSamplingMethod character Valid options are proportional, equal, or all. Defaults to equal.
#'                                      If eventSamplingMethod is set to all, all events for the selected population
#'                                      from all selected files will be used, without any further subsampling.
#' @param learningMagnification integer By increasing the magnification, the user can determine how many different
#'                                      models are being trained using different parameters on the same training data.
#'                                      The model with the highest KPI will be returned to the user.
#'                                      With a magnification greater than 1, you may be able to influence the model
#'                                      selection to return a model performing better on your population of interest,
#'                                      but usually not significant. Of note, increasing the magnification also causes
#'                                      a proportional increase of the runtime. It may also cause the run to crash
#'                                      due to memory constraints if there are millions of events.
#' @param optimalClusters integer The best estimate of the number of distinct groups of files amongst those selected.
#'                                Usually, this aligns with how you would sample tag your files into different
#'                                conditions or time points. It helps the algorithm pick representative samples and
#'                                perform better. There is an option to create an experiment with blind test files and
#'                                their inferred populations. It can make it easier to visually evaluate model
#'                                performance.
#' @param randomSeed integer Accepts a positive integer value and sets a specific random seed to that value.
#'                           If this parameter is not specified or set to 0, autoSeed will automatically be set to true,
#'                           and a seed value will be randomly chosen, so that afterward it can be referred to for
#'                           reproducing the analysis results.
#' @param cloneGatesFromParent boolean The created child experiment will contain a copy of all gates & populations
#'                                     already present in the parent experiment
#' @param trainedModelAnalysisId character The ID of the Autogate Training analysis that contains the model that
#'                                         the inference run will use.
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#'
#' @examples \dontrun{
#' # Create train analysis
#' autogating_train_analysis <- autogating.list_autogating_analyses_of_type(cyto_session,
#'                              p_experiment_id,
#'                              "auto_gate_train")
#' # Update train settings
#' autogating.update_autogating_training_analysis_details(cyto_session, p_experiment_id,
#'                     autogating_train_analysis$id,
#'                     FALSE, 39139, 100001, "proportional",
#'                     c(114386,114373,114383,114374,114384,114387,114385,114377,114382,114375),
#'                     c(4,3,1,11,10), 1, 2, 1)
#' # Run analysis
#' autogating.run_autogating_analysis(cyto_session, p_experiment_id, autogating_train_analysis$id)
#' # Create inference analysis
#' autogating_inference_analysis <- autogating.list_autogating_analyses_of_type(cyto_session,
#'                                             p_experiment_id,
#'                                             "auto_gate_inference")
#' # Update inference settings
#' autogating.update_autogating_inference_analysis_details(cyto_session, p_experiment_id,
#'                                     autogating_inference_analysis$id, FALSE,
#'                                     c(114376,114378,114379,114380,114381,114388,114389,114390),
#'                                     autogating_train_analysis$id)
#' # Run analysis
#' autogating.run_autogating_analysis(cyto_session, p_experiment_id,
#'                                    autogating_inference_analysis$id)
#' }
NULL

setGeneric("autogating.list_autogating_analyses_of_type",
           function(UserSession, experiment_id, analysis_type, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.list_autogating_analyses_of_type")
})
#' @rdname autogating
#' @aliases autogating.list_autogating_analyses_of_type
#'
#' @details \code{autogating.list_autogating_analyses_of_type}
#' @examples \dontrun{autogating.list_autogating_analyses_of_type(cyto_session, 22, "auto_gate_train")
#' }
#' @export
setMethod("autogating.list_autogating_analyses_of_type", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_type, output="default", timeout=UserSession@long_timeout)
{
    resp <- GET(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_type),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.show_autogating_analysis_details",
           function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.show_autogating_analysis_details")
})
#' @rdname autogating
#' @aliases autogating.show_autogating_analysis_details
#'
#' @details \code{autogating.show_autogating_analysis_details}
#' @examples \dontrun{autogating.show_autogating_analysis_details(cyto_session, 22, 10)
#' }
#' @export
setMethod("autogating.show_autogating_analysis_details", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    resp <- GET(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_id,
                       "?include_settings=1"),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.create_autogating_analysis",
           function(UserSession, experiment_id, analysis_type, name, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.create_autogating_analysis")
})
#' @rdname autogating
#' @aliases autogating.create_autogating_analysis
#'
#' @details \code{autogating.create_autogating_analysis} Create a new automatic gating analysis of
#'                                                       the specified type (auto_gate_train or auto_gate_inference).
#' @examples \dontrun{autogating.create_autogating_analysis(cyto_session, 22, "auto_gate_train",
#'                                                         "My auto gating train analysis")
#' }
#' @export
setMethod("autogating.create_autogating_analysis", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_type, name, output="default", timeout=UserSession@long_timeout)
{
    resp <- POST(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_type),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(name = name),
                 encode="json",
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.update_autogating_training_analysis_details",
           function(UserSession, experiment_id, analysis_id, createBlindTestExperiment, desiredEventsPerFile,
                    desiredTotalEvents, eventSamplingMethod, fcsFileIds, gateSetIds, learningMagnification,
                    optimalClusters, randomSeed, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.update_autogating_training_analysis_details")
})
#' @rdname autogating
#' @aliases autogating.update_autogating_training_analysis_details
#'
#' @details \code{autogating.update_autogating_training_analysis_details}
#' @examples \dontrun{autogating.update_autogating_training_analysis_details(
#'                                          cyto_session, 22, 10, FALSE, 5000, 100000,
#'                                          "proportional",
#'                                          c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
#'                                          c(3, 4), 1, 2, NULL)
#' }
#' @export
setMethod("autogating.update_autogating_training_analysis_details", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, createBlindTestExperiment, desiredEventsPerFile,
                   desiredTotalEvents, eventSamplingMethod, fcsFileIds, gateSetIds, learningMagnification,
                   optimalClusters, randomSeed, output="default", timeout=UserSession@long_timeout)
{
    body <- list(options =
                   list(createBlindTestExperiment = createBlindTestExperiment,
                        desiredEventsPerFile = desiredEventsPerFile,
                        desiredTotalEvents = desiredTotalEvents,
                        eventSamplingMethod = eventSamplingMethod,
                        fcsFileIds = fcsFileIds,
                        gateSetIds = gateSetIds,
                        learningMagnification = learningMagnification,
                        optimalClusters = optimalClusters,
                        randomSeed = randomSeed
                   ))
    resp <- PUT(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/auto_gate_train/",
                       analysis_id),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=body,
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.update_autogating_inference_analysis_details",
           function(UserSession, experiment_id, analysis_id, cloneGatesFromParent, fcsFileIds, trainedModelAnalysisId,
                    output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.update_autogating_inference_analysis_details")
})
#' @rdname autogating
#' @aliases autogating.update_autogating_inference_analysis_details
#'
#' @details \code{autogating.update_autogating_inference_analysis_details}
#' @examples \dontrun{autogating.update_autogating_inference_analysis_details(
#'                                       cyto_session, 22, 10, FALSE, c(21, 22, 23), 10)
#' }
#' @export
setMethod("autogating.update_autogating_inference_analysis_details", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, cloneGatesFromParent, fcsFileIds, trainedModelAnalysisId,
                   output="default", timeout=UserSession@long_timeout)
{
    body <- list(options =
                   list(
                        cloneGatesFromParent = cloneGatesFromParent,
                        fcsFileIds = fcsFileIds,
                        trainedModelAnalysisId = trainedModelAnalysisId
                   ))
    resp <- PUT(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/auto_gate_inference/",
                       analysis_id),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=body,
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.delete_autogating_analysis",
           function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.delete_autogating_analysis")
})
#' @rdname autogating
#' @aliases autogating.delete_autogating_analysis
#'
#' @details \code{autogating.delete_autogating_analysis}
#' @examples \dontrun{autogating.delete_autogating_analysis(cyto_session, 22, 10)
#' }
#' @export
setMethod("autogating.delete_autogating_analysis", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, timeout=UserSession@long_timeout)
{
    resp <- DELETE(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_id),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "autogating")
    }

    return(paste("Autogating (ID=", analysis_id, ") successfully deleted.", sep=""))
})


setGeneric("autogating.copy_autogating_analysis_settings",
           function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.copy_autogating_analysis_settings")
})
#' @rdname autogating
#' @aliases autogating.copy_autogating_analysis_settings
#'
#' @details \code{autogating.copy_autogating_analysis_settings}
#' @examples \dontrun{autogating.copy_autogating_analysis_settings(cyto_session, 22, 10)
#' }
#' @export
setMethod("autogating.copy_autogating_analysis_settings", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    resp <- POST(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_id,
                        "/copy_settings"),
                   add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                   timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.rename_autogating_analysis",
           function(UserSession, experiment_id, analysis_id, name, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.rename_autogating_analysis")
})
#' @rdname autogating
#' @aliases autogating.rename_autogating_analysis
#'
#' @details \code{autogating.rename_autogating_analysis}
#' @examples \dontrun{autogating.rename_autogating_analysis(cyto_session, 22, 10, "New new of analysis")
#' }
#' @export
setMethod("autogating.rename_autogating_analysis", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, name, output="default", timeout=UserSession@long_timeout)
{
    resp <- PUT(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_id, "/rename"),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=list(name=name),
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.run_autogating_analysis",
           function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.run_autogating_analysis")
})
#' @rdname autogating
#' @aliases autogating.run_autogating_analysis
#'
#' @details \code{autogating.run_autogating_analysis}
#' @examples \dontrun{autogating.run_autogating_analysis(cyto_session, 22, 10)
#' }
#' @export
setMethod("autogating.run_autogating_analysis", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    resp <- POST(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_id, "/run"),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})


setGeneric("autogating.show_autogating_analysis_status",
           function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("autogating.show_autogating_analysis_status")
})
#' @rdname autogating
#' @aliases autogating.show_autogating_analysis_status
#'
#' @details \code{autogating.show_autogating_analysis_status}
#' @examples \dontrun{autogating.show_autogating_analysis_status(cyto_session, 22, 10)
#' }
#' @export
setMethod("autogating.show_autogating_analysis_status", signature(UserSession="UserSession"),
          function(UserSession, experiment_id, analysis_id, output="default", timeout=UserSession@long_timeout)
{
    resp <- GET(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/", analysis_id, "/status"),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "autogating")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "autogating"))
    }
})
