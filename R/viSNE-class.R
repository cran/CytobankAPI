#' S4 viSNE Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A viSNE object that holds pertinent viSNE advanced analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank viSNE object, utilize the \link{visne.new} function, or any other \link[=visne]{viSNE endpoints that return viSNE objects documented in the 'Details' section}.
#' @slot created_experiment numeric representing the experiment that gets created from the viSNE analysis
#' @slot iterations numeric representing the number of times viSNE processes the dataset using its step-wise optimization algorithm, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-viSNE-Analysis#iterations}{learn more about how iterations affect viSNE results}
#' @slot perplexity dataframe representing a rough guess for the number of close neighbors any given cellular event will have, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-viSNE-Analysis#perplexity}{learn more about viSNE perplexity}
#' @slot population_selections dataframe representing which population(s) data will be sourced, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-viSNE-Analysis#populations}{learn more about selecting populations for viSNE}
#' @slot visne_id numeric representing the viSNE analysis ID
#' @slot sampling_total_count numeric representing the total number of events to sample for the viSNE analysis
#' @slot sampling_target_type character representing the event sampling type\cr
#' \emph{- choose one of the following : \code{("proportional", "equal")}}
#' @slot theta numeric representing the balance of speed and accuracy in the viSNE run compared to the original tSNE algorithm, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-viSNE-Analysis#theta}{learn more about viSNE theta}
#' @return A viSNE advanced analysis object
setClass("viSNE", contains="AdvancedAnalysis",
         representation(created_experiment="numeric",
                        iterations="numeric",
                        perplexity="numeric",
                        population_selections="data.frame",
                        sampling_total_count="numeric",
                        sampling_target_type="character",
                        theta="numeric",
                        visne_id="numeric"

         ),
         validity=function(object)
         {
             if (typeof(object@created_experiment) != "integer" ||
                 typeof(object@iterations) != "integer" ||
                 typeof(object@perplexity) != "double" ||
                 typeof(object@population_selections) != "list" ||
                 typeof(object@sampling_total_count) != "integer" ||
                 typeof(object@sampling_target_type) != "character" ||
                 typeof(object@theta) != "double" ||
                 typeof(object@visne_id) != "integer"

             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


