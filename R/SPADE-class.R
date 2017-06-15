#' S4 SPADE Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A SPADE object that holds pertinent SPADE advanced analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank SPADE object, utilize the \link{spade.new} function, or any other \link[=spade]{SPADE endpoints that return SPADE objects documented in the 'Details' section}.
#' @slot created_experiment numeric representing the experiment that gets created from the SPADE analysis
#' @slot down_sampled_events_target numeric representing the percent OR absolute number (depends on 'down_sampled_events_type' slot) for downsampling occurring within the SPADE analysis, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Downsampling-Target}{learn more about SPADE density-dependent downsampling}
#' @slot down_sampled_events_type character representing the downsampling type for down_sampled_events_target, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Downsampling-Target}{learn more about SPADE density-dependent downsampling types}
#' \emph{- choose one of the following : \code{("percent" [default], "absolute_number")}}
#' @slot fold_change_groups dataframe representing the fold change groups within a SPADE analysis, \href{https://support.cytobank.org/hc/en-us/articles/206145497-SPADE-with-fold-change-overview-setup-and-analysis}{learn more about SPADE fold change groups}
#' @slot population_id numeric representing the population to run the SPADE analysis on, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Selecting-a-Population}{learn more about choosing a population for SPADE}
#' @slot spade_id numeric representing the SPADE analysis ID
#' @slot target_number_nodes numeric representing how many population nodes SPADE will seek out within the given data, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Target-Number-of-Nodes}{learn more about target number of nodes for SPADE}
#' @return A SPADE advanced analysis object
setClass("SPADE", contains="AdvancedAnalysis",
         representation(created_experiment="numeric",
                        down_sampled_events_target="numeric",
                        down_sampled_events_type="character",
                        fold_change_groups="data.frame",
                        population_id="numeric",
                        spade_id="numeric",
                        target_number_nodes="numeric"

         ),
         prototype(down_sampled_type="percent",
             target_number_nodes=NA_real_
         ),
         validity=function(object)
         {
             if (typeof(object@created_experiment) != "integer" ||
                 typeof(object@down_sampled_events_target) != "integer" ||
                 typeof(object@down_sampled_events_type) != "character" ||
                 typeof(object@fold_change_groups) != "list" ||
                 typeof(object@population_id) != "integer" ||
                 typeof(object@spade_id) != "integer" ||
                 typeof(object@target_number_nodes) != "integer"
             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })

