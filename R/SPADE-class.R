#' S4 SPADE Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A SPADE object that holds pertinent SPADE advanced analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank SPADE object, utilize the \link{spade.new} function.
#' @slot down_sampled_events_target numeric representing the percent of downsampling occurring within the SPADE analysis, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Downsampling-Target}{learn more about SPADE density-dependent downsampling}
#' @slot fold_change_groups dataframe representing the fold change groups within a SPADE analysis, \href{https://support.cytobank.org/hc/en-us/articles/206145497-SPADE-with-fold-change-overview-setup-and-analysis}{learn more about SPADE fold change groups}
#' @slot population_id numeric representing the population to run the SPADE analysis on, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Selecting-a-Population}{learn more about choosing a population for SPADE}
#' @slot spade_id numeric representing the SPADE analysis ID
#' @slot target_number_nodes numeric representing how many population nodes SPADE will seek out within the given data, \href{https://support.cytobank.org/hc/en-us/articles/115000597188-How-to-Configure-and-Run-a-SPADE-Analysis#Target-Number-of-Nodes}{learn more about target number of nodes for SPADE}
#' @return A SPADE advanced analysis object
setClass("SPADE", contains="AdvancedAnalysis",
         representation(down_sampled_events_target="numeric",
                        fold_change_groups="data.frame",
                        population_id="numeric",
                        spade_id="numeric",
                        target_number_nodes="numeric"

         ),
         prototype(target_number_nodes=NA_real_
         ),
         validity=function(object)
         {
             if (typeof(object@down_sampled_events_target) != "integer" ||
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

