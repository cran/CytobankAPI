#' S4 CITRUS Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A CITRUS object that holds pertinent CITRUS advanced analysis run information, \href{https://support.cytobank.org/hc/en-us/articles/226940667-Overview-of-CITRUS}{learn more about CITRUS}.
#' This class should never be called explicitly. If a user would like to create a new Cytobank CITRUS object, utilize the \link{citrus.new} function, or any other \link[=citrus]{CITRUS endpoints that return CITRUS objects documented in the 'Details' section}.
#' @slot associated_models list representing statistical methods used to discover stratifying signatures from clustered data features that explain differences between sample groups, \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#Association_Models}{learn more about CITRUS association models}\cr
#' \emph{- choose one of the following : \code{("sam", "pamr" [default], "glmnet")}}
#' @slot attachment_id numeric representing the CITRUS attachment ID
#' @slot cross_validation_folds numeric representing the regulation threshold, controlling the number of features in the model (only applies to PAM, LASSO), \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#cross_validation_fold}{learn more about CITRUS cross validation folds}
#' @slot citrus_id numeric representing the CITRUS analysis ID
#' @slot cluster_characterization character representing the principle for analyzing and quantifying individual samples, \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#Cluster_Characterization}{learn more about CITRUS cluster characterization}\cr
#' \emph{- choose one of the following : \code{("abundance" [default], "medians")}}
#' @slot event_sampling_method character representing the sampling method, \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#Event_Sampling}{learn more about CITRUS event sampling methods}\cr
#' \emph{- choose one of the following : \code{("equal" [default], "max-per-file")}}
#' @slot events_per_file numeric representing the number of events taken from each sample
#' @slot false_discovery_rate numeric representing the false discovery rate (only applies to PAM, SAM), \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#False_Discovery_Rate_(FDR)}{learn more about CITRUS false discovery rate}
#' @slot file_grouping numeric dataframe representing which group samples belong to, \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#Assigning_Sample_Groups}{learn more about CITRUS file grouping, the core functionality of CITRUS}
#' @slot minimum_cluster_size numeric representing the number of nodes, \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#Minimum_Cluster_Size}{learn more about CITRUS minimum cluster size}
#' @slot normalize_scales logical representing whether or not to normalize channels, \href{https://support.cytobank.org/hc/en-us/articles/226678087-How-to-Configure-and-Run-a-CITRUS-Analysis#Normalize_Scales}{learn more about normalizing CITRUS scales}
#' @slot plot_theme character representing the background color of images and figures within the CITRUS results\cr
#' \emph{- choose one of the following : \code{("white" [default], "black")}}
#' @slot population_id dataframe representing a population \strong{gate set ID}
#' @slot statistics_channels list representing the statistics channels used for the 'median' cluster characterization, these channels should not be selected for clustering
#' @return A CITRUS advanced analysis object
setClass("CITRUS", contains="AdvancedAnalysis",
         representation(citrus_id="numeric",
                        population_id="numeric",
                        file_grouping="data.frame",
                        association_models="list",
                        cluster_characterization="character",
                        statistics_channels="list",
                        event_sampling_method="character",
                        events_per_file="numeric",
                        minimum_cluster_size="numeric",
                        cross_validation_folds="numeric",
                        false_discovery_rate="numeric",
                        normalize_scales="logical",
                        plot_theme="character",
                        attachment_id="numeric"

         ),
         prototype(attachment_id=NA_integer_
         ),
         validity=function(object)
         {
             if (typeof(object@citrus_id) != "integer" ||
                 typeof(object@population_id) != "integer" ||
                 typeof(object@file_grouping) != "list" ||
                 typeof(object@association_models) != "list" ||
                 typeof(object@cluster_characterization) != "character" ||
                 typeof(object@statistics_channels) != "list" ||
                 typeof(object@event_sampling_method) != "character" ||
                 typeof(object@events_per_file) != "integer" ||
                 typeof(object@minimum_cluster_size) != "integer" ||
                 typeof(object@cross_validation_folds) != "integer" ||
                 typeof(object@false_discovery_rate) != "integer" ||
                 typeof(object@normalize_scales) != "logical" ||
                 typeof(object@plot_theme) != "character" ||
                 typeof(object@attachment_id) != "integer"

             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


