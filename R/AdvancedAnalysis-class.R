#' S4 Advanced Analysis Class
#'
#' @description An Advanced Analysis object that is a parent class to all advanced analysis algorithms.
#' This class should never be called explicitly. Its purpose is to act as a parent class for advanced analyses.
#' @slot channels the channels selected for the advanced analysis, this can be either a list of short channel IDs (integer) OR long channel names (character)
#' @slot compensation_id the compensation ID selected for the advanced analysis
#' @slot name the name of the advanced analysis
#' @slot source_experiment the source experiment ID the advanced analysis is associated with
#' @slot .available_channels the list of available channels based off the \link{panels.list} function
#' @slot .available_files the list of available files based off the \link{fcs_files.list} function
#' @slot .available_populations the list of available populations based off the \link{populations.list} function
#' @return An Advanced Analysis object
setClass("AdvancedAnalysis",
         representation(channels="list",
                        compensation_id="numeric",
                        name="character",
                        source_experiment="numeric",
                        status="character",
                        .available_channels="list",
                        .available_files="data.frame",
                        .available_populations="data.frame"
         ),
         prototype(compensation_id=NA_real_,
                   name=NA_character_
         ),
         validity=function(object)
         {
             if (typeof(object@channels) != "list" ||
                 typeof(object@compensation_id) != "integer" ||
                 typeof(object@name) != "character" ||
                 typeof(object@source_experiment) != "integer" ||
                 typeof(object@status) != "character" ||
                 typeof(object@.available_channels) != "list" ||
                 typeof(object@.available_files) != "list" ||
                 typeof(object@.available_populations) != "list"
                 )
             {
                 return(FALSE)
             }
             return(TRUE)
         })

