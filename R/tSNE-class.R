# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' S4 tSNE Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A tSNE object that holds pertinent tSNE advanced analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank Dimensionality Reduction object, utilize the \link{dimensionality_reduction.new} function, or any other \link[=dimensionality_reduction]{Dimensionality Reduction endpoints that return Dimensionality Reduction objects documented in the 'Details' section}.
#' @slot iterations numeric representing the number of times Dimensionality Reduction processes the dataset using its step-wise optimization algorithm, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis#tSNE-CUDA}{learn more about how iterations affect Dimensionality Reduction results}
#' @slot perplexity numeric representing a rough guess for the number of close neighbors any given cellular event will have, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis#tSNE-CUDA}{learn more about Dimensionality Reduction perplexity}
#' @slot auto_iterations logical representing whether or not to set auto interations
#' @slot auto_learning_rate logical representing whether or not to set auto learning rate
#' @slot clustering_channels list the channels selected for the Dimensionality Reduction analysis, this can be either a list of short channel IDs (integer) OR long channel names (character)
#' @slot desired_events_per_file numeric representing the number of desired events per file
#' @slot desired_total_events numeric representing the number of desired total events per file
#' @slot early_exaggeration numeric representing how tight natural clusters in the original space are in the embedded space and how much space will be between them
#' @slot event_sampling_method character representing the name of event sampling method will be used, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis#Event-Sampling}{learn more about Event Sampling for Dimensionality Reduction analysis}
#' @slot fcsfile_ids list representing the fcs file ids
#' @slot gateset_id numeric representing the selected gate id
#' @slot learning_rate numeric representing the learning rate
#' @slot normalize_scales logical representing whether or not to normalize scales
#' @return A Dimensionality Reduction advanced analysis object
setClass("tSNE", contains="DimensionalityReduction",
        representation(iterations="numeric",
                       perplexity="numeric",
                       auto_iterations="logical",
                       auto_learning_rate="logical",
                       clustering_channels="list",
                       num_events_to_actually_sample="numeric",
                       desired_events_per_file="numeric",
                       desired_total_events="numeric",
                       early_exaggeration="numeric",
                       event_sampling_method="character",
                       fcsfile_ids="list",
                       gateset_id="numeric",
                       learning_rate="numeric",
                       normalize_scales="logical"
         ),
         validity=function(object)
         {
             if (typeof(object@iterations) != "integer" ||
                 typeof(object@perplexity) != "integer" ||
                 typeof(object@auto_iterations) != "logical" ||
                 typeof(object@auto_learning_rate) != "logical" ||
                 typeof(object@clustering_channels) != "list" ||
                 typeof(object@num_events_to_actually_sample) != "integer" ||
                 typeof(object@desired_events_per_file) != "integer" ||
                 typeof(object@desired_total_events) != "integer" ||
                 typeof(object@early_exaggeration) != "integer" ||
                 typeof(object@event_sampling_method) != "character" ||
                 typeof(object@fcsfile_ids) != "list" ||
                 typeof(object@gateset_id) != "integer" ||
                 typeof(object@learning_rate) != "integer" ||
                 typeof(object@normalize_scales) != "logical"
             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


