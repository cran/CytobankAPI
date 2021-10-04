# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' S4 UMAP Class
#'
#' @description A UMAP object that holds pertinent UMAP advanced analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank UMAP object, utilize the \link{dimensionality_reduction.new} function, or any other \link[=dimensionality_reduction]{UMAP endpoints that return UMAP objects documented in the 'Details' section}.
#' @slot clustering_channels list the channels selected for the Dimensionality Reduction analysis, this can be either a list of short channel IDs (integer) OR long channel names (character)
#' @slot collapse_outliers logical Dimension values that are significant outliers (z-score > 3) will be collapsed to be equal to the min or max value. Try this if you observe that most of the data appears squished within small region
#' @slot desired_events_per_file numeric representing the number of desired events per file
#' @slot desired_total_events numeric representing the number of desired total events per file
#' @slot event_sampling_method character representing the name of event sampling method will be used, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis\%23Event-Sampling#Event-Sampling}{learn more about Event Sampling for Dimensionality Reduction analysis}
#' @slot fcsfile_ids list representing the fcs file ids
#' @slot gateset_id numeric representing the selected gate id
#' @slot min_distance numeric the effective minimum distance between embedded points, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis\%23Event-Sampling#UMAP}{learn more about minimum distance for UMAP analysis}
#' @slot num_neighbors numeric the size of local neighborhood (in terms of number of neighboring sample points) used for manifold approximation, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis#UMAP}{learn more about number of neighbors for UMAP analysis}
#' @slot normalize_scales logical representing whether or not to normalize scales
#' @return A UMAP advanced analysis object
setClass("UMAP", contains="DimensionalityReduction",
        representation(clustering_channels="list",
                       collapse_outliers="logical",
                       num_events_to_actually_sample="numeric",
                       desired_events_per_file="numeric",
                       desired_total_events="numeric",
                       event_sampling_method="character",
                       fcsfile_ids="list",
                       gateset_id="numeric",
                       min_distance="numeric",
                       num_neighbors="numeric",
                       normalize_scales="logical"
         ),
         validity=function(object)
         {
             if (typeof(object@clustering_channels) != "list" ||
                 typeof(object@collapse_outliers) != "logical" ||
                 typeof(object@num_events_to_actually_sample) != "integer" ||
                 typeof(object@desired_events_per_file) != "integer" ||
                 typeof(object@desired_total_events) != "integer" ||
                 typeof(object@event_sampling_method) != "character" ||
                 typeof(object@fcsfile_ids) != "list" ||
                 typeof(object@gateset_id) != "integer" ||
                 typeof(object@min_distance) != "double" ||
                 typeof(object@num_neighbors) != "integer" ||
                 typeof(object@normalize_scales) != "logical"
             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


