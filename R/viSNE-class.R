# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' S4 viSNE Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A viSNE object that holds pertinent viSNE analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank Dimensionality Reduction object, utilize the \link{dimensionality_reduction.new} function, or any other \link[=dimensionality_reduction]{Dimensionality Reduction endpoints that return Dimensionality Reduction objects documented in the 'Details' section}.
#' @slot iterations numeric representing the number of times Dimensionality Reduction processes the dataset using its step-wise optimization algorithm, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis}{learn more about how iterations affect Dimensionality Reduction results}
#' @slot perplexity numeric representing a rough guess for the number of close neighbors any given cellular event will have, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis}{learn more about Dimensionality Reduction perplexity}
#' @slot channels list the channels selected for the Dimensionality Reduction analysis, this can be either a list of short channel IDs (integer) OR long channel names (character)
#' @slot compensation_id the compensation ID selected for the Dimensionality Reduction analysis
#' @slot population_selections dataframe representing which population(s) data will be sourced, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis#Selecting-Populations-and-Samples}{learn more about selecting populations for Dimensionality Reduction}
#' @slot sampling_total_count numeric representing the total number of events to sample for the Dimensionality Reduction analysis
#' @slot sampling_target_type character representing the event sampling type\cr
#' \emph{- choose one of the following : \code{("proportional", "equal")}}
#' @slot seed character representing the seed, Dimensionality Reduction picks a random seed each run, but if users want reproducible data, setting the same seed will allow them to do this
#' @slot theta numeric representing the balance of speed and accuracy in the Dimensionality Reduction run compared to the original tSNE algorithm, \href{https://support.cytobank.org/hc/en-us/articles/206439707-How-to-Configure-and-Run-a-Dimensionality\%20Reduction-Analysis}{learn more about Dimensionality Reduction theta}
#' @slot visne_id numeric representing the Dimensionality Reduction analysis ID
#' @return A Dimensionality Reduction advanced analysis object
setClass("viSNE", contains="DimensionalityReduction",
         representation(iterations="numeric",
                        perplexity="numeric",
                        channels="list",
                        compensation_id="numeric",
                        population_selections="data.frame",
                        sampling_total_count="numeric",
                        sampling_target_type="character",
                        seed="character",
                        theta="numeric",
                        visne_id="numeric"

         ),
         validity=function(object)
         {
             if (typeof(object@iterations) != "integer" ||
                 typeof(object@perplexity) != "double" ||
                 typeof(object@channels) != "list" ||
                 typeof(object@compensation_id) != "integer" ||
                 typeof(object@population_selections) != "list" ||
                 typeof(object@sampling_total_count) != "integer" ||
                 typeof(object@sampling_target_type) != "character" ||
                 typeof(object@seed) != "character" ||
                 typeof(object@theta) != "double" ||
                 typeof(object@visne_id) != "integer"
             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


