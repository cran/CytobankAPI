# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' S4 DimensionalityReduction Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A Dimensionality Reduction object that holds pertinent Dimensionality Reduction advanced analysis run information.
#' This class should never be called explicitly. If a user would like to create a new Cytobank Dimensionality Reduction object, utilize the \link{dimensionality_reduction.new} function, or any other \link[=dimensionality_reduction]{Dimensionality Reduction endpoints that return Dimensionality Reduction objects documented in the 'Details' section}.
#' @slot analysis_id numeric representing the Dimensionality Reduction analysis ID
#' @slot type character representing the Dimensionality Reduction type (tSNE-CUDA, opt-SNE, UMAP, or viSNE)
#' @slot name character the name of the Dimensionality Reduction analysis
#' @slot status character representing the status of the Dimensionality Reduction analysis
#' @slot source_experiment numeric the source experiment ID the Dimensionality Reduction analysis is associated with
#' @slot created_experiment numeric representing the experiment that gets created from the Dimensionality Reduction analysis
#' @slot .available_channels the list of available channels based off the \link{panels.list} function
#' @slot .available_files the list of available files based off the \link{fcs_files.list} function
#' @slot .available_populations the list of available populations based off the \link{populations.list} function
#' @return A Dimensionality Reduction advanced analysis object
setClass("DimensionalityReduction",
         representation(analysis_id="numeric",
                        type="character",
                        name="character",
                        status="character",
                        source_experiment="numeric",
                        created_experiment="numeric",
                        .available_channels="list",
                        .available_files="data.frame",
                        .available_populations="data.frame"

         ),
         validity=function(object)
         {
             if (typeof(object@analysis_id) != "integer" ||
                 typeof(object@type) != "character" ||
                 typeof(object@name) != "character" ||
                 typeof(object@status) != "character" ||
                 typeof(object@source_experiment) != "integer" ||
                 typeof(object@created_experiment) != "integer" ||
                 typeof(object@.available_channels) != "list" ||
                 typeof(object@.available_files) != "list" ||
                 typeof(object@.available_populations) != "list"
             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


