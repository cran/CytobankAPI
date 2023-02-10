# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' S4 PeacoQC Class
#'
#' @description A PeacoQC object that holds pertinent PeacoQC data QC run information
#' This class should never be called explicitly. If a user would like to create a new Cytobank PeacoQC object, utilize the \link{peacoqc.new} function, or any other \link[=peacoqc]{PeacoQC endpoints that return PeacoQC objects documented in the 'Details' section}.
#' @slot author character representing the author of the PeacoQC analysis
#' @slot attachment_id numeric representing the PeacoQC attachment to the source experiment containing the PeacoQC results
#' @slot channel_unique_identifiers list of character representing a list of unique channel identifiers
#' @slot compensation_id the compensation ID selected for the PeacoQC data QC
#' @slot completed logical representing whether or not the PeacoQC is complete
#' @slot consecutive_bins numeric if 'good' bins are located between bins that are removed, they will also be marked as 'bad'. Can be set to any integer between 1 and 50 (inclusive)
#' @slot detection_method character representing the method(s) used to detect and filter out anomalies.
#' \emph{- choose from the following : \code{("all" [default], "IT", "MAD")}}
#' @slot errors list of character representing a list of error messages of the PeacoQC
#' @slot failed logical representing whether or not the PeacoQC is failed
#' @slot fcs_files list of integers or character representing a list of FCS file IDs
#' @slot final_result character representing whether or not the PeacoQC is successful
#' @slot heatmap_attachment_id numeric representing the PeacoQC heatmap image attachment to the source experiment
#' @slot it_limit numeric representing the IsolationTree parameter. Higher values mean the IT method will be less strict. Can be set to any float between 0.2 and 1.0(inclusive)
#' @slot mad numeric representing the MAD parameter. Higher values mean the MAD method will be less strict. Can be set to any integer between 1 and 100 (inclusive)
#' @slot max_bins numeric representing the maximum number of bins that can be used in the cleaning process. If this value is lowered, larger bins will be made. Can be set to any integer between 40 and 1,000,000 (inclusive)
#' @slot name the name of the advanced analysis
#' @slot peaco_qc_id numeric representing the PeacoQC ID
#' @slot remove_margins if the value is true, they will remove margin events based on the internal description of the fcs file. Can be set to a boolean value
#' @slot source_experiment the source experiment ID the advanced analysis is associated with
#' @slot status character representing the status of the advanced analysis
#' @slot type character
#' @slot use_internal_scales_for_margins logical this parameter is required when removeMargins is set to true. Set to true, the events will transtorm with fcs file internal scales. Set to false, the events will transform with cytobank scales. Can be set to a boolean value
#' @slot validFcsFileIds list of integers or character representing a list of valid FCS file IDs can run PeacoQC
#' @return A PeacoQC object
setClass("PeacoQC",
         slots = c(
           author = "character",
           attachment_id = "numeric",
           channel_unique_identifiers = "list",
           compensation_id = "numeric",
           completed = "logical",
           consecutive_bins = "numeric",
           detection_method = "character",
           errors = "list",
           failed = "logical",
           fcs_files = "list",
           final_result = "character",
           heatmap_attachment_id = "numeric",
           it_limit = "numeric",
           mad = "numeric",
           max_bins = "numeric",
           name = "character",
           peaco_qc_id = "numeric",
           remove_margins = "logical",
           source_experiment = "numeric",
           status = "character",
           type = "character",
           use_internal_scales_for_margins = "logical",
           validFcsFileIds = "list"
         ),
         validity = function(object) {
           if (typeof(object@channel_unique_identifiers) != "list" ||
             typeof(object@fcs_files) != "list" ||
             typeof(object@validFcsFileIds) != "list" ||
             typeof(object@errors) != "list" ||
             typeof(object@remove_margins) != "logical" ||
             typeof(object@use_internal_scales_for_margins) != "logical" ||
             typeof(object@completed) != "logical" ||
             typeof(object@failed) != "logical" ||
             typeof(object@name) != "character" ||
             typeof(object@status) != "character" ||
             typeof(object@source_experiment) != "integer" ||
             typeof(object@compensation_id) != "integer" ||
             typeof(object@mad) != "integer" ||
             typeof(object@max_bins) != "integer" ||
             typeof(object@consecutive_bins) != "integer"
           )
           {
             return(FALSE)
           }
           return(TRUE)
         }
)

