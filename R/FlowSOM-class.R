# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' S4 FlowSOM Class
#'
# NEED TO UPDATE DESCRIPTION/SLOTS/EXAMPLES
#' @description A FlowSOM object that holds pertinent FlowSOM advanced analysis run information, \href{https://support.cytobank.org/hc/en-us/articles/360018969712}{learn more about FlowSOM}.
#' This class should never be called explicitly. If a user would like to create a new Cytobank FlowSOM object, utilize the \link{flowsom.new} function, or any other \link[=flowsom]{FlowSOM endpoints that return FlowSOM objects documented in the 'Details' section}.
#' @slot attachment_id numeric representing the FlowSOM attachment to the source experiment containing the FlowSOM results
#' @slot author character representing the author of the FlowSOM analysis
#' @slot auto_seed logical representing whether to set an auto seed value or not
#' @slot canceled logical representing whether or not the FlowSOM analysis is canceled
#' @slot channels_to_plot list representing short channel IDs corresponding to channels to output channel-colored MST plots, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Channels-to-plot}{learn more about FlowSOM PDF output}
#' @slot clustering_method character representing the clustering method\cr
#' \emph{- choose from the following : \code{("consensus" [default], "hierarchical", "kmeans")}}
#' @slot cluster_size_type character representing the cluster size type, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}\cr
#' \emph{- choose from the following : \code{("both", "fixed", "relative" [default])}}
#' @slot completed logical representing whether or not the FlowSOM analysis is complete
#' @slot created_experiment numeric representing the experiment that gets created from the FlowSOM analysis
#' @slot desired_events_per_file numeric representing the number of desired events per file if \code{event_sampling_method} is set to \code{equal}, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Event-Count-Sampling}{learn more about FlowSOM event sampling methods}
#' @slot desired_total_events numeric representing the total desired number of events to sample amongst all selected files if \code{event_sampling_method} is set to \code{proportional}, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Event-Count-Sampling}{learn more about FlowSOM event sampling methods}
#' @slot event_sampling_method character representing the FlowSOM sampling method, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Event-Count-Sampling}{learn more about FlowSOM event sampling methods}\cr
#' \emph{- choose from the following : \code{("all", "equal" [default], "proportional")}}
#' @slot expected_clusters numeric representing the number of expected clusters, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Choosing-a-Target-Number-of-Clusters}{learn more about choosing target number of clusters for FlowSOM}
#' @slot expected_metaclusters numeric representing the expected number of metaclusters \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Choosing-a-Target-Number-of-Metaclusters}{learn more about choosing target number of metaclusters for FlowSOM}
#' @slot external_som_analysis_info character representing FlowSOM analysis information
#' @slot external_som_analysis_id character representing the ID of a corresponding FlowSOM analysis ID if \code{som_creation_method} set to \code{"import_existing"}
#' @slot external_som_attachment_id character representing the ID of a corresponding completed FlowSOM analysis if \code{som_creation_method} is set to \code{import_existing}
#' @slot fcs_files list of integers or character representing a list of FCS file IDs
#' @slot final_result character representing whether or not the FlowSOM analysis is successful
#' @slot fixed_cluster_size integer representing fixed cluster size if \code{cluster_size_type} set to \code{"fixed"} or \code{"both"}\href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}
#' @slot flowsom_id numeric representing the FlowSOM analysis ID
#' @slot gate_set_names_to_label list of character representing populations to label in the population pie plots, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}
#' @slot iterations numeric representing the number of times FlowSOM processes the dataset using its step-wise optimization algorithm, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Configuring-Iterations}{learn more about iterations in FlowSOM}
#' @slot max_relative_cluster_size numeric representing the max relative cluster size (only applicable if \code{cluster_size_type} set to \code{"relative"} or \code{"both"}, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}
#' @slot normalize_scales logical representing whether or not to normalize scales
#' @slot num_events_to_actually_sample numeric representing the events actually sampled
#' @slot num_fcs_files numeric representing the number of FCS files
#' @slot output_file_type character representing the output file type\cr
#' \emph{- choose from the following : \code{("both", "pdf" [default], "png")}}
#' @slot population_id integer representing a population \strong{gate set ID}
#' @slot random_seed numeric representing the seed value \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Setting-the-Seed}{learn more about setting the seed for FlowSOM}
#' @slot show_background_on_legend logical representing whether or not to show background on legend, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}
#' @slot show_background_on_channel_colored_msts logical representing whether or not to show background on channel colored MSTs, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}
#' @slot show_background_on_population_pies logical representing whether or not to show background on population pies, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}
#' @slot som_creation_method character representing the FlowSOM creation method, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#SOM-Creation}{learn more about SOM creationg methods for FlowSOM}\cr
#' \emph{- choose from the following : \code{("create_new" [default], "import_existing")}}
#' @slot type character
#' @return A FlowSOM advanced analysis object
setClass("FlowSOM", contains="AdvancedAnalysis",
         representation(author="character",
                        type="character",
                        flowsom_id="numeric",
                        selected_population_name="character",
                        population_id="numeric",
                        num_fcs_files="numeric",
                        fcs_files="list",
                        event_sampling_method="character",
                        desired_events_per_file="numeric",
                        desired_total_events="numeric",
                        sampled_event_total="numeric",
                        num_events_to_actually_sample="numeric",
                        random_seed="numeric",
                        som_creation_method="character",
                        clustering_method="character",
                        expected_metaclusters="numeric",
                        expected_clusters="numeric",
                        iterations="numeric",
                        normalize_scales="logical",
                        created_experiment="numeric",
                        attachment_id="numeric",
                        auto_seed="logical",
                        external_som_analysis_info="character",
                        external_som_analysis_id="character",
                        channels_to_plot="list",
                        cluster_size_type="character",
                        fixed_cluster_size="numeric",
                        gate_set_names_to_label="list",
                        max_relative_cluster_size="numeric",
                        output_file_type="character",
                        show_background_on_legend="logical",
                        show_background_on_channel_colored_msts="logical",
                        show_background_on_population_pies="logical",
                        final_result="character",
                        completed="logical",
                        canceled="logical"
         ),
         validity=function(object)
         {
             if (typeof(object@channels_to_plot) != "list" ||
                 typeof(object@cluster_size_type) != "character" ||
                 typeof(object@clustering_method) != "character" ||
                 typeof(object@desired_events_per_file) != "integer" ||
                 typeof(object@desired_total_events) != "integer" ||
                 typeof(object@event_sampling_method) != "character" ||
                 typeof(object@expected_clusters) != "integer" ||
                 typeof(object@expected_metaclusters) != "integer" ||
                 typeof(object@external_som_analysis_info) != "character" ||
                 typeof(object@external_som_analysis_id) != "character" ||
                 typeof(object@fcs_files) != "list" ||
                 typeof(object@fixed_cluster_size) != "integer" ||
                 typeof(object@gate_set_names_to_label) != "list" ||
                 typeof(object@iterations) != "integer" ||
                 typeof(object@max_relative_cluster_size) != "integer" ||
                 typeof(object@normalize_scales) != "logical" ||
                 typeof(object@output_file_type) != "character" ||
                 typeof(object@population_id) != "integer" ||
                 typeof(object@random_seed) != "integer" ||
                 typeof(object@show_background_on_legend) != "logical" ||
                 typeof(object@show_background_on_channel_colored_msts) != "logical" ||
                 typeof(object@show_background_on_population_pies) != "logical" ||
                 typeof(object@som_creation_method) != "character"
             )
             {
                 return(FALSE)
             }
             return(TRUE)
         })


