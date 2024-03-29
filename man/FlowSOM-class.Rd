% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FlowSOM-class.R
\docType{class}
\name{FlowSOM-class}
\alias{FlowSOM-class}
\title{S4 FlowSOM Class}
\value{
A FlowSOM advanced analysis object
}
\description{
A FlowSOM object that holds pertinent FlowSOM advanced analysis run information, \href{https://support.cytobank.org/hc/en-us/articles/360018969712}{learn more about FlowSOM}.
This class should never be called explicitly. If a user would like to create a new Cytobank FlowSOM object, utilize the \link{flowsom.new} function, or any other \link[=flowsom]{FlowSOM endpoints that return FlowSOM objects documented in the 'Details' section}.
}
\section{Slots}{

\describe{
\item{\code{attachment_id}}{numeric representing the FlowSOM attachment to the source experiment containing the FlowSOM results}

\item{\code{author}}{character representing the author of the FlowSOM analysis}

\item{\code{auto_seed}}{logical representing whether to set an auto seed value or not}

\item{\code{canceled}}{logical representing whether or not the FlowSOM analysis is canceled}

\item{\code{channels_to_plot}}{list representing short channel IDs corresponding to channels to output channel-colored MST plots, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Channels-to-plot}{learn more about FlowSOM PDF output}}

\item{\code{clustering_method}}{character representing the clustering method\cr
\emph{- choose from the following : \code{("consensus" [default], "hierarchical", "kmeans")}}}

\item{\code{cluster_size_type}}{character representing the cluster size type, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}\cr
\emph{- choose from the following : \code{("both", "fixed", "relative" [default])}}}

\item{\code{completed}}{logical representing whether or not the FlowSOM analysis is complete}

\item{\code{created_experiment}}{numeric representing the experiment that gets created from the FlowSOM analysis}

\item{\code{desired_events_per_file}}{numeric representing the number of desired events per file if \code{event_sampling_method} is set to \code{equal}, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Event-Count-Sampling}{learn more about FlowSOM event sampling methods}}

\item{\code{desired_total_events}}{numeric representing the total desired number of events to sample amongst all selected files if \code{event_sampling_method} is set to \code{proportional}, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Event-Count-Sampling}{learn more about FlowSOM event sampling methods}}

\item{\code{event_sampling_method}}{character representing the FlowSOM sampling method, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Event-Count-Sampling}{learn more about FlowSOM event sampling methods}\cr
\emph{- choose from the following : \code{("all", "equal" [default], "proportional")}}}

\item{\code{expected_clusters}}{numeric representing the number of expected clusters, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Choosing-a-Target-Number-of-Clusters}{learn more about choosing target number of clusters for FlowSOM}}

\item{\code{expected_metaclusters}}{numeric representing the expected number of metaclusters \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Choosing-a-Target-Number-of-Metaclusters}{learn more about choosing target number of metaclusters for FlowSOM}}

\item{\code{external_som_analysis_info}}{character representing FlowSOM analysis information}

\item{\code{external_som_analysis_id}}{character representing the ID of a corresponding FlowSOM analysis ID if \code{som_creation_method} set to \code{"import_existing"}}

\item{\code{external_som_attachment_id}}{character representing the ID of a corresponding completed FlowSOM analysis if \code{som_creation_method} is set to \code{import_existing}}

\item{\code{fcs_files}}{list of integers or character representing a list of FCS file IDs}

\item{\code{final_result}}{character representing whether or not the FlowSOM analysis is successful}

\item{\code{fixed_cluster_size}}{integer representing fixed cluster size if \code{cluster_size_type} set to \code{"fixed"} or \code{"both"}\href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}}

\item{\code{flowsom_id}}{numeric representing the FlowSOM analysis ID}

\item{\code{gate_set_names_to_label}}{list of character representing populations to label in the population pie plots, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}}

\item{\code{iterations}}{numeric representing the number of times FlowSOM processes the dataset using its step-wise optimization algorithm, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Configuring-Iterations}{learn more about iterations in FlowSOM}}

\item{\code{max_relative_cluster_size}}{numeric representing the max relative cluster size (only applicable if \code{cluster_size_type} set to \code{"relative"} or \code{"both"}, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}}

\item{\code{normalize_scales}}{logical representing whether or not to normalize scales}

\item{\code{num_events_to_actually_sample}}{numeric representing the events actually sampled}

\item{\code{num_fcs_files}}{numeric representing the number of FCS files}

\item{\code{output_file_type}}{character representing the output file type\cr
\emph{- choose from the following : \code{("both", "pdf" [default], "png")}}}

\item{\code{population_id}}{integer representing a population \strong{gate set ID}}

\item{\code{random_seed}}{numeric representing the seed value \href{https://support.cytobank.org/hc/en-us/articles/360015918512#Setting-the-Seed}{learn more about setting the seed for FlowSOM}}

\item{\code{show_background_on_legend}}{logical representing whether or not to show background on legend, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}}

\item{\code{show_background_on_channel_colored_msts}}{logical representing whether or not to show background on channel colored MSTs, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}}

\item{\code{show_background_on_population_pies}}{logical representing whether or not to show background on population pies, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#cluster-sizing}{learn more about FlowSOM PDF output}}

\item{\code{som_creation_method}}{character representing the FlowSOM creation method, \href{https://support.cytobank.org/hc/en-us/articles/360015918512#SOM-Creation}{learn more about SOM creationg methods for FlowSOM}\cr
\emph{- choose from the following : \code{("create_new" [default], "import_existing")}}}

\item{\code{type}}{character}
}}

