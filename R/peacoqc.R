# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' PeacoQC Endpoints
#'
#' Interact with PeacoQC using these endpoints.
#' @name peacoqc
#' @param peacoqc Cytobank PeacoQC object
#' @param peaco_qc_id integer representing a PeacoQC ID
#' @param peaco_qc_name character representing a new PeacoQC name
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#'
#' @param output character representing the output format  \strong{[optional]}\cr
#' \emph{- peacoqc.list, peacoqc.run, peacoqc.status, peacoqc.copy_settings : \code{("default", "raw")}}
#'
#' @param timeout integer representing the request timeout time in seconds  \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#'
#' # cyto_peacoqc refers to a PeacoQC object that is created from PeacoQC endpoints
#' #   examples: peacoqc.new, peacoqc.show (see details section for more)
#' }
NULL


######################
# PeacoQC class methods
######################


setGeneric("peacoqc.copy_settings", function(UserSession, peacoqc, output="default", timeout=UserSession@short_timeout)
{
  output_check(output, "PeacoQC", possible_outputs= "raw")

  standardGeneric("peacoqc.copy_settings")
})
#' @rdname peacoqc
#' @aliases peacoqc.copy_settings
#'
#' @details \code{peacoqc.copy_settings} Copy PeacoQC settings from an experiment and returns a PeacoQC object.
#' @examples \dontrun{peacoqc.copy_settings(cyto_session, peacoqc=cyto_peacoqc)
#' }
#' @export
setMethod("peacoqc.copy_settings", signature(UserSession="UserSession", peacoqc="PeacoQC"), function(UserSession, peacoqc, output="default", timeout=UserSession@short_timeout)
{
  resp <- POST(paste0(UserSession@site, "/experiments/", peacoqc@source_experiment, "/advanced_analyses/peaco_qc/", peacoqc@peaco_qc_id, "/copy_settings"),
               add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
               timeout(timeout)
  )

  if (output == "default")
  {
    return(cyto_dataframe(parse(resp, "PeacoQC")))
  }
  else # if (output == "raw")
  {
    return(parse(resp, "PeacoQC"))
  }
})


setGeneric("peacoqc.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
  standardGeneric("peacoqc.list")
})
#' @rdname peacoqc
#' @aliases peacoqc.list
#'
#' @details \code{peacoqc.list} List all PeacoQC from an experiment. Outputs a dataframe [default] or list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all PeacoQCs with all fields present
#' peacoqc.list(cyto_session, 22)
#'
#' # Raw list of all PeacoQCs with all fields present
#' peacoqc.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("peacoqc.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
  output_check(output, "PeacoQC", possible_outputs= "raw")

  resp <- GET(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/peaco_qc"),
              add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
              timeout(timeout)
  )

  if (output == "default")
  {
    return(cyto_dataframe(parse(resp, "PeacoQC")$peacoqc))
  }
  else # if (output == "raw")
  {
    return(parse(resp, "PeacoQC"))
  }
})


setGeneric("peacoqc.new", function(UserSession, experiment_id, peaco_qc_name, timeout=UserSession@long_timeout)
{
  standardGeneric("peacoqc.new")
})
#' @rdname peacoqc
#' @aliases peacoqc.new
#'
#' @details \code{peacoqc.new} Create a new PeacoQC advanced analysis from an experiment and returns a PeacoQC object.
#' @examples \dontrun{peacoqc.new(cyto_session, 22, peaco_qc_name="My new PeacoQC")
#' }
#' @export
setMethod("peacoqc.new", signature(UserSession="UserSession"), function(UserSession, experiment_id, peaco_qc_name, timeout=UserSession@long_timeout)
{
  resp <- POST(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/peaco_qc/"),
               add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
               body=list(name=peaco_qc_name),
               encode="json",
               timeout(timeout)
  )

  return(create_peacoqc_object(parse(resp, "PeacoQC")))
})


setGeneric("peacoqc.rename", function(UserSession, peacoqc, peaco_qc_name, timeout=UserSession@short_timeout)
{
  standardGeneric("peacoqc.rename")
})
#' @rdname peacoqc
#' @aliases peacoqc.rename
#'
#' @details \code{peacoqc.rename} Rename a PeacoQC from an experiment and returns a PeacoQC object.
#' @examples \dontrun{peacoqc.rename(cyto_session, peacoqc=cyto_peacoqc,
#'     peaco_qc_name="My updated PeacoQC name")
#' }
#' @export
setMethod("peacoqc.rename", signature(UserSession="UserSession", peacoqc="PeacoQC"), function(UserSession, peacoqc, peaco_qc_name, timeout=UserSession@short_timeout)
{
  resp <- PUT(paste0(UserSession@site, "/experiments/", peacoqc@source_experiment, "/advanced_analyses/peaco_qc/", peacoqc@peaco_qc_id, "/rename"),
              add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
              body=list(name=peaco_qc_name),
              encode="json",
              timeout(timeout)
  )

  peacoqc@name <- parse(resp, "PeacoQC")$peaco_qc$name
  return(peacoqc)
})


setGeneric("peacoqc.run", function(UserSession, peacoqc, output="default", timeout=UserSession@long_timeout)
{
  standardGeneric("peacoqc.run")
})
#' @rdname peacoqc
#' @aliases peacoqc.run
#'
#' @details \code{peacoqc.run} Run a PeacoQC from an experiment.
#' @examples \dontrun{peacoqc.run(cyto_session, peacoqc=cyto_peacoqc)
#' }
#' @export
setMethod("peacoqc.run", signature(UserSession="UserSession", peacoqc="PeacoQC"), function(UserSession, peacoqc, output="default", timeout=UserSession@long_timeout)
{
  output_check(output, "PeacoQC", possible_outputs= "raw")

  resp <- POST(paste0(UserSession@site, "/experiments/", peacoqc@source_experiment, "/advanced_analyses/peaco_qc/", peacoqc@peaco_qc_id, "/run"),
               add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
               timeout(timeout)
  )

  if (output == "default")
  {
    return(cyto_dataframe(parse(resp, "PeacoQC")))
  }
  else # if (output == "raw")
  {
    return(parse(resp, "PeacoQC"))
  }
})


setGeneric("peacoqc.show", function(UserSession, experiment_id, peaco_qc_id, timeout=UserSession@short_timeout)
{
  standardGeneric("peacoqc.show")
})
#' @rdname peacoqc
#' @aliases peacoqc.show
#'
#' @details \code{peacoqc.show} Show PeacoQC details from an experiment and returns a PeacoQC object.
#' @examples \dontrun{peacoqc.show(cyto_session, experiment_id=22, peaco_qc_id=2)
#' }
#' @export
setMethod("peacoqc.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, peaco_qc_id, timeout=UserSession@short_timeout)
{
  resp <- GET(paste0(UserSession@site, "/experiments/", experiment_id, "/advanced_analyses/peaco_qc/", peaco_qc_id, "?include_settings=1"),
              add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
              timeout(timeout)
  )

  return(create_peacoqc_object(parse(resp, "PeacoQC")))
})


setGeneric("peacoqc.status", function(UserSession, peacoqc, output="default", timeout=UserSession@long_timeout)
{
  standardGeneric("peacoqc.status")
})
#' @rdname peacoqc
#' @aliases peacoqc.status
#'
#' @details \code{peacoqc.status} Show the status of a PeacoQC from an experiment.
#' @examples \dontrun{peacoqc.status(cyto_session, peacoqc=cyto_peacoqc)
#' }
#' @export
setMethod("peacoqc.status", signature(UserSession="UserSession", peacoqc="PeacoQC"), function(UserSession, peacoqc, output="default", timeout=UserSession@long_timeout)
{
  output_check(output, "PeacoQC", possible_outputs= "raw")

  resp <- GET(paste0(UserSession@site, "/experiments/", peacoqc@source_experiment, "/advanced_analyses/peaco_qc/", peacoqc@peaco_qc_id, "/status"),
              add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
              timeout(timeout)
  )

  if (output == "default")
  {
    return(cyto_dataframe(parse(resp, "PeacoQC")))
  }
  else # if (output == "raw")
  {
    return(parse(resp, "PeacoQC"))
  }
})


setGeneric("peacoqc.update", function(UserSession, peacoqc, timeout=UserSession@long_timeout)
{
  standardGeneric("peacoqc.update")
})
#' @rdname peacoqc
#' @aliases peacoqc.update
#'
#' @details \code{peacoqc.update} Update a PeacoQC from an experiment and returns the new PeacoQC object.
#' @examples \dontrun{peacoqc.update(cyto_session, peacoqc=cyto_peacoqc)
#' }
#' @export
setMethod("peacoqc.update", signature(UserSession="UserSession", peacoqc="PeacoQC"), function(UserSession, peacoqc, timeout=UserSession@long_timeout)
{
  # required settings for running PeacoQC
  if (!length(peacoqc@fcs_files))
  {
    stop(
      sprintf (
        "Cytobank API 'peacoqc.update' request failed [client]\n    Must specify FCS file IDs in order to update."
      ),
      call. = FALSE
    )
  }
  if (!length(peacoqc@channel_unique_identifiers))
  {
    stop(
      sprintf (
        "Cytobank API 'peacoqc.update' request failed [client]\n    Must specify unique channel identifiers in order to update."
      ),
      call. = FALSE
    )
  }

  resp <- PUT(paste0(UserSession@site, "/experiments/", peacoqc@source_experiment, "/advanced_analyses/peaco_qc/", peacoqc@peaco_qc_id),
              add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
              body=list(options=list(
                fcsFileIds=peacoqc@fcs_files,
                channelUniqueIdentifiers=peacoqc@channel_unique_identifiers,
                detectionMethod=peacoqc@detection_method,
                maxBins=peacoqc@max_bins,
                MAD=peacoqc@mad,
                ITLimit=peacoqc@it_limit,
                consecutiveBins=peacoqc@consecutive_bins,
                removeMargins=peacoqc@remove_margins,
                useInternalScalesForMargins=peacoqc@use_internal_scales_for_margins
              )),
              encode="json",
              timeout(timeout)
  )

  return(create_peacoqc_object(parse(resp, "PeacoQC")))
})


##########################
# PeacoQC HELPER FUNCTIONS
##########################


##########
# PRIVATE
##########


# Create PeacoQC object from PeacoQC json response
create_peacoqc_object <- function(peaco_qc_response)
{
  return(
    new("PeacoQC",
        attachment_id=if (!is.integer(peaco_qc_response$peaco_qc$settings$attachmentId)) NA_integer_ else peaco_qc_response$peaco_qc$settings$attachmentId,
        author=peaco_qc_response$peaco_qc$author,
        channel_unique_identifiers=if (!is.list(peaco_qc_response$peaco_qc$settings$channelUniqueIdentifiers)) list() else peaco_qc_response$peaco_qc$settings$channelUniqueIdentifiers,
        completed=(peaco_qc_response$peaco_qc$status == 'Completed'),
        compensation_id=if (!is.integer(peaco_qc_response$peaco_qc$settings$compensationId)) as.integer(-2) else peaco_qc_response$peaco_qc$settings$compensationId,
        consecutive_bins=peaco_qc_response$peaco_qc$settings$consecutiveBins,
        detection_method=peaco_qc_response$peaco_qc$settings$detectionMethod,
        errors=if (!is.list(peaco_qc_response$peaco_qc$settings$errors)) list() else peaco_qc_response$peaco_qc$settings$errors,
        failed=(peaco_qc_response$peaco_qc$status == 'Failed'),
        fcs_files=if (!is.list(peaco_qc_response$peaco_qc$settings$fcsFileIds)) list() else peaco_qc_response$peaco_qc$settings$fcsFileIds,
        final_result=if (!is.character(peaco_qc_response$peaco_qc$settings$finalResult)) "" else peaco_qc_response$peaco_qc$settings$finalResult,
        heatmap_attachment_id=if (!is.integer(peaco_qc_response$peaco_qc$settings$heatmapAttachmentId)) NA_integer_ else peaco_qc_response$peaco_qc$settings$heatmapAttachmentId,
        it_limit=peaco_qc_response$peaco_qc$settings$ITLimit,
        mad=peaco_qc_response$peaco_qc$settings$MAD,
        max_bins=peaco_qc_response$peaco_qc$settings$maxBins,
        name=peaco_qc_response$peaco_qc$name,
        peaco_qc_id=peaco_qc_response$peaco_qc$id,
        remove_margins=peaco_qc_response$peaco_qc$settings$removeMargins,
        source_experiment=peaco_qc_response$peaco_qc$sourceExperiment,
        status=peaco_qc_response$peaco_qc$status,
        type=peaco_qc_response$peaco_qc$type,
        use_internal_scales_for_margins=peaco_qc_response$peaco_qc$settings$useInternalScalesForMargins,
        validFcsFileIds=if (!is.list(peaco_qc_response$peaco_qc$settings$validFcsFileIds)) list() else peaco_qc_response$peaco_qc$settings$validFcsFileIds
    )
  )
}
