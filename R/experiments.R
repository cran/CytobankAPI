# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Experiment Endpoints
#'
#' Interact with experiment endpoints. An Experiment is a container for data and analyses in Cytobank. If data are on Cytobank, they must be within an Experiment. Configurations such as \link{gates}, \link{compensations}, \link{scales}, Sample Tags, and illustrations are also linked to an individual Experiment. Within the Cytobank interface, the \href{https://support.cytobank.org/hc/en-us/articles/206946617-The-Experiment-Summary-page}{Experiment Summary Page} is a useful integration point for information about an Experiment.
#' @name experiments
#' @param allow_full_access_pi boolean denoting to allow full access to PI option \strong{[optional]}
#' @param clone_annotations boolean denoting cloning annotations option \strong{[optional]}
#' @param clone_attachments boolean denoting cloning attachments option \strong{[optional]}
#' @param clone_compensations boolean denoting cloning compensations option \strong{[optional]}
#' @param clone_gates boolean denoting cloning gates option \strong{[optional]}
#' @param clone_illustrations boolean denoting cloning illustrations option \strong{[optional]}
#' @param clone_panels boolean denoting cloning panels option \strong{[optional]}
#' @param clone_project boolean denoting cloning project option \strong{[optional]}
#' @param clone_reagents boolean denoting cloning reagents option \strong{[optional]}
#' @param clone_user_access boolean denoting cloning user access option \strong{[optional]}
#' @param comments character representing an experiment comment \strong{[optional]}
#' @param experiment dataframe representing an experiment
#' @param experiment_id integer representing an experiment ID
#' @param experiment_name character representing an experiment name
#' @param fcs_files vector/list of integers representing a list of \link[=fcs_files]{FCS file} IDs \strong{[optional]}
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- experiments.clone_full, experiments.clone_selective, experiments.full_access_users_list, experiments.list, experiments.new, experiments.show, experiments.trash, experiments.update : \code{("default", "raw")}}
#' @param primary_researcher integer representing a primary researcher ID \strong{[optional]}
#' @param principal_investigator integer representing a principal investigator ID \strong{[optional]}
#' @param purpose character representing an experiment purpose
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param username character representing a username
#' @param UserSession Cytobank UserSession object
#' @param user_email character representing a user's email
#' @param user_id integer representing a user's ID
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("experiments.clone_full", function(UserSession, experiment_id, output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("experiments.clone_full")
})
#' @rdname experiments
#' @aliases experiments.clone_full
#'
#' @details \code{experiments.clone_full} Full clone an experiment. \href{https://support.cytobank.org/hc/en-us/articles/205337847-Clone-an-experiment-to-make-a-copy-for-your-own-use#full_clone}{Learn more about the full clone functionality}.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{experiments.clone_full(cyto_session, 22)
#' }
#' @export
setMethod("experiments.clone_full", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/clone", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.clone_selective", function(UserSession, experiment_id, experiment_name, fcs_files=c(-1),
                                                   primary_researcher=NA, principal_investigator=NA, clone_gates=FALSE,
                                                   clone_annotations=FALSE, clone_attachments=FALSE, clone_reagents=FALSE,
                                                   clone_compensations=FALSE, clone_panels=FALSE,
                                                   clone_illustrations=FALSE, clone_project=FALSE,
                                                   clone_user_access=FALSE, allow_full_access_pi=FALSE,
                                                   output="default", timeout=UserSession@long_timeout)
{
    standardGeneric("experiments.clone_selective")
})
#' @rdname experiments
#' @aliases experiments.clone_selective
#'
#' @details \code{experiments.clone_selective} Selectively clone an experiment. \href{https://support.cytobank.org/hc/en-us/articles/205337847-Clone-an-experiment-to-make-a-copy-for-your-own-use#selective_clone}{Learn more about the selective clone functionality}\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{experiments.clone_selective(cyto_session, 22,
#'   experiment_name="My New Experiment Name", fcs_files=c(12, 13, 14, 15, 16))
#' }
#' @export
setMethod("experiments.clone_selective", signature(UserSession="UserSession"), function(UserSession, experiment_id, experiment_name, fcs_files=c(-1),
                                                                                        primary_researcher=NA, principal_investigator=NA, clone_gates=FALSE,
                                                                                        clone_annotations=FALSE, clone_attachments=FALSE, clone_reagents=FALSE,
                                                                                        clone_compensations=FALSE, clone_panels=FALSE,
                                                                                        clone_illustrations=FALSE, clone_project=FALSE,
                                                                                        clone_user_access=FALSE, allow_full_access_pi=FALSE,
                                                                                        output="default", timeout=UserSession@long_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    # Clone gates + compensations
    if (clone_gates)
    {
        clone_compensations <- TRUE
    }

    if (clone_annotations)
    {
        clone_panels <- TRUE
    }



    body <- list(experiment=list(
        experimentName=experiment_name,
        fcsFileIds=as.list(fcs_files),
        cloneGates=clone_gates,
        cloneAnnotations=clone_annotations,
        cloneAttachments=clone_attachments,
        cloneReagents=clone_reagents,
        cloneCompensations=clone_compensations,
        clonePanels=clone_panels,
        cloneIllustrations=clone_illustrations,
        cloneProject=clone_project,
        cloneUserAccess=clone_user_access,
        allowFullAccessPi=allow_full_access_pi
    ))
    # Optional primary_researcher & principal_investigator
    if (!is.na(primary_researcher))
    {
        body[["experiment"]][["primaryResearcherId"]] <- primary_researcher
    }
    if (!is.na(principal_investigator))
    {
        body[["experiment"]][["principalInvestigatorId"]] <- principal_investigator
    }

    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/selective_clone", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=body,
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.delete", function(UserSession, experiment_id, timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.delete")
})
#' @rdname experiments
#' @aliases experiments.delete
#'
#' @details \code{experiments.delete} Permanently delete an experiment and all analyses (including SPADE, viSNE, etc.) permanently. This is not reversible.
#' @examples \dontrun{experiments.delete(cyto_session, 22)
#' }
#' @export
setMethod("experiments.delete", signature(UserSession="UserSession"), function(UserSession, experiment_id, timeout=UserSession@short_timeout)
{
    resp <- DELETE(paste(UserSession@site, "/experiments/", experiment_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (http_error(resp))
    {
        error_parse(resp, "experiments")
    }

    return(paste("Experiment (ID=", experiment_id, ") successfully deleted.", sep=""))
})


setGeneric("experiments.full_access_users_list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.full_access_users_list")
})
#' @rdname experiments
#' @aliases experiments.full_access_users_list
#'
#' @details \code{experiments.list} List all full access users from an experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all full access users
#' experiments.full_access_users_list(cyto_session, 22)
#'
#' # List of all full access users
#' experiments.full_access_users_list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("experiments.full_access_users_list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/full_access_users", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")[[1]][[1]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.full_access_users_add", function(UserSession, experiment_id, user_id=NA, user_email=NA, username=NA, timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.full_access_users_add")
})
#' @rdname experiments
#' @aliases experiments.full_access_users_add
#'
#' @details \code{experiments.list} Add a full access user to an experiment. A full access user can be added by a user ID, email, or username.
#' @examples \dontrun{# Add a user as a full access user by user's ID
#' experiments.full_access_users_add(cyto_session, 22, user_id=2)
#'
#' # Add a user as a full access user by user's email
#' experiments.full_access_users_add(cyto_session, 22, user_email="sammy_cytometry@cytobank.org")
#'
#' # Add a user as a full access user by user's username
#' experiments.full_access_users_add(cyto_session, 22, username="sammy_cytometry")
#' }
#' @export
setMethod("experiments.full_access_users_add", signature(UserSession="UserSession"), function(UserSession, experiment_id, user_id=NA, user_email=NA, username=NA, timeout=UserSession@short_timeout)
{
    if (!is.na(user_id))
    {
        body <- list(experiment=list(userId=user_id))
    }
    else if (!is.na(user_email))
    {
        body <- list(experiment=list(userEmail=user_email))
    }
    else # if default back to user_name
    {
        body <- list(experiment=list(userName=username))
    }

    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/add_full_access_user", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=body,
                encode="json",
                timeout(timeout)
    )

    if (resp$status_code == 204)
    {
        return ("You are the Primary Researcher. You cannot add yourself again.")
    }

    return(parse(resp, "experiments"))
})


setGeneric("experiments.full_access_users_remove", function(UserSession, experiment_id, user_id=NA, user_email=NA, username=NA, timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.full_access_users_remove")
})
#' @rdname experiments
#' @aliases experiments.full_access_users_remove
#'
#' @details \code{experiments.list} Remove a full access user from an experiment. A full access user can be removed by a user ID, email, or username.
#' @examples \dontrun{# Remove a user as a full access user by user's ID
#' experiments.full_access_users_remove(cyto_session, 22, user_id=2)
#'
#' # Remove a user as a full access user by user's email
#' experiments.full_access_users_remove(cyto_session, 22, user_email="sammy_cytometry@cytobank.org")
#'
#' # Remove a user as a full access user by user's username
#' experiments.full_access_users_remove(cyto_session, 22, username="sammy_cytometry")
#' }
#' @export
setMethod("experiments.full_access_users_remove", signature(UserSession="UserSession"), function(UserSession, experiment_id, user_id=NA, user_email=NA, username=NA, timeout=UserSession@short_timeout)
{
    if (!is.na(user_id))
    {
        body <- list(experiment=list(userId=user_id))
    }
    else if (!is.na(user_email))
    {
        body <- list(experiment=list(userEmail=user_email))
    }
    else # if default back to user_name
    {
        body <- list(experiment=list(userName=username))
    }

    resp <- POST(paste(UserSession@site, "/experiments/", experiment_id, "/remove_full_access_user", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=body,
                 encode="json",
                 timeout(timeout)
    )

    if (resp$status_code == 204)
    {
        # double check on wording
        return ("User cannot be removed because the requested user is not a full access user.")
    }

    return(parse(resp, "experiments"))
})


setGeneric("experiments.list", function(UserSession, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.list")
})
#' @rdname experiments
#' @aliases experiments.list
#'
#' @details \code{experiments.list} List all inbox experiments. Outputs a data frame [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all inbox experiments with all fields present
#' experiments.list(cyto_session)
#'
#' # Raw list of all inbox experiments with all fields present
#' experiments.list(cyto_session, output="raw")
#' }
#' @export
setMethod("experiments.list", signature(UserSession="UserSession"), function(UserSession, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")[[1]]))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.new", function(UserSession, experiment_name, purpose, comments=NA, primary_researcher=NA, principal_investigator=NA, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.new")
})
#' @rdname experiments
#' @aliases experiments.new
#'
#' @details \code{experiments.new} Create a new experiment.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{experiments.new(cyto_session, "My New Experiment Name", "My experiment purpose",
#'   "An optional comment")
#' }
#' @export
setMethod("experiments.new", signature(UserSession="UserSession"), function(UserSession, experiment_name, purpose, comments=NA, primary_researcher=NA, principal_investigator=NA, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    body <- list(experiment=list(
        experimentName=experiment_name,
        purpose=purpose
    ))

    # Optional comment, primary_researcher, & principal_investigator
    if (!is.na(comments))
    {
        body[["experiment"]][["comments"]] <- comments
    }
    if (!is.na(primary_researcher))
    {
        body[["experiment"]][["primaryResearcherId"]] <- primary_researcher
    }
    if (!is.na(principal_investigator))
    {
        body[["experiment"]][["principalInvestigatorId"]] <- principal_investigator
    }

    resp <- POST(paste(UserSession@site, "/experiments/", sep=""),
                 add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                 body=body,
                 encode="json",
                 timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.show", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.show")
})
#' @rdname experiments
#' @aliases experiments.show
#'
#' @details \code{experiments.show} Show experiment details.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{experiments.show(cyto_session, 22)
#' }
#' @export
setMethod("experiments.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.trash", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.trash")
})
#' @rdname experiments
#' @aliases experiments.trash
#'
#' @details \code{experiments.trash} Trash an experiment. This is reversible and not to be confused with permanent deletion.
#' @examples \dontrun{experiments.trash(cyto_session, 22)
#' }
#' @export
setMethod("experiments.trash", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))

    resp <- PUT(paste(UserSession@site, "/experiments/", experiment_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(experiment=list(deleted=TRUE)),
                encode="json",
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "experiments"))
    }
})


setGeneric("experiments.update", function(UserSession, experiment, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("experiments.update")
})
#' @rdname experiments
#' @aliases experiments.update
#'
#' @details \code{experiments.update} Update an experiment.
#' (all parameters are optional, except for experiment_id)\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{experiments.update(cyto_session, experiment=cyto_experiment)
#' }
#' @export
setMethod("experiments.update", signature(UserSession="UserSession"), function(UserSession, experiment, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "experiments", possible_outputs=c("raw"))
    update_check(experiment, "experiments")

    # All parameters are optional, except for experiment_id
    body <- list()
    if (!is.na(experiment$experimentName))
    {
        body <- c(body, experimentName=experiment$experimentName)
    }
    if (!is.na(experiment$primaryResearcherId))
    {
        body <- c(body, primaryResearcherId=experiment$primaryResearcherId)
    }
    if (!is.na(experiment$principalInvestigatorId))
    {
        body <- c(body, principalInvestigatorId=experiment$principalInvestigatorId)
    }
    if (!is.na(experiment$projectId))
    {
        body <- c(body, projectId=experiment$projectId)
    }
    if (!is.na(experiment$purpose))
    {
        body <- c(body, purpose=experiment$purpose)
    }
    if (!is.na(experiment$comments))
    {
        body <- c(body, comments=experiment$comments)
    }

    resp <- PUT(paste(UserSession@site, "/experiments/", experiment$id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                body=list(experiment=body),
                encode="json",
                timeout(timeout)
    )

    if (output == "default")
    {
        return(cyto_dataframe(parse(resp, "experiments")))
    }
    else # if (output == "list)
    {
        return(parse(resp, "experiments"))
    }
})


###############################
# EXPERIMENTS HELPER FUNCTIONS
###############################


##########
# PRIVATE
##########




