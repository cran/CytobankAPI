# Copyright 2020 Beckman Coulter, Inc.
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Population Endpoints
#'
#' Interact with population (aka gate sets) endpoints. A population is a set of \link[=gates]{gates} and can have parents and children. \href{https://support.cytobank.org/hc/en-us/articles/204765578-The-Difference-Between-a-Gate-and-a-Population-Using-the-Population-Manager-and-considerations-for-deleting-and-renaming-gates}{Learn more about gates and populations}.
#' @name populations
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param output character representing the output format \strong{[optional]}\cr
#' \emph{- populations.list, populations.show : \code{("default", "raw")}}
#' @param population_id integer representing a population ID
#' @param timeout integer representing the request timeout time in seconds
#' @param UserSession Cytobank UserSession object
#' @examples \dontrun{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


setGeneric("populations.list", function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("populations.list")
})
#' @rdname populations
#' @aliases populations.list
#'
#' @details \code{populations.list} List all populations from an experiment. Outputs a dataframe [default] or raw list with all fields present.\cr
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{# Dataframe of all populations with all fields present
#' populations.list(cyto_session, 22)
#'
#' # Raw list of all populations with all fields present
#' populations.list(cyto_session, 22, output="raw")
#' }
#' @export
setMethod("populations.list", signature(UserSession="UserSession"), function(UserSession, experiment_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "populations", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/populations", sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_population_list(parse(resp, "populations")$populations))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "populations"))
    }
})


setGeneric("populations.show", function(UserSession, experiment_id, population_id, output="default", timeout=UserSession@short_timeout)
{
    standardGeneric("populations.show")
})
#' @rdname populations
#' @aliases populations.show
#'
#' @details \code{populations.show} Show population details from an experiment.
#' \emph{- Optional output parameter, specify one of the following: \code{("default", "raw")}}
#' @examples \dontrun{populations.show(cyto_session, 22, population_id=2)
#' }
#' @export
setMethod("populations.show", signature(UserSession="UserSession"), function(UserSession, experiment_id, population_id, output="default", timeout=UserSession@short_timeout)
{
    output_check(output, "populations", possible_outputs=c("raw"))

    resp <- GET(paste(UserSession@site, "/experiments/", experiment_id, "/populations/", population_id, sep=""),
                add_headers(Authorization=paste("Bearer", UserSession@auth_token)),
                timeout(timeout)
    )

    if (output == "default")
    {
        return(build_population(parse(resp, "populations")$population))
    }
    else # if (output == "raw")
    {
        return(parse(resp, "populations"))
    }
})


###############################
# POPULATIONS HELPER FUNCTIONS
###############################


##########
# PRIVATE
##########


# Build population list in R
build_population_list <- function(population_list_info)
{
    population_list <- list()

    # **Requires the Ungated population -- WEIRD HACK REQUIRED, SHOULD BE RETURNED FORMALLY VIA API INSTEAD**
    # Population is gateSetDefinition in cytobank app.
    # AutoCompensation project add a new colmun "compensationId", there are two cases:
    #   - When the gateSet is a cleanup gateSet:        compensationId > 0
    #   - When the gateSet is a none cleanup gateset:   compensationId = 0
    ungated <- cyto_dataframe(list(list(id=NA_character_, name="Ungated", version=as.integer(-1), experimentId=NA_character_, compensationId=0, gateSetId=as.integer(0), createdAt=NA_character_, updatedAt=NA_character_)))
    ungated$definition <- apply(ungated, 1, function(row) list(gates=list(), negGates=list(), tailoredPerPopulation=list()))
    population_list <- c(population_list, list(ungated))

    for (population in population_list_info)
    {
        # Need to encapsulate built population within list in order to do.call the rbind
        population_list <- c(population_list, list(build_population(population)))
    }

    return(do.call(rbind, population_list))
}


# Build population info in R
build_population <- function(population_info)
{
    # Check if population_info havs compensationId, give a default value 0 when compensationId not in population_info
    # Make the populations before cytobank 9.4 have a dummy compensationId, to make all population has some columns
    if (!("compensationId" %in% names(population_info))) {
        population_info$compensationId <- 0
    }

    # Create dataframe without definition
    population <- cyto_dataframe(list(population_info[-which(names(population_info) == "definition")]))
    # Add definition as a list
    population$definition <- apply(population, 1, function(row) population_info$definition)

    return(population)
}

