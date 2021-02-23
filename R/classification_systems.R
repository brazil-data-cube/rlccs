#' @title Endpoint functions
#'
#' @rdname classification_system
#'
#' @description
#' The \code{classification_system} implements operations to access and
#' manipulate LCCS-WS Classification Systems endpoints (LCCS-WS-SPEC 0.6.0-0)
#'
#' @param q a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param system_id a optional \code{character} system_id used to get information
#' about a specific Classification System
#' @seealso
#' \code{\link{get_request}}, \code{\link{post_request}},
#'  \code{\link{put_request}}, \code{\link{delete_request}}
#'
#' @param params_list HTTP Body Parameter List. The elements entered
#'  in this list may vary depending on the method being used. See the
#'  specification for the usage details for each of the operations.
#'  (https://github.com/brazil-data-cube/lccs-ws-spec).
#'
#' @return
#' A \code{RLCCSQuery} object with the subclass \code{classification_system} for
#'  \code{/classification_system/} endpoint with information about registered
#'  Classification Systems or about a new Classification System inserted. The
#'  \code{RLCCSQuery} also can be represent as a subclass
#'  \code{classification_systems_id} for \code{/classification_system/system_id} for
#'  information or operations about a specific Classification System.
#'
#'  These operations' results are all represented in JSON
#'  format, with the content varying according to the HTTP method
#'  that was performed. For the retrieval, addition, or update of classes
#'  linked to a Classification System, the result summarizes what was
#'  retrieved/added. In the case of deletions, only the confirmation
#'  of the operation is presented.
#'
#' @examples
#' \donttest{
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   classification_systems() %>%
#'   get_request()
#'
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   classification_systems(system_id = 2) %>%
#'   get_request()
#' }
#' @export
classification_systems <- function(q, system_id = NULL, params_list = list()) {

  # check q parameter
  check_subclass(q, "lccs")

  params <- list()

  subclass <- "classification_systems"
  if (!is.null(system_id)) {

    if (length(system_id) != 1)
      .error("Parameter `system_id` must be a single value.")

    params[["system_id"]] <- system_id

    subclass <- "classification_systems_id"
  }

  params <- utils::modifyList(params, params_list)


  RLCCSQuery(base_url = q$base_url,
             token = q$token,
             params = utils::modifyList(q$params, params),
             subclass = subclass)
}

#' @export
endpoint.classification_systems <- function(q) {
  return("/classification_systems")
}

before_request.classification_systems <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  return(q)
}

after_response.classification_systems <- function(q, res) {
  content <- content_response(res, c("200", "201", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "ClassificationSystem")
}

#' @export
endpoint.classification_systems_id <- function(q) {
  return(paste("/classification_systems", q$params[["system_id"]], sep = "/"))
}

#' @export
before_request.classification_systems_id <- function(q) {

  check_query_verb(q, verbs = c("GET", "PUT", "DELETE"))

  q <- omit_query_params(q, names = "system_id")

  return(q)
}

#' @export
after_response.classification_systems_id <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "ClassificationSystem")
}
