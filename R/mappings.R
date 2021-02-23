#' @title Endpoint functions
#'
#' @rdname mappings
#'
#' @description
#' The \code{mappings} implements operations to access and manipulate LCCS-WS
#' mappings of Classification Systems endpoints (LCCS-WS-SPEC 0.6.0-0)
#'
#' @param q a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param system_id_source A \code{integer} containing the ID of the classification
#' system to which you want to check the associated mappings. This parameter is
#' mandatory, and its use allows retrieving only the mappings associated with a
#' classification system.
#'
#' @param system_id_target an \code{integer} representing the ID of the target
#' classification system that you want to check for mappings. This parameter is
#' optional and can be done when there is a need to check the mapping
#' relationship between two classification systems. This parameter enables the
#' use of all management routes (HTTP verbs POST, PUT, DELETE) of the mappings
#' between two rating systems.
#'
#' @seealso
#' \code{\link{get_request}}, \code{\link{post_request}},
#'  \code{\link{put_request}}, \code{\link{delete_request}}
#'
#' @param params_list HTTP Body Parameter List. The elements entered
#'  in this list may vary depending on the method being used. See the
#'  specification for the usage details for each of the operations.
#'  (https://github.com/brazil-data-cube/lccs-ws-spec)
#'
#' @return
#' A \code{RLCCSQuery} object with the subclass \code{mappings_id_source} for
#'  \code{/mappings/system_id_source/} endpoint, or
#'  a\code{mappings_id_source_and_target} subclass for
#'  \code{/mappings/system_id_source/system_id_target} endpoint containing
#'  operations results.
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
#'   mappings(system_id_source = 3) %>%
#'   get_request()
#'
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   mappings(system_id_source = 3, system_id_target = 5) %>%
#'   get_request()
#' }
#' @export
mappings <- function(q,
                     system_id_source,
                     system_id_target = NULL,
                     params_list = list()) {

  # check q parameter
  check_subclass(q, "lccs")

  params <- list("system_id_source" = system_id_source)

  subclass <- "mappings_id_source"
  if (!is.null(system_id_target)) {

    if (length(system_id_target) != 1)
      .error("Parameter `system_id_target` must be a single value.")

    params[["system_id_target"]] <- system_id_target
    subclass <- "mappings_id_source_and_target"
  }

  params <- utils::modifyList(params, params_list)
  RLCCSQuery(base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass,
             token = q$token,
             query_type = "list")
}

#' @export
endpoint.mappings_id_source <- function(q) {
  return(paste("/mappings", q$params[["system_id_source"]], sep = "/"))
}

#' @export
before_request.mappings_id_source <- function(q) {
  check_query_verb(q, verbs = c("GET"))

  q <- omit_query_params(q, names = "system_id_source")
  return(q)
}

#' @export
after_response.mappings_id_source <- function(q, res) {
  content <- content_response(res, c("200", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Mapping")
}

#' @export
endpoint.mappings_id_source_and_target <- function(q) {
  return(paste("/mappings",
               q$params[["system_id_source"]],
               q$params[["system_id_target"]],
               sep = "/"))
}

#' @export
before_request.mappings_id_source_and_target <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST", "PUT", "DELETE"))

  q <- omit_query_params(q, names = c("system_id_source", "system_id_target"))
  return(q)
}

#' @export
after_response.mappings_id_source_and_target <- function(q, res) {
  content <- content_response(res, c("200", "201", "204", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Mapping")
}


