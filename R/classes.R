#' @title Endpoint functions
#'
#' @rdname classes
#'
#' @description
#' The \code{classes} implements operations to access and manipulate LCCS-WS
#' Classes of Classification Systems endpoints (LCCS-WS-SPEC 0.6.0-0)
#'
#' @param q a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param class_id a \code{character} class id to be retrieved from a
#' Classification System
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
#' A \code{RLCCSQuery} object with the subclass \code{classes} for
#'  \code{/classification_system/system_id/classes/} endpoint, containing
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
#'   classification_systems(system_id = 2) %>%
#'   classes() %>%
#'   get_request()
#' }
#' @export
classes <- function(q, class_id = NULL, params_list = list()) {

  # check q parameter
  check_subclass(q, "classification_systems_id")

  params <- list()

  subclass <- "classes"
  if (!is.null(class_id)) {

    if (length(class_id) != 1)
      .error("Parameter `class_id` must be a single value.")

    params[["class_id"]] <- class_id
    subclass <- "classes_id"
  }
  params <- utils::modifyList(params, params_list)

  RLCCSQuery(base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass,
             token = q$token,
             query_type = "list")
}

#' @export
endpoint.classes <- function(q) {

  return(paste("/classification_systems", q$params[["system_id"]], "classes",
               sep = "/"))

}

#' @export
before_request.classes <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  q <- omit_query_params(q, names = "system_id")

  return(q)
}

#' @export
after_response.classes <- function(q, res) {
  content <- content_response(res, c("200", "201", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Classes")
}


#' @export
endpoint.classes_id <- function(q) {

  return(paste("/classification_systems", q$params[["system_id"]],
               "classes",  q$params[["class_id"]],
               sep = "/"))
}

#' @export
before_request.classes_id <- function(q) {
  check_query_verb(q, verbs = c("GET", "PUT", "DELETE"))

  q <- omit_query_params(q, names = c("system_id", "class_id"))

  return(q)
}

#' @export
after_response.classes_id <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Classes")
}

