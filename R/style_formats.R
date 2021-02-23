#' @title Endpoint functions
#'
#' @rdname style_formats
#'
#' @description
#' The \code{style_formats} implements operations to access and manipulate LCCS-WS
#' Style Formats endpoints (LCCS-WS-SPEC 0.6)
#'
#' @param q a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param style_format_id A \code{integer} containing the ID of the Style Format.
#' This parameter is optional. When it is not set, the list of all registered
#' Style Formats is retrieved (the add operation must be done without this parameter).
#' On the other hand, its setting causes the retrieval and management operations
#' to be done for a particular Style Format.
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
#' A \code{RLCCSQuery} object with the subclass \code{style_formats} for
#'  \code{/style_formats/} endpoint, or
#'  a \code{style_formats_id} subclass for
#'  \code{/style_formats/style_format_id/} endpoint containing
#'  operations results.
#'
#'  These operations' results are all represented in JSON
#'  format, with the content varying according to the HTTP method
#'  that was performed. For the retrieval, addition, or update of classes
#'  linked to a Classification System, the result summarizes what was
#'  retrieved/added. In the case of deletions, only the confirmation
#'  of the operation is presented.
#'
#'  All classification systems in LCCS are linked to a Style Format. This allow
#'  the use of \code{classification_system_id class} subclass object to this
#'  endpoint and retrieve the Style Format related information for a specific
#'  Classification System. This use returns subclass
#'  \code{style_formats_classification} to
#'  \code{/classification_systems/system_id/style_formats/} endpoint.
#'
#' @examples
#' \donttest{
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   style_formats() %>%
#'   get_request()
#'
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   style_formats(style_format_id = 3) %>%
#'   get_request()
#'
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   classification_systems(system_id = 1) %>%
#'   style_formats() %>%
#'   get_request()
#' }
#' @export
style_formats <- function(q, style_format_id = NULL, params_list = list()) {

  # check q parameter
  check_subclass(q, c("lccs", "classification_systems_id"))

  subclass <- "style_formats"
  if (subclass(q) == "classification_systems_id")
    subclass <- "style_formats_classification"

  params <- list()

  if (!is.null(style_format_id)) {

    if (length(style_format_id) != 1)
      .error("Parameter `style_format_id` must be a single value.")

    params[["style_format_id"]] <- style_format_id
    subclass <- "style_formats_id"
  }

  params <- utils::modifyList(params, params_list)
  RLCCSQuery(base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             token = q$token,
             subclass = subclass)
}

#' @export
endpoint.style_formats_classification <- function(q) {
  return(paste("/classification_systems",
               q$params[["system_id"]],
               "style_formats",
               sep = "/"))
}


#' @export
before_request.style_formats_classification <- function(q) {
  check_query_verb(q, verbs = c("GET"))

  q <- omit_query_params(q, names = c("system_id"))

  return(q)
}

#' @export
after_response.style_formats_classification <- function(q, res) {
  content <- content_response(res, c("200", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "StyleFormats")
}


#' @export
endpoint.style_formats <- function(q) {
  return("/style_formats")
}


#' @export
before_request.style_formats <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))

  return(q)
}

#' @export
after_response.style_formats <- function(q, res) {
  content <- content_response(res, c("200", "201", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "StyleFormats")
}


#' @export
endpoint.style_formats_id <- function(q) {
  return(paste("/style_formats", q$params[["style_format_id"]], sep = "/"))
}

#' @export
before_request.style_formats_id <- function(q) {
  check_query_verb(q, verbs = c("GET", "PUT", "DELETE"))

  q <- omit_query_params(q, names = c("style_format_id"))

  return(q)
}

#' @export
after_response.style_formats_id <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "StyleFormats")
}
