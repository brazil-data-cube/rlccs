#' @title Endpoint functions
#'
#' @rdname styles
#'
#' @description
#' The \code{styles} implements operations to access and manipulate LCCS-WS
#' Styles endpoints (LCCS-WS-SPEC 0.6)
#'
#' @param q a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param style_id A \code{integer} containing the ID of the Style.
#' This parameter is optional. When it is not set, the list of all registered
#' Styles is retrieved (the add operation must be done without this parameter).
#' On the other hand, its setting causes the retrieval and management operations
#' to be done for a particular Style.
#'
#' @param style_file A \code{character} containing the Style file that will be
#' registered and sent to the server in a registration or style update operation
#' of a rating system.
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
#' A \code{RLCCSQuery} object with the subclass \code{styles} for
#'  \code{/classifiaction_systems/system_id/styles} endpoint, or
#'  a \code{styles_id} subclass for
#'  \code{/classifiaction_systems/system_id/styles/style_format_id} endpoint.
#'  When the style ID is passed next to a file, the operation to be performed is
#'  an update or add. In this case, the return will be the \code{styles_id_file}
#'  subclass.
#'
#' @examples
#' \donttest{
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   classification_systems(system_id = 1) %>%
#'   styles() %>%
#'   get_request()
#'
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   classification_systems(system_id = 1) %>%
#'   styles(style_id = 3) %>%
#'   get_request()
#' }
#' @export
styles <- function(q,
                   style_id = NULL,
                   style_file = NULL,
                   params_list = list()) {

  # check q parameter
  check_subclass(q, "classification_systems_id")

  encode <- NULL
  params <- list()

  subclass <- "styles"
  if (!is.null(style_id)) {

    if (length(style_id) != 1)
      .error("Parameter `style_id` must be a single value.")

    params[["style_id"]] <- style_id
    subclass <- "styles_id"
  }

  if (!is.null(style_file)) {
    if (length(style_file) != 1)
      .error("Parameter `style_file` must be a single value.")

    encode <- "multipart"
    style_file <- .parse_style_file(style_file)

    params[["style"]] <- httr::upload_file(style_file)
    subclass <- "styles_file"
  }

  if (!is.null(style_id) && !is.null(style_file))
    subclass <- "styles_id_file"

  params <- utils::modifyList(params, params_list)

  RLCCSQuery(base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass,
             token = q$token,
             encode = encode)
}

#' @export
endpoint.styles <- function(q) {
  return(paste("/classification_systems", q$params[["system_id"]], "style_formats",
               sep = "/"))
}

#' @export
before_request.styles <- function(q) {
  check_query_verb(q, verbs = c("GET"))
  q <- omit_query_params(q, names = "system_id")

  return(q)
}

#' @export
after_response.styles <- function(q, res) {
  content <- content_response(res, c("200", "201", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Styles")
}

#' @export
endpoint.styles_id <- function(q) {
  return(paste("/classification_systems",
               q$params[["system_id"]],
               "styles",
               q$params[["style_id"]],
               sep = "/"))
}

#' @export
before_request.styles_id <- function(q) {
  check_query_verb(q, verbs = c("GET", "DELETE"))
  q <- omit_query_params(q, names = c("system_id", "style_id"))

  return(q)
}

#' @export
after_response.styles_id <- function(q, res) {
  content <- content_response(res, c("200", "204", "400", "404", "500"),
                              c("application/json", "application/octet-stream"))

  RLCCSDocument(content = content, q = q, subclass = "Styles")
}

#' @export
endpoint.styles_file <- function(q) {
  return(paste("/classification_systems", q$params[["system_id"]],
               "styles", sep = "/"))
}

#' @export
before_request.styles_file <- function(q) {
  check_query_verb(q, verbs = c("POST"))

  q <- omit_query_params(q, names = c("system_id"))

  return(q)
}

#' @export
after_response.styles_file <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Styles")
}



#' @export
endpoint.styles_id_file <- function(q) {
  return(paste("/classification_systems", q$params[["system_id"]],
               "styles",  q$params[["style_id"]], sep = "/"))
}

#' @export
before_request.styles_id_file <- function(q) {
  check_query_verb(q, verbs = c("GET", "PUT", "DELETE"))

  q <- omit_query_params(q, names = c("system_id", "style_id"))

  return(q)
}

#' @export
after_response.styles_id_file <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Styles")
}
