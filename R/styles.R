#' @title ...
#' @description ...
#'
#' @param q ...
#' @param system_id ...
#' @param params_list ...
#'
#' @return ...
#'
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
