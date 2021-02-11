#' @title ...
#' @description ...
#'
#' @param q ...
#' @param style_format_id ...
#' @param params_list ...
#'
#' @return ...
#'
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

