#' @title Extension development functions
#'
#' @description ...
#'
#' @param q       a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param res     a \code{httr} \code{response} object.
#'
#' @return
#' A \code{character} endpoint value for \code{endpoint()} function.
#' A \code{RLCCSQuery} object for \code{before_request()} and
#' \code{after_response()} functions.
#'
#'
#' @name extensions
NULL

#' @title Extension development functions
#' @rdname extensions
#' @export
endpoint <- function(q) {

  UseMethod("endpoint")
}

#' @title Extension development functions
#' @rdname extensions
#' @export
before_request <- function(q) {

  UseMethod("before_request")
}

#' @title Extension development functions
#' @rdname extensions
#' @export
after_response <- function(q, res) {

  UseMethod("after_response")
}

#' @describeIn extensions
#' The \code{content_response} function checks if the request's
#' response is in accordance with the allowed status codes and content-types.
#' It returns the parsed content response.
#'
#' @param res     a \code{httr} \code{response} object.
#'
#' @param status_codes  a \code{character} vector with successful
#' status codes.
#'
#' @param content_types a \code{character} vector with all acceptable
#' responses' content type.
#'
#' @return
#' The \code{content_response()} function returns a \code{list} data structure
#' representing the JSON file received in HTTP response
#'
#' @export
content_response <- function(res, status_codes, content_types) {

  # convert any json extension
  content_type <- httr::http_type(res)
  if (grepl("application/.*json", content_type))
    content_type <- "application/json"

  # parse content
  content <- httr::content(res,
                           type = content_type,
                           encoding = "UTF-8",
                           simplifyVector = TRUE,
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE)

  # test for allowed status codes
  status_code <- as.character(httr::status_code(res))
  if (!status_code %in% status_codes) {
    message <- ""
    if (is.atomic(content))
      message <- content
    else if (!is.null(content[["description"]]))
      message <- content[["description"]]

    .error("HTTP status '%s'. %s", status_code, message)
  }

  # test for allowed content types
  if (!httr::http_type(res) %in% content_types)
    .error("HTTP content type response '%s' not defined for this operation.",
           httr::http_type(res))

  return(content)
}

#' @describeIn extensions
#' The \code{check_query_verb()} function allows you to define which HTTP
#' verbs are allowed. It is useful for establishing which verbs will be
#' supported by an extension.
#'
#' @param q       a \code{RLCCSQuery} object.
#'
#' @param verbs   a \code{character} vector with allowed HTTP request methods
#'
#' @param msg     a \code{character} with a personalized error message
#'
#' @export
check_query_verb <- function(q, verbs, msg = NULL) {

  if (!q$verb %in% verbs) {
    if (is.null(msg))
      msg <- sprintf("HTTP verb '%s' is not defined for the query '%s'.",
                     q$verb, subclass(q))
    .error(msg)
  }
}

#' @describeIn extensions
#' The \code{check_subclass()} function specifies which type of query
#' objects (\code{RLCCSQuery}) or document objects (\code{RLCCSDocument})
#' are expected in the function extension.
#'
#' @param x            either a \code{RLCCSQuery} object expressing a LCCS query
#' criteria or any \code{RLCCSDocument}.
#'
#' @param subclasses   a \code{character} vector with all allowed S3 subclasses
#'
#' @export
check_subclass <- function(x, subclasses) {

  UseMethod("check_subclass")
}

#' @describeIn extensions
#' The \code{subclass()} function returns a \code{character} representing the
#' subclass name of either \code{RLCCSQuery} or \code{RSTACDocument} S3 classes.
#'
#' @export
subclass <- function(x) {

  UseMethod("subclass")
}

#' @describeIn extensions
#' The \code{omit_query_params()} function was created to omit the paths that
#'  are defined as query parameters to simplify the creation of a query.
#'  Therefore, use this method only in endpoints that specify a parameter in
#'  their paths.
#'
#' @param q       a \code{RLCCSQuery} object.
#'
#' @param names   a \code{character} vector with the names do omit.
#'
#' @export
omit_query_params <- function(q, names) {

  .check_obj(names, "character")
  q$omitted <- unname(names)
  q
}
