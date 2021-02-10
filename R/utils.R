#' @title Utility functions
#'
#' @param feature_id  a \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{featureId\}}.
#'
#' @return A \code{character} with the parameter provided,or an error if the
#'  supplied \code{feature_id} has a length different from 1.
#'
#' @noRd
.parse_feature_id <- function(feature_id) {

  if (length(feature_id) != 1)
    .error("Parameter `feature_id` must be a single value.")

  return(feature_id)
}

#' @title Utility functions
#'
#' @param collections a \code{character} vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @return A \code{list} of collections.
#'
#' @noRd
.parse_collections <- function(collections) {

  if (length(collections) == 1 && !is.list(collections))
    collections <- list(collections)

  return(collections)
}

#' @title Utility functions
#'
#' @param msg   a \code{character} string with format error message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.error <- function(msg, ...) {

  stop(sprintf(msg, ...), call. = FALSE)
}

#' @title Utility functions
#'
#' @param msg   a \code{character} string with format text message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.message <- function(msg, ...) {

  message(sprintf(msg, ...))
}

#' @title Utility functions
#'
#' @param msg   a \code{character} string with format warning message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.warning <- function(msg, ...) {

  warning(sprintf(msg, ...), call. = FALSE)
}

#' @title Utility functions
#'
#' @param obj       an \code{object} to compare.
#'
#' @param expected  a \code{character} with the expected classes.
#'
#' @noRd
.check_obj <- function(obj, expected) {

  obj_name <- as.character(substitute(obj))

  if (missing(obj))
    .error("Param `%s` is missing.", obj_name)

  if (!inherits(obj, expected))
    .error("Invalid %s value in `%s` param.",
           paste0("`", expected, "`", collapse = " or "), obj_name)
}


#' @title uUtility functions
#'
#' @rdname http_request
#'
#' @description
#' \code{.make_url} is a helper function to generate url. The returned
#' url is formed by appending \code{endpoint} at the end of base url
#' informed by \code{url} parameter. If \code{endpoint} has multiple elements
#' it will be collapsed using \code{'/'} character.
#'
#' Note that \code{.make_url} function differs from standards of relative URI
#' path resolution (RFC 3986). Any existing path in base url
#' is maintained in the final url, and a simple string contatenation is made
#' whithout including any character separator. For this reason, this function
#' does not support the query and fragment URI components in the base url.
#'
#' @param url         a \code{character} informing the base url of a
#' LCCS web service.
#'
#' @param endpoint    a \code{character} a path to be appended in the final
#' url.
#'
#' @param params      a named \code{list} with all url query parameters to be
#' appended in the url.
#'
#' @return
#' \code{.make_url} returns an url to access LCCS endpoints.
#'
#' @noRd
.make_url <- function(url, endpoint = "", params = list()) {

  # remove trailing '/' char
  if (substring(url, nchar(url)) == "/")
    url <- substring(url, 1, nchar(url) - 1)

  endpoint <- paste0(endpoint, collapse = "/")

  # TODO: URI resolution for previous existing query and fragment URI components
  # in informed url.
  res <- paste0(url, endpoint)

  if (length(params) > 0) {

    if (is.null(names(params)))
      stop("URL query values must be named.", call. = FALSE)
    params <- .querystring_encode(params)
    res <- paste(res, params, sep = "?")
  }

  return(res)
}

#' @title Utility functions
#'
#' @param params a \code{list} of parameters received from LCCS objects.
#'
#' @return a \code{character} representing the encode parameters of the query.
#'
#' @noRd
.querystring_encode <- function(params) {

  if (!is.null(names(params)))
    return(paste(names(params),
                 vapply(unname(params), paste0, collapse = ",", character(1)),
                 sep = "=", collapse = "&"))
  return(paste0(params, collapse = ","))
}

#' @title Utility functions
#'
#' @param querystring a \code{character} with the query to be decoded.
#'
#' @return a \code{list} with the query params.
#'
#' @noRd
.querystring_decode <- function(querystring) {

  values <- lapply(strsplit(querystring, split = "&")[[1]],
                   function(x) strsplit(x, split = "=")[[1]])

  params <- lapply(values, `[[`, 2)
  names(params) <- vapply(values, `[[`, 1, FUN.VALUE = character(1))

  return(params)
}
