#' @title LCCS API request functions
#'
#' @rdname request
#'
#' @description The \code{get_request} ...
#'
#' The \code{post_request} ...
#'
#' The \code{put_request} ...
#'
#' The \code{delete_request} ...
#'
#' @param q         a \code{RLCCSQuery} object expressing a LCCS query
#' criteria.
#'
#' @param ... ...
#'
#' @param encode    a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @return ...
#'
#' @export
get_request <- function(q, ...) {

  # check the object class
  .check_obj(q, "RLCCSQuery")

  # stamp verb
  q$verb <- "GET"
  q$encode <- NULL

  # set endpoint
  q$endpoint <- endpoint(q)

  # process LCCS object
  q <- before_request(q)

  # process omitted params
  q <- .do_omit_query_params(q)

  tryCatch({
    res <- httr::GET(url = .make_url(q$base_url,
                                     endpoint = q$endpoint,
                                     params = q$params), ...)
  },
  error = function(e) {

    .error("Request error. %s", e$message)
  })

  # restore omitted params
  q <- .undo_omit_query_params(q)

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}

#' @rdname request
#' @export
post_request <- function(q, ..., encode = c("json", "multipart", "form")) {

  # check the object class
  .check_obj(q, "RLCCSQuery")

  # check request settings
  httr_encode <- c("json", "multipart", "form")
  encode <- encode[[1]]
  if (!encode %in% httr_encode)
    .error("Invalid body `encode` '%s'. Allowed `econde` are %s.",
           encode, paste0("'", httr_encode, "'", collapse = ", "))

  # stamp verb
  q$verb <- "POST"

  if (is.null(q$encode))
    q$encode <- encode

  # set endpoint
  q$endpoint <- endpoint(q)

  # process LCCS object
  q <- before_request(q)

  # process omitted params
  q <- .do_omit_query_params(q)

  if (q$query_type == "list")
    q$params <- list(q$params)

  tryCatch({
    res <- httr::POST(url = .make_url(q$base_url, endpoint = q$endpoint), ...,
                      body = q$params, encode = q$encode)
  },
  error = function(e) {
    .error("Request error. %s", e$message)
  })

  # restore omitted params
  q <- .undo_omit_query_params(q)

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}

#' @rdname request
#' @export
put_request <- function(q, ..., encode = c("json", "multipart", "form")) {

  # check the object class
  .check_obj(q, "RLCCSQuery")

  # check request settings
  httr_encode <- c("json", "multipart", "form")
  encode <- encode[[1]]
  if (!encode %in% httr_encode)
    .error("Invalid body `encode` '%s'. Allowed `econde` are %s.",
           encode, paste0("'", httr_encode, "'", collapse = ", "))

  # stamp verb
  q$verb <- "PUT"

  if (is.null(q$encode))
    q$encode <- encode

  # set endpoint
  q$endpoint <- endpoint(q)

  # process LCCS object
  q <- before_request(q)

  # process omitted params
  q <- .do_omit_query_params(q)

  if (q$query_type == "list")
    q$params <- list(q$params)

  tryCatch({
    res <- httr::PUT(url = .make_url(q$base_url, endpoint = q$endpoint), ...,
                      body = q$params, encode = q$encode)
  },
  error = function(e) {
    .error("Request error. %s", e$message)
  })

  # restore omitted params
  q <- .undo_omit_query_params(q)

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}

#' @rdname request
#'@export
delete_request <- function(q, ...) {

  # check the object class
  .check_obj(q, "RLCCSQuery")

  # stamp verb
  q$verb <- "DELETE"
  q$encode <- NULL

  # set endpoint
  q$endpoint <- endpoint(q)

  # process LCCS object
  q <- before_request(q)

  # process omitted params
  q <- .do_omit_query_params(q)

  tryCatch({
    res <- httr::DELETE(url = .make_url(q$base_url,
                                     endpoint = q$endpoint,
                                     params = q$params), ...)
  },
  error = function(e) {

    .error("Request error. %s", e$message)
  })

  # restore omitted params
  q <- .undo_omit_query_params(q)

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}


#' @describeIn extensions
#' The \code{.do_omit_query_params()} Function to make the omission of the
#'  parameters that were omitted in function \code{omit_query_params()}.
#'
#' @param q       a \code{RLCCSQuery} object.
#'
#' @noRd
.do_omit_query_params <- function(q) {

  if (is.character(q$omitted)) {

    to_omit <- names(q$param) %in% q$omitted
    if (length(to_omit) > 0) {
      q$omitted <- q$params[to_omit]
      q$params[to_omit] <- NULL
    }
  }
  q
}

#' @describeIn extensions
#' The \code{.undo_omit_query_params()} function to undo the omission of
#'  parameters that were omitted in function \code{omit_query_params()}.
#'
#' @param q       a \code{RLCCSQuery} object.
#'
#' @noRd
.undo_omit_query_params <- function(q) {

  if (is.list(q$omitted))
    q$params <- utils::modifyList(q$params, q$omitted)
  q$omitted <- NULL
  q
}
