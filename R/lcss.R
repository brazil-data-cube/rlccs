#' @title ...
#' @description ...
#'
#' @param url ...
#' @param token ...
#'
#' @return a class ...
#'
#' @export
lccs <- function(url = NULL, token = NULL) {

  # ...
  if (is.null(url))
    .error("...")

  RLCCSQuery(base_url = url,
             params = list(),
             subclass = "lccs")

}

#' @export
endpoint.lcss <- function(q) {

  return("/")
}

#' @export
before_request.lcss <- function(q) {

  check_query_verb(q, verbs = c("GET"))

  return(q)
}

#' @export
after_response.lcss <- function(q, res) {

  content <- content_response(res, "200", "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Root")
}
