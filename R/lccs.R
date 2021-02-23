#' @title Endpoint functions
#' @description The \code{lccs} function implements \code{/} API
#' endpoint. It prepares search fields parameters to be provided to
#' a LCCS-WS. This endpoint should return a \code{RLCCSDocument} containing links
#' to LCCS-WS API root and Classification Systems published.
#'
#' @param url  a \code{character} informing the base url of a
#'  LCCS-WS (LCCS-WS-SPEC 0.6.0-0)
#' @param .token a \code{character} informing the authentication token in the
#' BDC-OAuth service. This token is used for the service's administrative
#' operations through the HTTP verbs POST, PUT, DELETE.
#'
#' @return
#' A \code{RLCCSQuery} object with the subclass \code{lccs} containing all
#' request parameters to be provided to API service.
#'
#' @examples
#' \donttest{
#' lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
#'   get_request()
#' }
#' @export
lccs <- function(url = NULL, .token = NULL) {

  # check provided URL
  if (is.null(url))
    .error("LCCS-WS URL Service is required!")

  RLCCSQuery(base_url = url,
             params = list(),
             token = .token,
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
