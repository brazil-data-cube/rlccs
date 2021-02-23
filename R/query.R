#' @title Query development functions
#'
#' @describeIn extensions
#' The \code{RLCCSQuery()} function is a constructor of \code{RLCCSQuery}
#' objects. Every extension must implement a subclass of \code{RLCCSQuery} to
#' represent its queries. This is done by informing to the \code{subclass}
#' parameter the extension's subclass name.
#'
#' The \code{params} parameter is a named \code{list} where user parameters
#' must be stored. It is important to know if previous query parameters needs
#' to be keeped in the new query. If so, it is recommended do use
#' \code{\link[utils]{modifyList}()} function to merge the old and new
#' query parameters.
#'
#' @param base_url   a \code{character} informing the base url of a
#'  LCCS-WS (LCCS-WS-SPEC 0.6.0-0)
#'
#' @param params     a named \code{list} with all URL query parameters to be
#' appended in the URL.
#'
#' @param token      a \code{character} informing the authentication token in
#' the BDC-OAuth service. This token is used for the service's administrative
#' operations through the HTTP verbs POST, PUT, DELETE.
#'
#' @param subclass   a \code{character} corresponding to the subclass of the
#' object to be created.
#'
#' @param query_type a \code{character} representing the type of document that
#' should be sent to the LCCS-WS. With this parameter, you can specify when
#' rlccs should send the document in the format of a list (query_type = list) or
#' a document with JSON keys (query_type = key). Examples of use are given below:
#'
#' To send {'key': 'value'} to the server, use the type \code{key}.
#' On the other hand, sending [{'key': 'value'}] must be done
#' with a type \code{list}
#'
#' @param encode    a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @return
#' The \code{RSTACQuery()} function returns a \code{STACQuery} object with
#' subclass defined by \code{subclass} parameter.
#'
#' @export
RLCCSQuery <- function(base_url,
                       params = list(),
                       token = NULL,
                       subclass,
                       query_type = "key",
                       encode = NULL) {

  structure(
    list(base_url = base_url,
         endpoint = NULL,
         params = params,
         token = token,
         query_type = query_type,
         verb = "GET",
         encode = encode),
    class = c(subclass, "RLCCSQuery"))
}

#' @export
subclass.RLCCSQuery <- function(x) {

  class(x)[[1]]
}

#' @export
check_subclass.RLCCSQuery <- function(x, subclasses) {

  if (!subclass(x) %in% subclasses)
    .error("Expecting %s query.",
           paste0("`", subclasses, "`", collapse = " or "))
}


#' @export
endpoint.RLCCSQuery <- function(q) {

  .error("No endpoint was defined for the extension `%s`.", subclass(q))
}

#' @export
before_request.RLCCSQuery <- function(q) {

  check_query_verb(q, "")
}

#' @export
after_response.RLCCSQuery <- function(q, res) {

  check_query_verb(q, "")
}
