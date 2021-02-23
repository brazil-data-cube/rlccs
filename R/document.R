#' @title Document development functions
#'
#' @describeIn extensions
#' The \code{RLCCSDocument()} function is a constructor of
#' LCCS documents. Currently, this class is used to represent the return of all
#' LCCS-WS endpoints. The general use of this document is possible since the
#' service return follows the same hierarchical structure.
#'
#' @param content    a \code{list} data structure representing the JSON file
#' received in HTTP response (see \code{\link{content_response}()} function)
#'
#' @param q          a \code{RLCCSDocument} object expressing the LCCS-WS query used
#' to retrieve the document.
#'
#' @param subclass   a \code{character} corresponding to the subclass of the
#' document to be created.
#'
#' @return
#' The \code{RLCCSDocument()} function returns a \code{RLCCSDocument} object
#' with subclass defined by \code{subclass} parameter.
#'
#' @export
RLCCSDocument <- function(content, q, subclass) {

  # DELETE operations do not return contents in the LCCS-WS (0.6.0)
  if (!is.null(content))
    return(structure(
      content,
      query = q,
      class = c(subclass, "RLCCSDocument", "list")
    ))

  NULL
}

#' @export
subclass.RLCCSDocument <- function(x) {

  class(x)[[1]]
}

#' @export
check_subclass.RLCCSDocument <- function(x, subclasses) {

  if (!subclass(x) %in% subclasses)
    .error("Expecting %s document(s).",
           paste0("`", subclasses, "`", collapse = " or "))
}

#' @title Document utils functions
#'
#' @param d \code{RLCCSDocument} object
#'
#' @return a \code{RLCCSQuery} object with the predecessor subclass with the
#'  fields used in the request.
#'
#' @export
doc_query <- function(d) {

  .check_obj(d, "RLCCSDocument")

  attr(d, "query")
}
