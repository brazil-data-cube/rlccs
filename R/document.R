RLCCSDocument <- function(content, q, subclass) {

  structure(
    content,
    query = q,
    class = c(subclass, "RLCCSDocument", "list")
  )
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
