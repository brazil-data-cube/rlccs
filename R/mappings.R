#' @title ...
#' @description ...
#'
#' @param q ...
#' @param system_id_source ...
#' @param system_id_target ...
#' @param params_list ...
#'
#' @return ...
#'
#' @export
mappings <- function(q,
                     system_id_source,
                     system_id_target = NULL,
                     params_list = list()) {

  # check q parameter
  check_subclass(q, "lccs")

  params <- list("system_id_source" = system_id_source)

  subclass <- "mappings_id_source"
  if (!is.null(system_id_target)) {

    if (length(system_id_target) != 1)
      .error("Parameter `system_id_target` must be a single value.")

    params[["system_id_target"]] <- system_id_target
    subclass <- "mappings_id_source_and_target"
  }

  params <- utils::modifyList(params, params_list)
  RLCCSQuery(base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass,
             token = q$token,
             query_type = "list")
}

#' @export
endpoint.mappings_id_source <- function(q) {
  return(paste("/mappings", q$params[["system_id_source"]], sep = "/"))
}

#' @export
before_request.mappings_id_source <- function(q) {
  check_query_verb(q, verbs = c("GET"))

  q <- omit_query_params(q, names = "system_id_source")
  return(q)
}

#' @export
after_response.mappings_id_source <- function(q, res) {
  content <- content_response(res, c("200", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Mapping")
}

#' @export
endpoint.mappings_id_source_and_target <- function(q) {
  return(paste("/mappings",
               q$params[["system_id_source"]],
               q$params[["system_id_target"]],
               sep = "/"))
}

#' @export
before_request.mappings_id_source_and_target <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST", "PUT", "DELETE"))

  q <- omit_query_params(q, names = c("system_id_source", "system_id_target"))
  return(q)
}

#' @export
after_response.mappings_id_source_and_target <- function(q, res) {
  content <- content_response(res, c("200", "201", "204", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Mapping")
}


