#' @title ...
#' @description ...
#'
#' @param q ...
#' @param system_id ...
#' @param params_list ...
#'
#' @return ...
#'
#' @export
classification_systems <- function(q, system_id = NULL, params_list = list()) {

  # check q parameter
  check_subclass(q, "lccs")

  params <- list()

  subclass <- "classification_systems"
  if (!is.null(system_id)) {

    if (length(system_id) != 1)
      .error("Parameter `system_id` must be a single value.")

    params[["system_id"]] <- system_id

    subclass <- "classification_systems_id"
  }

  params <- utils::modifyList(params, params_list)


  RLCCSQuery(base_url = q$base_url,
             token = q$token,
             params = utils::modifyList(q$params, params),
             subclass = subclass)
}

#' @export
endpoint.classification_systems <- function(q) {
  return("/classification_systems")
}

before_request.classification_systems <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  return(q)
}

after_response.classification_systems <- function(q, res) {
  content <- content_response(res, c("200", "201", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "ClassificationSystem")
}

#' @export
endpoint.classification_systems_id <- function(q) {
  return(paste("/classification_systems", q$params[["system_id"]], sep = "/"))
}

#' @export
before_request.classification_systems_id <- function(q) {

  check_query_verb(q, verbs = c("GET", "PUT", "DELETE"))

  q <- omit_query_params(q, names = "system_id")

  return(q)
}

#' @export
after_response.classification_systems_id <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "ClassificationSystem")
}
