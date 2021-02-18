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
classes <- function(q, class_id = NULL, params_list = list()) {

  # check q parameter
  check_subclass(q, "classification_systems_id")

  params <- list()

  subclass <- "classes"
  if (!is.null(class_id)) {

    if (length(class_id) != 1)
      .error("Parameter `class_id` must be a single value.")

    params[["class_id"]] <- class_id
    subclass <- "classes_id"
  }
  params <- utils::modifyList(params, params_list)

  RLCCSQuery(base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass,
             token = q$token,
             query_type = "list")
}

#' @export
endpoint.classes <- function(q) {

  return(paste("/classification_systems", q$params[["system_id"]], "classes",
               sep = "/"))

}

#' @export
before_request.classes <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  q <- omit_query_params(q, names = "system_id")

  return(q)
}

#' @export
after_response.classes <- function(q, res) {
  content <- content_response(res, c("200", "201", "400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Classes")
}


#' @export
endpoint.classes_id <- function(q) {

  return(paste("/classification_systems", q$params[["system_id"]],
               "classes",  q$params[["class_id"]],
               sep = "/"))
}

#' @export
before_request.classes_id <- function(q) {
  check_query_verb(q, verbs = c("GET", "PUT", "DELETE"))

  q <- omit_query_params(q, names = c("system_id", "class_id"))

  return(q)
}

#' @export
after_response.classes_id <- function(q, res) {
  content <- content_response(res, c("200", "204" ,"400", "404", "500"),
                              "application/json")

  RLCCSDocument(content = content, q = q, subclass = "Classes")
}

