#' Update record
#'
#' @param app the app name
#' @param api the api
#' @param collection the collection name or id
#' @param id the record id
#' @param body the body
#' @param token the authentication token, depending on collection's `updateRule`
#' @param ... the query params as supported by
#'            [PocketBase View API](https://pocketbase.io/docs/api-records/#update-record)
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' adm <- ph_authenticate_admin_username_password(
#'   "eurocontrol-data-test",
#'   "/api/admins/auth-with-password",
#'   username,
#'   password)
#'   bd <- list(
#'     DAY_TFC= 121,
#'     FLIGHT_DATE= "2024-05-01 00:00:00.000Z"
#'     )
#'  id <- "lufkj3346ygx9a2"
#'  ph_update_record(
#'   app = "eurocontrol-data-test",
#'   api = "/api/collections",
#'   collection = "nw_traffic",
#'   token = adm$token,
#'   body = bd)
#' }
ph_update_record <- function(app, api, collection, id, body, token = NULL, ...) {
  params <- rlang::list2(...)
  check_update_params(params)

  req <- base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_url_path_append(collection) |>
    httr2::req_url_path_append("records") |>
    httr2::req_url_path_append(id) |>
    httr2::req_method("PATCH") |>
    httr2::req_user_agent(.POCKETHOSTR_UA) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_body_json(body)

  # add token if present. this depends on the collection's updateRule
  if (!is.null(token)) {
    req <- req |>
      httr2::req_headers(Authorization = token)
  }

  req |>
    httr2::req_perform()
}


# check that the params are those supported by PocketBase
check_update_params <- function(params, call = rlang::caller_env()) {
  param_names <- names(params)
  possible_param_names <- c("expand", "fields")
  non_supported <- setdiff(param_names, possible_param_names)
  if (length(non_supported) > 0) {
    cli::cli_abort("{.params {non_supported}}: Not supported.", call = call)
  }
}

