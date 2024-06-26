#' View record
#'
#' @param app the app name
#' @param api the api
#' @param collection the collection name or id
#' @param id the record id
#' @param ... the query params as supported by
#'            [PocketBase View API](https://pocketbase.io/docs/api-records/#view-record)
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' app <- "eurocontrol-data-test"
#' collection <- "nw_traffic"
#' id <- "lufkj3346ygx9a2"
#'
#' ph_view_record(app = app,
#'                api = "/api/collections",
#'                id = id,
#'                collection = collection,
#'                fields = "FLIGHT_DATE,DAY_TFC")
#' }
ph_view_record <- function(app, api, collection, id, ...) {
  params <- rlang::list2(...)
  check_view_params(params)

  base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_url_path_append(collection) |>
    httr2::req_url_path_append("records") |>
    httr2::req_url_path_append(id) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(.POCKETHOSTR_UA) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}


# check that the params are those supported by PocketBase
check_view_params <- function(params, call = rlang::caller_env()) {
  param_names <- names(params)
  possible_param_names <- c("expand", "fields")
  non_supported <- setdiff(param_names, possible_param_names)
  if (length(non_supported) > 0) {
    cli::cli_abort("{.params {non_supported}}: Not supported.", call = call)
  }
}



