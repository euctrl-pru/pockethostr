#' list records
#'
#' @param app the application name
#' @param api the api to hit
#' @param collection the collection
#' @param ... the query params as supported by
#'            [PocketBase List/Search API](https://pocketbase.io/docs/api-records/#listsearch-records)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' ph_list_records(
#'   app,
#'   "/api/collections/",
#'   collection = "nw_traffic",
#'   perPage = 1,
#'   sort = "-FLIGHT_DATE",
#'   skipTotal = 1)
#'
#' # get only subset of columns
#' ph_list_records(
#'   app,
#'   "/api/collections/",
#'   collection = "nw_traffic",
#'   sort = "-FLIGHT_DATE",
#'   fileds = "id,FLIGHT_DATE,DAY_TFC"
#'   )
#' }
ph_list_records <- function(app, api, collection, ...) {
  params <- list(...)
  check_list_params(params)

  withCallingHandlers(
    v <- base_url(app = app) |>
      httr2::request() |>
      httr2::req_url_path_append(api) |>
      httr2::req_url_path_append(collection) |>
      httr2::req_url_path_append("records") |>
      httr2::req_url_query(!!!params) |>
      httr2::req_user_agent(.POCKETHOSTR_UA) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    httr2_http = function(cnd) {
      rlang::abort("Incorrect request", parent = cnd)
    }
  )

  e <- v |>
    purrr::pluck("items") |>
    tibble::enframe() |>
    tidyr::unnest_wider(.data$value)
  e |>
    dplyr::select(-.data$name) |>
    dplyr::mutate(
      page = v$page,
      perPage = v$perPage,
      totalItems = v$totalItems,
      totalPages = v$totalPages
    )

}

# check that the params are those supported by PocketBase
check_list_params <- function(params, call = rlang::caller_env()) {
  param_names <- names(params)
  possible_param_names <- c("page", "perPage", "sort", "filter", "expand", "fields", "skipTotal")
  non_supported <- setdiff(param_names, possible_param_names)
  if (length(non_supported) > 0) {
    cli::cli_abort("{.params {non_supported}}: Not supported.", call = call)
  }
}
