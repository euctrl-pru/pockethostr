#' list records
#'
#' @param app the application name
#' @param api the api to hit
#' @param collection the collection
#' @param ... other query params
#' @param perPage number of records per page
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
#' }
ph_list_records <- function(app, api, collection, ..., perPage = 30) {
  params <- list(..., perPage = perPage)

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
