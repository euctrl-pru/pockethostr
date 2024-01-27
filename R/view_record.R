#' View record
#'
#' @param app the app name
#' @param api the api
#' @param collection the collection
#' @param id the record id
#' @param ... other query params
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' ph_view_record(app, api, id, fileds = "COL1,COL3")
#' }
ph_view_record <- function(app, api, collection, id, ...) {
  params <- list(...)
  base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_url_path_append(collection) |>
    httr2::req_url_path_append("records") |>
    httr2::req_url_path_append(id) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(.POCKETHOSTR_UA) |>
    httr2::req_perform()
}





