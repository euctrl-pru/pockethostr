#' Delete record
#'
#' @param app the app name
#' @param api the api
#' @param collection the collection
#' @param id the record id
#' @param token the authentication token
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' delete_record(app, api, id, token)
#' }
ph_delete_record <- function(app, api, collection, id, token) {
  base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_url_path_append(collection) |>
    httr2::req_url_path_append("records") |>
    httr2::req_url_path_append(id) |>
    httr2::req_method("DELETE") |>
    httr2::req_user_agent(.POCKETHOSTR_UA) |>
    httr2::req_headers(Authorization = token) |>
    httr2::req_perform()
}


