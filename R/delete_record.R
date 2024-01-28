#' Delete record
#'
#' @param app the app name
#' @param api the api
#' @param collection the collection name or id
#' @param id the record id
#' @param token the authentication token, depending on collection's `deleteRule`
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' delete_record(app, api, id, token)
#' }
ph_delete_record <- function(app, api, collection, id, token = NULL) {
  req <- base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_url_path_append(collection) |>
    httr2::req_url_path_append("records") |>
    httr2::req_url_path_append(id) |>
    httr2::req_method("DELETE") |>
    httr2::req_user_agent(.POCKETHOSTR_UA)


  # add token if present. this depends on the collection's deleteRule
  if (!is.null(token)) {
    req <- req |>
      httr2::req_headers(Authorization = token)
  }

  req |>
    httr2::req_perform()
}


