#' authenticate
#'
#' @param app the app name
#' @param api the api to hit
#' @param username the username to use
#' @param password the password to use
#'
#' @return a json
#' @export
#'
#' @examples
#' \dontrun{
#' ph_authenticate_admin_username_password <- function(
#'    "eurocontrol-data",
#'    "/api/admins/auth-with-password",
#'    username,
#'    password)
#' }
ph_authenticate_admin_username_password <- function(app, api, username, password) {
  base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_user_agent(.POCKETHOSTR_UA) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(identity = username, password = password)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
