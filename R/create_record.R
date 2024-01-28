#' create record
#'
#' @param app the app
#' @param api the api
#' @param collection the collection name or id
#' @param body the body
#' @param token the authentication token, depending on collection's `createRule`
#' @param ... extra params
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' adm <- ph_authenticate_admin_username_password(
#'   "eurocontrol-data",
#'   "/api/admins/auth-with-password",
#'   username,
#'   password)
#'   bd <- list(
#'     AVG_ROLLING_WEEK= 22065.4285714286,
#'     DAY_DIFF_PREV_YEAR_PERC= 0.035235526043,
#'     DAY_TFC= 24944,
#'     DAY_TFC_DIFF_2019_PERC= -0.11967531322,
#'     DIFF_ROLLING_WEEK_2019_PERC= -0.13323232323,
#'     DIFF_WEEK_PREV_YEAR_PERC= 0.027945081493,
#'     FLIGHT_DATE= "2024-01-01 00:00:00.000Z",
#'     Y2D_AVG_TFC_YEAR= 23172.4210526316,
#'     Y2D_DIFF_2019_PERC= -0.087499533673,
#'     Y2D_DIFF_PREV_YEAR_PERC= 0.051857514872,
#'     Y2D_TFC_YEAR= 440276
#'     )
#'  ph_create_record(
#'   app = "eurocontrol-data",
#'   api = "/api/collections",
#'   collection = "nw_traffic",
#'   token = adm$token,
#'   body = bd)
#' }
ph_create_record <- function(app, api, collection, body, token = NULL, ...) {
  params <- list(...)
  check_create_params(params)

  req <- base_url(app = app) |>
    httr2::request() |>
    httr2::req_url_path_append(api) |>
    httr2::req_url_path_append(collection) |>
    httr2::req_url_path_append("records") |>
    httr2::req_method("POST") |>
    httr2::req_user_agent(.POCKETHOSTR_UA) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_body_json(body)

  # add token if present. this depends on the collection's createRule
  if (!is.null(token)) {
    req <- req |>
      httr2::req_headers(Authorization = token)
  }

  withCallingHandlers(
    req |> httr2::req_perform(),
    httr2_http = function(cnd) {
      rlang::abort("Incorrect request", parent = cnd)
    }
  )
}


# check that the params are those supported by PocketBase
check_create_params <- function(params, call = rlang::caller_env()) {
  param_names <- names(params)
  possible_param_names <- c("expand", "fields")
  non_supported <- setdiff(param_names, possible_param_names)
  if (length(non_supported) > 0) {
    cli::cli_abort("{.params {non_supported}}: Not supported.", call = call)
  }
}
