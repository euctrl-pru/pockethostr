% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_record.R
\name{ph_create_record}
\alias{ph_create_record}
\title{create record}
\usage{
ph_create_record(app, api, collection, body, token = NULL, ...)
}
\arguments{
\item{app}{the app}

\item{api}{the api}

\item{collection}{the collection name or id}

\item{body}{the body}

\item{token}{the authentication token, depending on collection's \code{createRule}}

\item{...}{extra params}
}
\value{
a response
}
\description{
create record
}
\examples{
\dontrun{
adm <- ph_authenticate_admin_username_password(
  "eurocontrol-data",
  "/api/admins/auth-with-password",
  username,
  password)
  bd <- list(
    AVG_ROLLING_WEEK= 22065.4285714286,
    DAY_DIFF_PREV_YEAR_PERC= 0.035235526043,
    DAY_TFC= 24944,
    DAY_TFC_DIFF_2019_PERC= -0.11967531322,
    DIFF_ROLLING_WEEK_2019_PERC= -0.13323232323,
    DIFF_WEEK_PREV_YEAR_PERC= 0.027945081493,
    FLIGHT_DATE= "2024-01-01 00:00:00.000Z",
    Y2D_AVG_TFC_YEAR= 23172.4210526316,
    Y2D_DIFF_2019_PERC= -0.087499533673,
    Y2D_DIFF_PREV_YEAR_PERC= 0.051857514872,
    Y2D_TFC_YEAR= 440276
    )
 ph_create_record(
  app = "eurocontrol-data",
  api = "/api/collections",
  collection = "nw_traffic",
  token = adm$token,
  body = bd)
}
}
