% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_record.R
\name{ph_view_record}
\alias{ph_view_record}
\title{View record}
\usage{
ph_view_record(app, api, collection, id, ...)
}
\arguments{
\item{app}{the app name}

\item{api}{the api}

\item{collection}{the collection name or id}

\item{id}{the record id}

\item{...}{the query params as supported by
\href{https://pocketbase.io/docs/api-records/#view-record}{PocketBase View API}}
}
\value{
a response
}
\description{
View record
}
\examples{
\dontrun{
app <- "eurocontrol-data-test"
collection <- "nw_traffic"
id <- "lufkj3346ygx9a2"

ph_view_record(app = app,
               api = "/api/collections",
               id = id,
               collection = collection,
               fields = "FLIGHT_DATE,DAY_TFC")
}
}
