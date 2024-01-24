
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pockethostr

<!-- badges: start -->
<!-- badges: end -->

**WORK in PROGRESS** and subject to change.

This is a very early attempt to wrap the EUROCONTROL Data API from
[PoketHost](https://pockethost.io/ "PocketHost - Amazingly Simple PocketBase Hosting")
in R.

It will probably result in a more low-level generic package to support
[PocketBase](https://pocketbase.io/ "PocketBase - Open Source backend for your next SaaS and Mobile app in 1 file")/PocketHost
and a package specific for the EUROCONTROL Data App.

## Installation

You can install the development version of `pockethostr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("euctrl-pru/pockethostr")
```

## Example

List all records in the EUROCONTROL Data app:

``` r
library(pockethostr)

ph_list_records(
  app = "eurocontrol-data-test",
  api = "/api/collections/",
  collection = "nw_traffic",
  sort = "-FLIGHT_DATE")
```

# Development

In order to build the relevant pkgdown web pages, execute the following
code:

``` r
# How to build the pakgdown from behind proxied Internet
library(withr)
library(pkgdown)

with_options(list(pkgdown.internet = FALSE),
             build_site())
```
