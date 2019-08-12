#' Median Home Price Data
#'
#' A dataset containing median home sale prices for all zip codes in North Carolina from January 2015 - July 2019.
#'
#' @format A data frame of 26840 rows and 6 columns
#' \describe{
#'  \item{date}{Date character value formatted as monthly time series}
#'  \item{zip}{Zip code character values}
#'  \item{city}{City character values}
#'  \item{state}{State character value, formatted as "NC"}
#'  \item{county}{County character values}
#'  \item{medianSalePrice}{Numeric value containing monthly median sale price}
#' }
#' @source RE Market View
"home_sales"
