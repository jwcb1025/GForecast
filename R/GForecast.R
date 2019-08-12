#' Multidirectional Group Time-Series Forecasting'
#'
#' Description: Apply ARIMA or ETS to grouped time series data and specify direction of horizon.
#'
#' No further details.
#'
#' @param data "A data frame that includes time, group, and target variable to forecast."
#' @param groupId "Grouping variable in the data frame."
#' @param date "Name of the date column. Must be in '%Y-%m-%d' format."
#' @param target "Numeric vector containing the values to forecast."
#' @param type  "'ets' or 'auto.arima'. Default = 'ets'.
#'
#' @author James Craig
#'
#' @import dplyr
#' @import forecast
#' @import tidyquant
#' @import timetk
#' @import tidyr
#' @import sweep
#' @import purrr
#' @import zoo
#' @import lubridate
#' @examples
#'GForecast(home_sales, group = "zip", target = "medianSalePrice")
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item groupId
#'  \item date
#'  \item key
#'  \item target
#'  \item lo.80
#'  \item lo.95
#'  \item hi.80
#'  \item hi.95
#' }
#' @export



GForecast <- function(data, target, groupId, date, type = "ets", h = 12, fitted = FALSE) {
  if(missing(data)) {
    stop(print("No data found"))
  }

  temp_nest <- data %>%
    dplyr::mutate(date = lubridate::as_date(zoo::as.yearmon(get("date")))) %>%
    dplyr::select(.dots = c(get("groupId"), date, get("target"))) %>%
    dplyr::group_by(.dots1) %>%
    tidyr::nest(.key = "data.tbl")

  temp_ts <- temp_nest %>%
    dplyr::mutate(data.ts = purrr:: map(.x = data.tbl,
                                  .f       = tk_ts,
                                  select   = -.dots2, #Remove Date
                                  start    = 2015,
                                  freq     = 12
    ))

  temp_ts_fit <- temp_ts %>%
    dplyr::mutate(fit.type = purrr::map(data.ts, forecast::ets))

  temp_ts_fcast <- temp_ts_fit %>%
    mutate(fcast.type = map(fit.type, forecast::forecast, h = 12))

  temp_ts_fcast_tidy <- temp_ts_fcast %>%
    mutate(sweep = map(fcast.type, sweep::sw_sweep, fitted = FALSE, timetk_idx = TRUE))

  temp <- temp_ts_fcast_tidy %>% dplyr::select(.dots1, sweep) %>% tidyr::unnest(.drop = TRUE)

  names(temp) <- c(groupId, date, "key", target, "lo.80", "lo.95", "hi.80", "hi.95")

  return(temp)
}

