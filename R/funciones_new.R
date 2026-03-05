#' Convert Depth Values to POSIXct Dates
#'
#' @description
#' Converts a numeric vector of depth values to POSIXct datetime objects,
#' based on a CF-convention unit string (e.g., "months since 1960-01-01").
#' Fractional values are floored to the start of the corresponding time period.
#'
#' @param depth numeric vector. Values representing elapsed time units since
#'   the origin date. Fractional values are floored (e.g., 2.5 months -> 
#'   start of month 2).
#' @param depth_unit character string. A CF-convention time unit string of the
#'   form \code{"<unit> since <origin>"}, where:
#'   \itemize{
#'     \item \code{<unit>} is one of: \code{"seconds"}, \code{"minutes"},
#'       \code{"hours"}, \code{"days"}, \code{"months"}, \code{"years"}
#'     \item \code{<origin>} is a date or datetime string parseable by
#'       \code{as.POSIXct}, e.g. \code{"1960-01-01"} or
#'       \code{"1960-01-01 00:00:00"}
#'   }
#' @param tz character string. Time zone to use for the output POSIXct vector
#'   and for parsing the origin. Defaults to \code{"UTC"}.
#'
#' @return A POSIXct vector of the same length as \code{depth}, representing
#'   the start of each time period.
#'
#' @details
#' For \code{"months"} and \code{"years"}, \code{seq.Date()} is used internally
#' to correctly handle variable month lengths and leap years. For all other
#' units, arithmetic on the POSIXct origin is applied directly.
#'
#' @examples
#' depth <- c(252.5, 253.5, 254.5, 255.5, 256.5, 257.5)
#'
#' # Months
#' depth_to_posixct(depth, "months since 1960-01-01")
#' #> [1] "1981-01-01 UTC" "1981-02-01 UTC" "1981-03-01 UTC"
#' #> [4] "1981-04-01 UTC" "1981-05-01 UTC" "1981-06-01 UTC"
#'
#' # Years
#' depth_to_posixct(depth, "years since 1960-01-01")
#'
#' # Days
#' depth_to_posixct(depth, "days since 1960-01-01")
#'
#' # Hours
#' depth_to_posixct(depth, "hours since 1960-01-01 00:00:00")
#'
#' # Minutes
#' depth_to_posixct(depth, "minutes since 1960-01-01 00:00:00")
#'
#' # Seconds
#' depth_to_posixct(depth, "seconds since 1960-01-01 00:00:00")
#'
#' # Custom timezone
#' depth_to_posixct(depth, "months since 1960-01-01", tz = "America/New_York")
#'
#' @export
depth_to_posixct <- function(depth, depth_unit, tz = "UTC") {
  
  # --- Input validation ---
  if (!is.numeric(depth)) stop("'depth' must be a numeric vector.")
  if (!is.character(depth_unit) || length(depth_unit) != 1) {
    stop("'depth_unit' must be a single character string.")
  }
  if (!grepl(" since ", depth_unit)) {
    stop("'depth_unit' must follow the format '<unit> since <origin>', e.g. 'months since 1960-01-01'.")
  }
  
  # --- Parse unit and origin ---
  parts  <- strsplit(depth_unit, " since ")[[1]]
  unit   <- trimws(parts[1])
  origin <- as.POSIXct(trimws(parts[2]), tz = tz)
  
  if (is.na(origin)) {
    stop("Could not parse origin date from '", depth_unit, "'. ",
         "Ensure it follows a standard format like 'YYYY-MM-DD' or 'YYYY-MM-DD HH:MM:SS'.")
  }
  
  valid_units <- c("seconds", "minutes", "hours", "days", "months", "years")
  if (!unit %in% valid_units) {
    stop("Unsupported unit: '", unit, "'. Must be one of: ",
         paste(valid_units, collapse = ", "), ".")
  }
  
  # --- Convert depth to POSIXct ---
  dates <- switch(unit,
                  "seconds" = as.POSIXct(origin + floor(depth),         tz = tz),
                  "minutes" = as.POSIXct(origin + floor(depth) * 60,    tz = tz),
                  "hours"   = as.POSIXct(origin + floor(depth) * 3600,  tz = tz),
                  "days"    = as.POSIXct(origin + floor(depth) * 86400, tz = tz),
                  "months"  = {
                    d <- as.Date(origin)
                    as.POSIXct(
                      seq.Date(d, by = "month", length.out = max(floor(depth)) + 1)[floor(depth) + 1],
                      tz = tz
                    )
                  },
                  "years"   = {
                    d <- as.Date(origin)
                    as.POSIXct(
                      seq.Date(d, by = "year", length.out = max(floor(depth)) + 1)[floor(depth) + 1],
                      tz = tz
                    )
                  }
  )
  
  return(dates)
}