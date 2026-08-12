#' Get filtered Idaho points of diversion
#'
#' Read the Idaho Department of Water Resources `PODRight` layer and apply
#' commonly used water-right filters before requesting report pages. Filtering
#' first is important because each retained unique `WRReport` URL is fetched
#' separately.
#'
#' @param local_path Path to an IDWR geodatabase containing the `PODRight`
#'   layer.
#' @param filter_geom Optional `sf` geometry used to retain intersecting PODs.
#' @param source Optional source name or vector of source names.
#' @param status Optional status name or vector of status names. Matching is
#'   case-insensitive.
#' @param exclude_source Optional source name or vector of source names to
#'   exclude. Matching is case-insensitive.
#' @param exclude_status Optional status name or vector of status names to
#'   exclude. Matching is case-insensitive.
#' @param layer Layer name in `local_path`.
#' @return An `sf` object of filtered PODs.
#' @export
get_idwr_pods <- function(local_path, filter_geom = NULL, source = NULL,
                          status = "Active", exclude_source = NULL,
                          exclude_status = NULL, layer = "PODRight") {
  pods <- sf::read_sf(local_path, layer = layer, quiet = TRUE)

  if (!is.null(filter_geom)) {
    filter_geom <- sf::st_transform(filter_geom, sf::st_crs(pods))
    pods <- pods[lengths(sf::st_intersects(pods, filter_geom)) > 0L, , drop = FALSE]
  }
  if (!is.null(source)) {
    pods <- pods[toupper(trimws(pods$Source)) %in% toupper(trimws(source)), , drop = FALSE]
  }
  if (!is.null(status)) {
    pods <- pods[toupper(trimws(pods$Status)) %in% toupper(trimws(status)), , drop = FALSE]
  }
  if (!is.null(exclude_source)) {
    pods <- pods[!toupper(trimws(pods$Source)) %in% toupper(trimws(exclude_source)), , drop = FALSE]
  }
  if (!is.null(exclude_status)) {
    pods <- pods[!toupper(trimws(pods$Status)) %in% toupper(trimws(exclude_status)), , drop = FALSE]
  }
  pods
}

.idwr_parse_quantity <- function(x) {
  x <- trimws(x)
  number_text <- sub("^[[:space:]]*([-+]?[0-9,]*\\.?[0-9]+).*$", "\\1", x)
  value <- suppressWarnings(as.numeric(gsub(",", "", number_text, fixed = TRUE)))
  unit <- trimws(sub(
    "^[[:space:]]*[-+]?[0-9,]*\\.?[0-9]+[[:space:]]*", "", x
  ))
  unit[!nzchar(unit)] <- NA_character_
  list(value = value, unit = unit)
}

.idwr_water_uses_table <- function(html) {
  tables <- rvest::html_elements(html, "table")
  parsed <- lapply(tables, rvest::html_table, convert = FALSE)
  is_water_uses <- vapply(
    parsed,
    function(table) all(c("Beneficial Use", "From", "To", "Diversion Rate", "Volume") %in% names(table)),
    logical(1)
  )
  if (!any(is_water_uses)) {
    return(data.frame(
      beneficial_use = character(), from = character(), to = character(),
      diversion_rate = numeric(), diversion_rate_unit = character(),
      volume = numeric(), vol_unit = character(), stringsAsFactors = FALSE
    ))
  }

  uses <- parsed[[which(is_water_uses)[1L]]]
  uses <- uses[toupper(trimws(uses[["Beneficial Use"]])) != "TOTAL", , drop = FALSE]
  uses <- uses[nzchar(trimws(uses[["Beneficial Use"]])), , drop = FALSE]
  rate <- .idwr_parse_quantity(uses[["Diversion Rate"]])
  volume <- .idwr_parse_quantity(uses[["Volume"]])
  data.frame(
    beneficial_use = trimws(uses[["Beneficial Use"]]),
    from = trimws(uses[["From"]]),
    to = trimws(uses[["To"]]),
    diversion_rate = rate$value,
    diversion_rate_unit = rate$unit,
    volume = volume$value,
    vol_unit = volume$unit,
    stringsAsFactors = FALSE
  )
}

#' Scrape Water Uses from Idaho water-right reports
#'
#' Extract the `Water Uses` table from IDWR `WRReport` URLs. The report's
#' summary `TOTAL` row is excluded; each returned row represents one beneficial
#' use.
#'
#' @param report_urls A character vector of `WRReport` URLs.
#' @param retries Number of retry attempts for a temporary request failure.
#' @return A data frame with one row per URL and water use, including parsed
#'   numeric `diversion_rate`/`volume` and their units.
#' @export
scrape_idwr_water_uses <- function(report_urls, retries = 2L) {
  report_urls <- unique(stats::na.omit(report_urls))
  results <- lapply(report_urls, function(report_url) {
    tryCatch({
      response <- httr2::request(report_url) |>
        httr2::req_retry(max_tries = retries + 1L) |>
        httr2::req_perform()
      html <- rvest::read_html(httr2::resp_body_string(response))
      uses <- .idwr_water_uses_table(html)
      if (!nrow(uses)) {
        uses <- uses[1L, , drop = FALSE]
      }
      uses$WRReport <- report_url
      uses$scrape_error <- NA_character_
      uses
    }, error = function(error) {
      data.frame(
        beneficial_use = NA_character_, from = NA_character_, to = NA_character_,
        diversion_rate = NA_real_, diversion_rate_unit = NA_character_,
        volume = NA_real_, vol_unit = NA_character_, WRReport = report_url,
        scrape_error = conditionMessage(error), stringsAsFactors = FALSE
      )
    })
  })
  do.call(rbind, results)
}

#' Expand Idaho PODs by Water Use
#'
#' Duplicate each filtered POD once for every Water Uses row in its IDWR report.
#' The output keeps the original POD geometry and attributes, adding beneficial
#' use, diversion rate, and volume columns.
#'
#' @param pods An `sf` object returned by [get_idwr_pods()] with a `WRReport`
#'   column.
#' @param retries Number of retry attempts per report request.
#' @return An `sf` object with one row per POD-water-use combination.
#' @export
expand_idwr_pods_by_use <- function(pods, retries = 2L) {
  if (!inherits(pods, "sf") || !"WRReport" %in% names(pods)) {
    stop("`pods` must be an sf object containing a `WRReport` column.", call. = FALSE)
  }
  uses <- scrape_idwr_water_uses(pods$WRReport, retries = retries)
  dplyr::left_join(pods, uses, by = "WRReport")
}

#' Write expanded Idaho water uses
#'
#' @param pods_by_use An `sf` object from [expand_idwr_pods_by_use()].
#' @param path Output GeoPackage or writable File Geodatabase path.
#' @param layer Output layer name.
#' @return `path`, invisibly.
#' @export
write_idwr_pods_by_use <- function(pods_by_use, path, layer = "PODRightWaterUses") {
  if (!inherits(pods_by_use, "sf")) {
    stop("`pods_by_use` must be an sf object.", call. = FALSE)
  }
  sf::write_sf(pods_by_use, path, layer = layer, delete_layer = TRUE, quiet = TRUE)
  invisible(path)
}
