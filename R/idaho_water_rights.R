# Source types excluded from the standard Idaho surface-water allocation input.
.idwr_excluded_sources <- c(
  "GROUND WATER", "WASTE DITCH", "WASTE WATER", "WASTEWATER", "SEEPAGE"
)
.idwr_excluded_diversion_types <- "B"

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
#'   exclude. Defaults to groundwater, waste-ditch/wastewater, and seepage
#'   records. Matching is case-insensitive.
#' @param exclude_status Optional status name or vector of status names to
#'   exclude. Matching is case-insensitive.
#' @param exclude_diversion_type Optional IDWR `DiversionType` code or codes to
#'   exclude. Defaults to `"B"` (Beginning of Stream Flow), retaining Ending
#'   of Stream Flow and ordinary point locations.
#' @param layer Layer name in `local_path`.
#' @return An `sf` object of filtered PODs.
#' @export
get_idwr_pods <- function(local_path, filter_geom = NULL, source = NULL,
                          status = "Active", exclude_source = .idwr_excluded_sources,
                          exclude_status = NULL,
                          exclude_diversion_type = .idwr_excluded_diversion_types,
                          layer = "PODRight") {
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
  if (!is.null(exclude_diversion_type)) {
    if (!"DiversionType" %in% names(pods)) {
      stop("`", layer, "` is missing the IDWR `DiversionType` field.", call. = FALSE)
    }
    pods <- pods[
      !toupper(trimws(pods$DiversionType)) %in% toupper(trimws(exclude_diversion_type)),
      , drop = FALSE
    ]
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

.idwr_conditions_table <- function(html) {
  tables <- rvest::html_elements(html, "table")
  parsed <- lapply(tables, rvest::html_table, convert = FALSE)
  is_conditions <- vapply(
    parsed,
    function(table) {
      names_lower <- tolower(trimws(names(table)))
      "code" %in% names_lower && any(names_lower %in% c("conditions", "condtions"))
    },
    logical(1)
  )
  if (!any(is_conditions)) {
    return(data.frame(
      condition_code = character(), condition_text = character(),
      stringsAsFactors = FALSE
    ))
  }
  conditions <- parsed[[which(is_conditions)[1L]]]
  names_lower <- tolower(trimws(names(conditions)))
  code_column <- names(conditions)[match("code", names_lower)]
  text_column <- names(conditions)[match(
    TRUE, names_lower %in% c("conditions", "condtions")
  )]
  condition_code <- trimws(as.character(conditions[[code_column]]))
  condition_code[!nzchar(condition_code)] <- NA_character_
  data.frame(
    condition_code = condition_code,
    condition_text = trimws(as.character(conditions[[text_column]])),
    stringsAsFactors = FALSE
  )
}

.idwr_scrape_report_details <- function(report_urls, retries) {
  report_urls <- unique(stats::na.omit(report_urls))
  details <- lapply(report_urls, function(report_url) {
    tryCatch({
      response <- httr2::request(report_url) |>
        httr2::req_retry(max_tries = retries + 1L) |>
        httr2::req_perform()
      html <- rvest::read_html(httr2::resp_body_string(response))
      uses <- .idwr_water_uses_table(html)
      if (!nrow(uses)) uses <- uses[1L, , drop = FALSE]
      uses$WRReport <- report_url
      uses$scrape_error <- NA_character_
      conditions <- .idwr_conditions_table(html)
      if (nrow(conditions)) {
        conditions$WRReport <- report_url
        conditions$scrape_error <- NA_character_
      } else {
        conditions <- data.frame(
          condition_code = character(), condition_text = character(),
          WRReport = character(), scrape_error = character(),
          stringsAsFactors = FALSE
        )
      }
      list(uses = uses, conditions = conditions)
    }, error = function(error) {
      list(
        uses = data.frame(
          beneficial_use = NA_character_, from = NA_character_, to = NA_character_,
          diversion_rate = NA_real_, diversion_rate_unit = NA_character_,
          volume = NA_real_, vol_unit = NA_character_, WRReport = report_url,
          scrape_error = conditionMessage(error), stringsAsFactors = FALSE
        ),
        conditions = data.frame(
          condition_code = NA_character_, condition_text = NA_character_,
          WRReport = report_url, scrape_error = conditionMessage(error),
          stringsAsFactors = FALSE
        )
      )
    })
  })
  list(
    uses = do.call(rbind, lapply(details, `[[`, "uses")),
    conditions = do.call(rbind, lapply(details, `[[`, "conditions"))
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
  .idwr_scrape_report_details(report_urls, retries)$uses
}

#' Scrape Conditions from Idaho water-right reports
#'
#' Extract the report `Conditions` table as one row per condition code. This
#' normalized output is intended for EDA and audit, rather than duplicating a
#' POD once for every condition.
#'
#' @param report_urls A character vector of `WRReport` URLs.
#' @param retries Number of retry attempts for a temporary request failure.
#' @return A data frame with `condition_code`, `condition_text`, `WRReport`,
#'   and `scrape_error`.
#' @export
scrape_idwr_conditions <- function(report_urls, retries = 2L) {
  .idwr_scrape_report_details(report_urls, retries)$conditions
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
  details <- .idwr_scrape_report_details(pods$WRReport, retries = retries)
  condition_summary <- details$conditions |>
    dplyr::filter(!is.na(.data$condition_code) | !is.na(.data$condition_text)) |>
    dplyr::group_by(.data$WRReport) |>
    dplyr::summarise(
      condition_codes = paste(stats::na.omit(.data$condition_code), collapse = "; "),
      condition_text = paste(.data$condition_text, collapse = "\n\n"),
      .groups = "drop"
    )
  pods |>
    dplyr::left_join(details$uses, by = "WRReport") |>
    dplyr::left_join(condition_summary, by = "WRReport")
}

#' Standardize Idaho water rights
#'
#' Convert expanded IDWR PODs into WaterRAT's canonical cross-state schema.
#' `diversion_rate` is retained in its reported unit and `max_flow_cfs` is set
#' only when that unit is CFS; rates in other units remain missing rather than
#' being converted with an unsupported assumption.
#'
#' @param pods_by_use An `sf` object from [expand_idwr_pods_by_use()].
#' @return Canonical WaterRAT water-right records.
#' @export
standardize_idwr_water_rights <- function(pods_by_use) {
  required <- c("WaterRightNumber", "PointOfDiversionID", "Status", "Source", "WRReport",
                "beneficial_use", "from", "to", "diversion_rate", "diversion_rate_unit",
                "volume", "vol_unit")
  missing <- setdiff(required, names(pods_by_use))
  if (!inherits(pods_by_use, "sf") || length(missing)) {
    stop("Idaho PODs are missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  use_number <- ave(seq_len(nrow(pods_by_use)), pods_by_use$PointOfDiversionID,
                    FUN = seq_along)
  rate_unit <- toupper(trimws(pods_by_use$diversion_rate_unit))
  idwr_uses <- if ("Uses" %in% names(pods_by_use)) as.character(pods_by_use$Uses) else NA_character_
  condition_codes <- if ("condition_codes" %in% names(pods_by_use)) pods_by_use$condition_codes else NA_character_
  condition_text <- if ("condition_text" %in% names(pods_by_use)) pods_by_use$condition_text else NA_character_
  instream_pattern <- paste(
    "INSTREAM",
    "MINI(?:M|N)UM[[:space:]]+STREAM[[:space:]]+FLOW",
    "WILD[[:space:]]+AND[[:space:]]+SCENIC[[:space:]]+RIVER",
    sep = "|"
  )
  has_condition_148 <- grepl(
    "(^|;[[:space:]]*)148([[:space:]]*;|$)",
    dplyr::coalesce(condition_codes, "")
  )
  is_instream <- grepl(instream_pattern, dplyr::coalesce(pods_by_use$beneficial_use, ""), ignore.case = TRUE) |
    grepl(instream_pattern, dplyr::coalesce(idwr_uses, ""), ignore.case = TRUE) |
    has_condition_148
  result <- dplyr::transmute(
    pods_by_use,
    state = "ID",
    right_id = trimws(.data$WaterRightNumber),
    site_id = as.character(.data$PointOfDiversionID),
    record_id = paste("ID", .data$PointOfDiversionID, use_number, sep = ":"),
    status = .data$Status,
    source = .data$Source,
    beneficial_use = .data$beneficial_use,
    diversion_start = dplyr::na_if(.data$from, ""),
    diversion_end = dplyr::na_if(.data$to, ""),
    max_flow_cfs = dplyr::if_else(rate_unit == "CFS", as.numeric(.data$diversion_rate), NA_real_),
    diversion_rate = as.numeric(.data$diversion_rate),
    diversion_rate_unit = .data$diversion_rate_unit,
    volume = as.numeric(.data$volume),
    volume_unit = .data$vol_unit,
    is_instream = is_instream,
    report_url = .data$WRReport,
    idwr_uses = idwr_uses,
    condition_codes = condition_codes,
    condition_text = condition_text
  )
  validate_water_rights(result)
  result
}

#' Build a cached Idaho canonical water-right layer
#'
#' Filter, scrape, expand, and standardize IDWR points of diversion, then write
#' the canonical output to a local GeoPackage. Run this deliberately when IDWR
#' data change; the state-wide scrape is intentionally not performed during a
#' routine targets pipeline run.
#'
#' @param local_path Path to the IDWR `PODRight` geodatabase.
#' @param filter_geom Idaho analysis boundary.
#' @param cache_path Output GeoPackage path for canonical records.
#' @param exclude_source Source values to exclude before scraping.
#' @param layer Output layer name.
#' @param retries Number of retry attempts per report request.
#' @param month Analysis month to retain after standardization. Defaults to
#'   August, matching FlowMet's August streamflow metric.
#' @param include_missing_period Whether to retain records without a reported
#'   diversion period.
#' @return `cache_path`, invisibly.
#' @export
cache_idwr_water_rights <- function(local_path, filter_geom, cache_path,
                                    exclude_source = .idwr_excluded_sources,
                                    layer = "water_rights", retries = 2L,
                                    month = 8L, include_missing_period = TRUE) {
  water_rights <- get_idwr_pods(
    local_path = local_path,
    filter_geom = filter_geom,
    exclude_source = exclude_source
  ) |>
    expand_idwr_pods_by_use(retries = retries) |>
    standardize_idwr_water_rights() |>
    filter_water_rights_month(
      month = month,
      include_missing_period = include_missing_period
    )

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  sf::write_sf(
    water_rights, cache_path, layer = layer,
    delete_layer = file.exists(cache_path), quiet = TRUE
  )
  invisible(cache_path)
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

#' Refresh Idaho water rights without re-scraping unchanged POD reports
#'
#' Read the current IDWR POD layer, compare its stable identity, report URL,
#' source status, and location to the Idaho rows already in the combined
#' prepped cache, and scrape only new or changed POD reports. Unchanged cached
#' canonical rows are reused before [refresh_network_water_rights()] updates
#' COMIDs, ownership, and the combined cache.
#'
#' @param local_path Path to the IDWR `PODRight` geodatabase.
#' @param filter_geom Idaho analysis boundary.
#' @param fs_boundary Forest Service ownership geometry for Idaho.
#' @param nldi_cache_path State-specific NLDI checkpoint path.
#' @param cache_path Combined prepped water-right GeoPackage path.
#' @param month Analysis month.
#' @param retries Report-request retries for new or changed PODs.
#' @param layer IDWR source layer name.
#' @param max_pods Optional maximum number of filtered POD locations to process.
#'   Intended for small smoke tests; leave `NULL` for a complete refresh.
#' @param force_rescrape Whether to scrape every currently eligible Idaho POD.
#'   Use this when IDWR has revised report-page content without changing the
#'   POD-layer identity or report URL.
#' @param ... Additional arguments passed to [refresh_network_water_rights()].
#' @return A list from [refresh_network_water_rights()] with additional
#'   `scraped_pod_count` and `reused_pod_count` fields.
#' @export
refresh_idwr_network_water_rights <- function(
    local_path, filter_geom, fs_boundary, nldi_cache_path,
    cache_path = file.path("data", "cache", "prepped", "water_rights.gpkg"),
    month = 8L, retries = 2L, layer = "PODRight", max_pods = NULL,
    force_rescrape = FALSE, ...) {
  pods <- get_idwr_pods(local_path = local_path, filter_geom = filter_geom, layer = layer)
  if (!is.null(max_pods)) {
    max_pods <- as.integer(max_pods)
    if (length(max_pods) != 1L || is.na(max_pods) || max_pods < 1L) {
      stop("`max_pods` must be one positive integer or NULL.", call. = FALSE)
    }
    pods <- pods[seq_len(min(nrow(pods), max_pods)), , drop = FALSE]
  }
  key_from_pods <- function(x) {
    coordinates <- sf::st_coordinates(sf::st_transform(x, 4326))
    paste(
      trimws(as.character(x$WaterRightNumber)), as.character(x$PointOfDiversionID),
      trimws(as.character(x$Status)), trimws(as.character(x$Source)),
      trimws(as.character(x$WRReport)),
      format(coordinates[, 1L], digits = 15, trim = TRUE),
      format(coordinates[, 2L], digits = 15, trim = TRUE), sep = "|"
    )
  }
  cached <- if (file.exists(cache_path)) {
    all_cached <- tryCatch(read_prepped_water_rights(cache_path), error = function(error) NULL)
    if (is.null(all_cached)) NULL else all_cached[all_cached$state == "ID", , drop = FALSE]
  } else {
    NULL
  }
  key_from_cached <- function(x) {
    coordinates <- sf::st_coordinates(sf::st_transform(x, 4326))
    paste(
      trimws(as.character(x$right_id)), as.character(x$site_id),
      trimws(as.character(x$status)), trimws(as.character(x$source)),
      trimws(as.character(x$report_url)),
      format(coordinates[, 1L], digits = 15, trim = TRUE),
      format(coordinates[, 2L], digits = 15, trim = TRUE), sep = "|"
    )
  }
  pod_key <- key_from_pods(pods)
  cached_key <- if (is.null(cached)) character() else key_from_cached(cached)
  unchanged_pod <- !isTRUE(force_rescrape) & pod_key %in% cached_key
  new_or_changed <- pods[!unchanged_pod, , drop = FALSE]
  # A POD can yield several canonical Water Uses records. Keep every cached
  # canonical row for an unchanged POD rather than only the first match.
  reused <- if (is.null(cached)) pods[0, , drop = FALSE] else cached[cached_key %in% pod_key[unchanged_pod], , drop = FALSE]
  if (nrow(reused)) {
    reused <- dplyr::select(reused, -dplyr::any_of(c("comid", "nldi_error", "fs_intersection")))
  }
  scraped <- if (nrow(new_or_changed)) {
    new_or_changed |>
      expand_idwr_pods_by_use(retries = retries) |>
      standardize_idwr_water_rights() |>
      filter_water_rights_month(month = month)
  } else {
    NULL
  }
  bind_rows <- function(x, y) {
    x_geometry <- attr(x, "sf_column")
    y_geometry <- attr(y, "sf_column")
    if (!identical(x_geometry, y_geometry)) {
      names(y)[names(y) == y_geometry] <- x_geometry
      sf::st_geometry(y) <- x_geometry
    }
    rbind(x, y)
  }
  current <- if (is.null(scraped)) reused else if (!nrow(reused)) scraped else dplyr::bind_rows(reused, scraped)
  if (!nrow(current)) {
    stop("No current Idaho PODs remain after filtering; remove Idaho from the combined cache explicitly.", call. = FALSE)
  }
  result <- refresh_network_water_rights(
    current, fs_boundary = fs_boundary, nldi_cache_path = nldi_cache_path,
    cache_path = cache_path, ...
  )
  result$scraped_pod_count <- nrow(new_or_changed)
  result$reused_pod_count <- nrow(reused)
  result
}
