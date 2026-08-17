#' Canonical water-right fields
#'
#' Return the state-agnostic fields required by WateRAT's analysis utilities.
#' Each state adapter must return these fields, with a point geometry, before
#' entering the targets pipeline.
#'
#' @return A named character vector of canonical field names and definitions.
#' @export
water_right_schema <- function() {
  c(
    state = "Two-letter state abbreviation.",
    right_id = "State-issued water-right identifier.",
    site_id = "Stable point-of-diversion identifier.",
    record_id = "Unique site and beneficial-use record identifier.",
    status = "Source-system right status.",
    source = "Source waterbody or source type.",
    beneficial_use = "Beneficial use associated with this record.",
    diversion_start = "Diversion start date as MM/DD, when available.",
    diversion_end = "Diversion end date as MM/DD, when available.",
    max_flow_cfs = "Authorized diversion rate in cubic feet per second.",
    diversion_rate = "Reported authorized diversion rate.",
    diversion_rate_unit = "Unit reported for diversion_rate.",
    volume = "Reported authorized volume.",
    volume_unit = "Unit reported for volume.",
    is_instream = "Whether the use is instream.",
    report_url = "Source record URL, when available."
  )
}

#' Validate canonical water-right records
#'
#' @param water_rights An `sf` object to validate.
#' @return `water_rights`, invisibly.
#' @export
validate_water_rights <- function(water_rights) {
  required <- names(water_right_schema())
  missing <- setdiff(required, names(water_rights))
  if (!inherits(water_rights, "sf") || length(missing)) {
    stop(
      "`water_rights` must be an sf object with canonical fields: ",
      paste(required, collapse = ", "), ". Missing: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  if (anyNA(water_rights$record_id) || anyDuplicated(water_rights$record_id)) {
    stop("`record_id` must be non-missing and unique.", call. = FALSE)
  }
  if (!is.numeric(water_rights$max_flow_cfs) || !is.logical(water_rights$is_instream)) {
    stop("`max_flow_cfs` must be numeric and `is_instream` must be logical.", call. = FALSE)
  }
  invisible(water_rights)
}

#' Filter canonical water rights to a month
#'
#' Retain records whose diversion period includes the 15th day of `month`.
#' This selects the correct authorized rate from month-by-month records while
#' retaining annual records. Records without a reported diversion period are
#' retained by default because their period cannot be determined.
#'
#' @param water_rights Canonical water-right records.
#' @param month Month number from 1 through 12.
#' @param include_missing_period Whether to retain records with a missing start
#'   or end diversion date.
#' @return An `sf` subset of `water_rights`.
#' @export
filter_water_rights_month <- function(water_rights, month = 8L,
                                      include_missing_period = TRUE) {
  validate_water_rights(water_rights)
  month <- as.integer(month)
  if (length(month) != 1L || is.na(month) || month < 1L || month > 12L) {
    stop("`month` must be an integer from 1 through 12.", call. = FALSE)
  }

  parse_mmdd <- function(x) {
    x <- trimws(x)
    as.Date(ifelse(is.na(x) | !nzchar(x), NA_character_, paste0("2000-", x)), format = "%Y-%m/%d")
  }
  start <- parse_mmdd(water_rights$diversion_start)
  end <- parse_mmdd(water_rights$diversion_end)
  reference <- as.Date(sprintf("2000-%02d-15", month))
  missing_period <- is.na(start) | is.na(end)
  within_period <- ifelse(
    start <= end,
    reference >= start & reference <= end,
    reference >= start | reference <= end
  )
  keep <- if (include_missing_period) missing_period | within_period else !missing_period & within_period
  water_rights[keep, , drop = FALSE]
}

#' Standardize Montana water rights
#'
#' @param pods An `sf` object from Montana's `WRQS_PODS` layer.
#' @return Canonical WateRAT water-right records.
#' @export
standardize_mt_water_rights <- function(pods) {
  required <- c("WRKEY", "PODV_ID_SEQ", "WR_STATUS", "SOURCE_NAME", "MEANS_OF_DIV",
                "MAX_FLOW_CFS", "MAX_FLOW_RT", "FLOW_RT_UNIT", "MAX_VOL")
  missing <- setdiff(required, names(pods))
  if (!inherits(pods, "sf") || length(missing)) {
    stop("Montana PODs are missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  result <- dplyr::transmute(
    pods,
    state = "MT",
    right_id = as.character(.data$WRKEY),
    site_id = as.character(.data$PODV_ID_SEQ),
    record_id = paste("MT", .data$PODV_ID_SEQ, sep = ":"),
    status = .data$WR_STATUS,
    source = .data$SOURCE_NAME,
    beneficial_use = NA_character_,
    diversion_start = NA_character_,
    diversion_end = NA_character_,
    max_flow_cfs = as.numeric(.data$MAX_FLOW_CFS),
    diversion_rate = as.numeric(.data$MAX_FLOW_RT),
    diversion_rate_unit = .data$FLOW_RT_UNIT,
    volume = as.numeric(.data$MAX_VOL),
    volume_unit = NA_character_,
    is_instream = toupper(trimws(.data$MEANS_OF_DIV)) == "INSTREAM",
    report_url = NA_character_
  )
  validate_water_rights(result)
  result
}

#' Get canonical water rights for a state
#'
#' Dispatch to a state adapter and return records in the canonical WateRAT
#' schema. New state support belongs in a dedicated adapter, not in downstream
#' analysis utilities.
#'
#' @param state Two-letter state abbreviation.
#' @param filter_geom `sf` geometry used to filter points of diversion.
#' @param local_path State water-right dataset path.
#' @param ... State-specific retrieval arguments passed to the adapter.
#' @return Canonical WateRAT water-right records.
#' @export
get_state_water_rights <- function(state, filter_geom, local_path, ...) {
  state <- toupper(state)
  if (state == "MT") {
    pods <- get_mtwr(
      filter_geom = filter_geom,
      layer = "WRQS_PODS",
      local_path = local_path,
      ...
    ) |>
      date_cleaning()
    return(standardize_mt_water_rights(pods))
  }
  if (state == "ID") {
    pods <- get_idwr_pods(
      local_path = local_path,
      filter_geom = filter_geom,
      ...
    ) |>
      expand_idwr_pods_by_use()
    return(standardize_idwr_water_rights(pods))
  }
  stop("No WateRAT adapter is registered for state `", state, "`.", call. = FALSE)
}
