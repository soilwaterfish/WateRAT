#' Canonical water-right fields
#'
#' Return the state-agnostic fields required by WaterRAT's analysis utilities.
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
#' Retain records whose diversion period overlaps `month`, then return one row
#' per state/POD. When a right reports multiple within-month periods at the same
#' POD, its numeric rates are represented by their simple mean. This prevents
#' half-month or other segmented Idaho Water Uses rows from becoming duplicate
#' diversion sites. Records without a reported diversion period are retained by
#' default because their period cannot be determined.
#'
#' @param water_rights Canonical water-right records.
#' @param month Month number from 1 through 12.
#' @param include_missing_period Whether to retain records with a missing start
#'   or end diversion date.
#' @return An `sf` subset of `water_rights` with one row per state/POD.
#' @export
filter_water_rights_month <- function(water_rights, month = 8L,
                                      include_missing_period = TRUE) {
  validate_water_rights(water_rights)
  month <- as.integer(month)
  if (length(month) != 1L || is.na(month) || month < 1L || month > 12L) {
    stop("`month` must be an integer from 1 through 12.", call. = FALSE)
  }

  mean_or_na <- function(x) {
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    if (length(x)) mean(x) else NA_real_
  }
  collapsed_text <- function(x) {
    x <- trimws(as.character(x))
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x)) paste(x, collapse = "; ") else NA_character_
  }
  single_unit <- function(x) {
    x <- trimws(as.character(x))
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x) == 1L) x else NA_character_
  }
  parse_mmdd <- function(x) {
    x <- trimws(x)
    as.Date(ifelse(is.na(x) | !nzchar(x), NA_character_, paste0("2000-", x)), format = "%Y-%m/%d")
  }
  start <- parse_mmdd(water_rights$diversion_start)
  end <- parse_mmdd(water_rights$diversion_end)
  month_start <- as.Date(sprintf("2000-%02d-01", month))
  month_end <- seq(month_start, by = "month", length.out = 2L)[[2L]] - 1L
  missing_period <- is.na(start) | is.na(end)
  overlaps_month <- ifelse(
    start <= end,
    start <= month_end & end >= month_start,
    start <= month_end | end >= month_start
  )
  keep <- if (include_missing_period) missing_period | overlaps_month else !missing_period & overlaps_month
  water_rights <- water_rights[keep, , drop = FALSE]
  if (!nrow(water_rights)) return(water_rights)

  site_groups <- split(
    seq_len(nrow(water_rights)),
    paste(water_rights$state, water_rights$site_id, sep = "\r")
  )
  result <- lapply(site_groups, function(rows) {
    site <- water_rights[rows[[1L]], , drop = FALSE]
    if (length(rows) == 1L) return(site)
    records <- water_rights[rows, , drop = FALSE]
    rate_unit <- single_unit(records$diversion_rate_unit)
    volume_unit <- single_unit(records$volume_unit)
    site$record_id <- paste(site$state, site$site_id, sep = ":")
    site$right_id <- collapsed_text(records$right_id)
    site$beneficial_use <- collapsed_text(records$beneficial_use)
    site$diversion_start <- format(month_start, "%m/%d")
    site$diversion_end <- format(month_end, "%m/%d")
    site$max_flow_cfs <- mean_or_na(records$max_flow_cfs)
    site$diversion_rate <- if (identical(toupper(rate_unit), "CFS")) mean_or_na(records$diversion_rate) else NA_real_
    site$diversion_rate_unit <- rate_unit
    site$volume <- if (!is.na(volume_unit)) mean_or_na(records$volume) else NA_real_
    site$volume_unit <- volume_unit
    site$is_instream <- any(records$is_instream, na.rm = TRUE)
    for (field in c("source", "status", "report_url", "idwr_uses", "condition_codes", "condition_text")) {
      if (field %in% names(records)) site[[field]] <- collapsed_text(records[[field]])
    }
    site
  })
  result <- do.call(rbind, result)
  validate_water_rights(result)
  result
}

.mt_required_fields <- function(pods) {
  required <- c("WR_NUMBER", "WRKEY", "PODV_ID_SEQ", "WR_STATUS", "SOURCE_NAME", "MEANS_OF_DIV",
                "MAX_FLOW_CFS", "MAX_FLOW_RT", "FLOW_RT_UNIT", "MAX_VOL")
  missing <- setdiff(required, names(pods))
  if (!inherits(pods, "sf") || length(missing)) {
    stop("Montana PODs are missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

#' Prepare Montana POD candidates for hydrologic selection
#'
#' Each physical POD receives a temporary unique `site_id` and `record_id` so
#' it can be indexed to NHDPlus. Use [select_mt_downstream_pods()] before
#' [standardize_mt_water_rights()] to reduce each Montana `WRKEY` to its
#' downstream-most POD.
#'
#' @param pods An `sf` object from Montana's `WRQS_PODS` layer.
#' @return Canonical records for individual Montana POD candidates.
#' @export
standardize_mt_pod_candidates <- function(pods) {
  .mt_required_fields(pods)
  result <- dplyr::transmute(
    pods,
    state = "MT",
    right_id = as.character(.data$WR_NUMBER),
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

#' Select the downstream-most Montana POD for each water-right site
#'
#' NLDI assigns each candidate POD a COMID. Within each Montana `WRKEY`, the
#' candidate associated with the lowest NHDPlus `hydroseq` is retained because
#' HydroSeq decreases in the downstream direction. Ties are resolved by the
#' stable numeric `PODV_ID_SEQ`. If no candidate in a group has a COMID that is
#' present in the supplied network, the lowest POD ID is retained and marked as
#' a fallback for review.
#'
#' @param pods Montana `WRQS_PODS` records after eligibility filtering.
#' @param comid_index Output from [index_water_right_comids()] run on
#'   [standardize_mt_pod_candidates()].
#' @param network NHDPlus flowlines containing `comid` and `hydroseq`.
#' @return One raw Montana POD per `WRKEY`, with `selected_comid`,
#'   `pod_candidate_count`, and `pod_selection_method` audit fields.
#' @export
select_mt_downstream_pods <- function(pods, comid_index, network) {
  .mt_required_fields(pods)
  index_required <- c("record_id", "comid")
  missing_index <- setdiff(index_required, names(comid_index))
  if (length(missing_index)) {
    stop("`comid_index` is missing fields: ", paste(missing_index, collapse = ", "), call. = FALSE)
  }
  network_required <- c("comid", "hydroseq")
  missing_network <- setdiff(network_required, names(network))
  if (length(missing_network)) {
    stop("`network` is missing fields: ", paste(missing_network, collapse = ", "), call. = FALSE)
  }

  candidates <- dplyr::mutate(
    pods,
    .mt_record_id = paste("MT", .data$PODV_ID_SEQ, sep = ":")
  )
  index <- dplyr::distinct(
    dplyr::transmute(comid_index, .mt_record_id = as.character(.data$record_id), selected_comid = as.character(.data$comid)),
    .data$.mt_record_id,
    .keep_all = TRUE
  )
  hydroseq <- dplyr::distinct(
    dplyr::transmute(sf::st_drop_geometry(network), selected_comid = as.character(.data$comid), .mt_hydroseq = as.numeric(.data$hydroseq)),
    .data$selected_comid,
    .keep_all = TRUE
  )
  candidates <- dplyr::left_join(candidates, index, by = ".mt_record_id")
  candidates <- dplyr::left_join(candidates, hydroseq, by = "selected_comid")
  candidates <- dplyr::group_by(candidates, .data$WRKEY)
  candidates <- dplyr::mutate(
    candidates,
    pod_candidate_count = dplyr::n(),
    .mt_has_network_comid = any(!is.na(.data$.mt_hydroseq)),
    pod_selection_method = dplyr::case_when(
      .data$pod_candidate_count == 1L ~ "single_pod",
      .data$.mt_has_network_comid ~ "lowest_hydroseq",
      TRUE ~ "lowest_pod_id_fallback"
    ),
    .mt_rank = dplyr::if_else(
      .data$pod_candidate_count > 1L & .data$.mt_has_network_comid,
      dplyr::coalesce(.data$.mt_hydroseq, Inf),
      0
    )
  )
  candidates <- dplyr::arrange(candidates, .data$.mt_rank, .data$PODV_ID_SEQ, .by_group = TRUE)
  selected <- dplyr::slice(candidates, 1L)
  selected <- dplyr::ungroup(selected)
  dplyr::select(selected, -dplyr::starts_with(".mt_"))
}

#' Standardize selected Montana water rights
#'
#' Montana's public water-right number (`WR_NUMBER`) identifies the right and
#' `WRKEY` identifies the site-level record. Input must first be reduced to one
#' downstream-most POD per `WRKEY` with [select_mt_downstream_pods()].
#'
#' @param pods Selected `WRQS_PODS` records, one per `WRKEY`.
#' @return Canonical WaterRAT water-right records.
#' @export
standardize_mt_water_rights <- function(pods) {
  .mt_required_fields(pods)
  if (anyDuplicated(pods$WRKEY)) {
    stop(
      "Montana `WRKEY` values must be unique. Run `select_mt_downstream_pods()` before standardization.",
      call. = FALSE
    )
  }
  result <- dplyr::transmute(
    pods,
    state = "MT",
    right_id = as.character(.data$WR_NUMBER),
    site_id = as.character(.data$WRKEY),
    record_id = paste("MT", .data$WRKEY, sep = ":"),
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
  for (field in c("selected_comid", "pod_candidate_count", "pod_selection_method")) {
    if (field %in% names(pods)) result[[field]] <- pods[[field]]
  }
  validate_water_rights(result)
  result
}

#' Get canonical water rights for a state
#'
#' Dispatch to a state adapter and return records in the canonical WaterRAT
#' schema. New state support belongs in a dedicated adapter, not in downstream
#' analysis utilities.
#'
#' @param state Two-letter state abbreviation.
#' @param filter_geom `sf` geometry used to filter points of diversion.
#' @param local_path State water-right dataset path.
#' @param comid_index For Montana, NLDI COMID assignments created from
#'   [standardize_mt_pod_candidates()].
#' @param network For Montana, NHDPlus flowlines with `comid` and `hydroseq`
#'   used to retain the downstream-most POD per `WRKEY`.
#' @param ... State-specific retrieval arguments passed to the adapter.
#' @return Canonical WaterRAT water-right records.
#' @export
get_state_water_rights <- function(state, filter_geom, local_path,
                                   comid_index = NULL, network = NULL, ...) {
  state <- toupper(state)
  if (state == "MT") {
    pods <- get_mtwr(
      filter_geom = filter_geom,
      layer = "WRQS_PODS",
      local_path = local_path,
      ...
    ) |>
      date_cleaning()
    if (is.null(comid_index) || is.null(network)) {
      stop(
        "Montana requires `comid_index` and `network` so duplicate WRKEY PODs can be reduced downstream.",
        call. = FALSE
      )
    }
    return(standardize_mt_water_rights(select_mt_downstream_pods(pods, comid_index, network)))
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
  stop("No WaterRAT adapter is registered for state `", state, "`.", call. = FALSE)
}
