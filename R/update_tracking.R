#' Create a water-right POD snapshot
#'
#' Create a dated snapshot of the full Montana Points of Diversion layer for
#' later comparison. Use [get_mtwr()] with `active_only = FALSE` before calling
#' this function so that a right which changes from active to retired remains
#' observable.
#'
#' @param pods An `sf` object from the `WRQS_PODS` layer.
#' @param snapshot_date Date recorded with the snapshot.
#' @param key Column containing the stable POD identifier.
#' @return An `sf` object with `analysis_active` and `snapshot_date` columns.
#' @export
water_right_snapshot <- function(pods, snapshot_date = Sys.Date(), key = "PODV_ID_SEQ") {
  required <- c(key, "WRKEY", "WR_STATUS", "MAX_FLOW_RT")
  missing <- setdiff(required, names(pods))
  if (!inherits(pods, "sf") || length(missing)) {
    stop("`pods` must be an sf object containing: ", paste(required, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(pods[[key]])) {
    stop("`pods` has duplicate values of `", key, "`; resolve them before snapshotting.", call. = FALSE)
  }

  dplyr::mutate(
    pods,
    analysis_active = toupper(trimws(.data$WR_STATUS)) == "ACTIVE" & !is.na(.data$MAX_FLOW_RT),
    snapshot_date = as.Date(snapshot_date)
  )
}

#' Write a water-right snapshot
#'
#' @param snapshot An `sf` object created by [water_right_snapshot()].
#' @param path Destination GeoPackage path. Store this outside the package
#'   repository or under its ignored `data/` directory.
#' @return The input `path`, invisibly.
#' @export
write_water_right_snapshot <- function(snapshot, path) {
  if (!inherits(snapshot, "sf")) {
    stop("`snapshot` must be an sf object.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::write_sf(snapshot, path, delete_dsn = file.exists(path), quiet = TRUE)
  invisible(path)
}

#' Read a water-right snapshot
#'
#' @param path A GeoPackage created by [write_water_right_snapshot()].
#' @return An `sf` object.
#' @export
read_water_right_snapshot <- function(path) {
  sf::read_sf(path, quiet = TRUE)
}

#' Detect added and retired water rights
#'
#' Compare two full POD snapshots. A right is considered active for analysis
#' when `WR_STATUS` is `ACTIVE` and `MAX_FLOW_RT` is present. A change from
#' ineligible to eligible is an addition; a change from eligible to ineligible,
#' including a missing record, is a retirement.
#'
#' @param previous, current `sf` POD snapshots created by
#'   [water_right_snapshot()].
#' @param key Column containing the stable POD identifier.
#' @return An `sf` object of changed PODs with a `change_type` column.
#' @export
detect_water_right_changes <- function(previous, current, key = "PODV_ID_SEQ") {
  required <- c(key, "analysis_active")
  for (snapshot in list(previous, current)) {
    if (!inherits(snapshot, "sf") || length(setdiff(required, names(snapshot)))) {
      stop("Both snapshots must be sf objects created by `water_right_snapshot()`.", call. = FALSE)
    }
    if (anyDuplicated(snapshot[[key]])) {
      stop("Snapshots must have unique values of `", key, "`.", call. = FALSE)
    }
  }

  old <- sf::st_drop_geometry(previous)
  new <- sf::st_drop_geometry(current)
  comparison <- dplyr::full_join(
    dplyr::transmute(old, !!key := .data[[key]], old_active = .data$analysis_active),
    dplyr::transmute(new, !!key := .data[[key]], new_active = .data$analysis_active),
    by = key
  )
  comparison <- dplyr::mutate(
    comparison,
    old_active = dplyr::coalesce(.data$old_active, FALSE),
    new_active = dplyr::coalesce(.data$new_active, FALSE),
    change_type = dplyr::case_when(
      !.data$old_active & .data$new_active ~ "added",
      .data$old_active & !.data$new_active ~ "retired",
      TRUE ~ NA_character_
    )
  )
  comparison <- dplyr::filter(comparison, !is.na(.data$change_type))

  added_keys <- comparison[[key]][comparison$change_type == "added"]
  retired_keys <- comparison[[key]][comparison$change_type == "retired"]
  added <- dplyr::mutate(current[current[[key]] %in% added_keys, , drop = FALSE], change_type = "added")
  retired <- dplyr::mutate(previous[previous[[key]] %in% retired_keys, , drop = FALSE], change_type = "retired")
  dplyr::bind_rows(added, retired)
}

#' Select downstream COMIDs affected by water-right changes
#'
#' A COMID is affected when its upstream drainage basin contains a changed POD.
#' Because each basin represents the area upstream of its COMID, this selects
#' the changed location's downstream network without a separate network query.
#'
#' @param changed_pods An `sf` object returned by [detect_water_right_changes()].
#' @param basins An `sf` object of upstream COMID basins from [get_pod_basin()].
#' @param comid Column in `basins` that contains COMIDs.
#' @return A character vector of affected COMIDs.
#' @export
affected_downstream_comids <- function(changed_pods, basins, comid = "comid") {
  if (!inherits(changed_pods, "sf") || !inherits(basins, "sf") || !comid %in% names(basins)) {
    stop("`changed_pods` and `basins` must be sf objects, and `basins` must contain `", comid, ".", call. = FALSE)
  }
  if (!nrow(changed_pods) || !nrow(basins)) {
    return(character())
  }
  changed_pods <- sf::st_transform(changed_pods, sf::st_crs(basins))
  intersects_change <- lengths(sf::st_intersects(basins, changed_pods)) > 0L
  unique(as.character(basins[[comid]][intersects_change]))
}

#' Select downstream basins affected by water-right changes
#'
#' @inheritParams affected_downstream_comids
#' @return An `sf` subset of `basins` that contains the changed PODs.
#' @export
affected_downstream_basins <- function(changed_pods, basins, comid = "comid") {
  affected <- affected_downstream_comids(changed_pods, basins, comid = comid)
  basins[as.character(basins[[comid]]) %in% affected, , drop = FALSE]
}
