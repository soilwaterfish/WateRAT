#' Prepare canonical water rights for network accumulation
#'
#' Join the persisted NLDI COMID index to canonical POD records and classify
#' each POD against Forest Service ownership. This is the durable handoff from
#' state-specific ingestion to network accumulation.
#'
#' @param water_rights Canonical WaterRAT water-right records.
#' @param comid_index Output from [index_water_right_comids()].
#' @param fs_boundary Forest Service ownership geometry for the analysis area.
#' @return Canonical `sf` records with `comid`, `nldi_error`, and
#'   `fs_intersection` fields.
#' @export
prepare_network_water_rights <- function(water_rights, comid_index, fs_boundary) {
  validate_water_rights(water_rights)
  # The combined cache may contain several state source CRSs. Persist one
  # geographic CRS so state rows can be appended without geometry coercion.
  water_rights <- sf::st_transform(water_rights, 4326)
  required <- c("record_id", "comid", "nldi_error")
  missing <- setdiff(required, names(comid_index))
  if (length(missing)) {
    stop("`comid_index` is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(comid_index$record_id)) {
    stop("`comid_index$record_id` must be unique.", call. = FALSE)
  }
  if (!inherits(fs_boundary, "sf") && !inherits(fs_boundary, "sfc")) {
    stop("`fs_boundary` must be an sf or sfc ownership geometry.", call. = FALSE)
  }
  indexed <- dplyr::left_join(
    water_rights,
    dplyr::select(comid_index, dplyr::all_of(required)),
    by = "record_id"
  )
  indexed$comid <- as.character(indexed$comid)
  indexed$nldi_error <- as.character(indexed$nldi_error)
  indexed <- fs_logic(indexed, sf::st_as_sf(fs_boundary))
  validate_water_rights(indexed)
  indexed
}

#' Write prepped water rights to a reusable GeoPackage cache
#'
#' @param water_rights Output from [prepare_network_water_rights()].
#' @param path Output GeoPackage path.
#' @param layer Layer name to write.
#' @param replace_states Whether to replace existing records for states present
#'   in `water_rights`. This permits an annual update for one state without
#'   re-preparing the other states in the combined cache.
#' @return `path`, invisibly. The layer may contain one or more states.
#' @export
write_prepped_water_rights <- function(water_rights, path,
                                       layer = "water_rights_prepped",
                                       replace_states = TRUE) {
  validate_water_rights(water_rights)
  required <- c("comid", "nldi_error", "fs_intersection")
  missing <- setdiff(required, names(water_rights))
  if (length(missing)) {
    stop("`water_rights` is not prepared. Missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(replace_states) && file.exists(path)) {
    existing <- tryCatch(
      read_prepped_water_rights(path, layer = layer),
      error = function(error) NULL
    )
    if (!is.null(existing)) {
      existing <- existing[!existing$state %in% unique(water_rights$state), , drop = FALSE]
      existing_geometry <- attr(existing, "sf_column")
      incoming_geometry <- attr(water_rights, "sf_column")
      if (!identical(existing_geometry, incoming_geometry)) {
        names(water_rights)[names(water_rights) == incoming_geometry] <- existing_geometry
        sf::st_geometry(water_rights) <- existing_geometry
      }
      # State adapters may retain different audit fields (e.g., Idaho report
      # conditions versus Montana POD-selection fields). Preserve their union
      # in the combined cache, with typed missing values for the other state.
      all_fields <- union(names(existing), names(water_rights))
      typed_na <- function(template, n) {
        if (is.factor(template)) {
          return(factor(rep(NA_character_, n), levels = levels(template)))
        }
        rep(template[NA_integer_], n)
      }
      for (field in setdiff(all_fields, names(existing))) {
        existing[[field]] <- typed_na(water_rights[[field]], nrow(existing))
      }
      for (field in setdiff(all_fields, names(water_rights))) {
        water_rights[[field]] <- typed_na(existing[[field]], nrow(water_rights))
      }
      existing <- existing[, all_fields, drop = FALSE]
      water_rights <- water_rights[, all_fields, drop = FALSE]
      water_rights <- rbind(existing, water_rights)
    }
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::write_sf(water_rights, path, layer = layer, delete_dsn = file.exists(path), quiet = TRUE)
  invisible(path)
}

#' Prepare and cache one or more states for network analysis
#'
#' This is the state-level handoff for annual updates. It enriches canonical
#' records with COMID and FS ownership fields, then replaces only the supplied
#' state rows in a combined prepped GeoPackage. The network targets workflow
#' reads this one cache as a single multi-state simple feature layer.
#'
#' @inheritParams prepare_network_water_rights
#' @param cache_path Combined prepped GeoPackage path.
#' @param layer Cache layer name.
#' @return `cache_path`, invisibly.
#' @export
cache_network_water_rights <- function(water_rights, comid_index, fs_boundary,
                                       cache_path = file.path(
                                         "data", "cache", "prepped", "water_rights.gpkg"
                                       ), layer = "water_rights_prepped") {
  prepped <- prepare_network_water_rights(water_rights, comid_index, fs_boundary)
  write_prepped_water_rights(prepped, cache_path, layer = layer, replace_states = TRUE)
  invisible(cache_path)
}

#' Refresh one state's prepared network input
#'
#' Compare a refreshed standardized state layer with its prior rows in the
#' combined cache, reuse location-matched COMID lookups, and replace that
#' state's cache rows. Missing prior records are reported as retired; changed
#' geometry or allocation fields are reported as changed.
#'
#' @param water_rights Refreshed canonical records for one state.
#' @param fs_boundary Forest Service ownership geometry for that state.
#' @param nldi_cache_path State-specific RDS checkpoint for COMID lookups.
#' @param cache_path Combined prepped GeoPackage path.
#' @param layer Cache layer name.
#' @param comid_index Optional precomputed `record_id`, `comid`, and
#'   `nldi_error` index. Supply this when a state adapter has already indexed
#'   POD candidates to make a state-specific selection.
#' @param ... Arguments passed to [index_water_right_comids()].
#' @return A list with `cache_path`, `changes`, and `prepped_water_rights`.
#' @export
refresh_network_water_rights <- function(water_rights, fs_boundary,
                                         nldi_cache_path,
                                         cache_path = file.path(
                                           "data", "cache", "prepped", "water_rights.gpkg"
                                         ), layer = "water_rights_prepped",
                                         comid_index = NULL, ...) {
  validate_water_rights(water_rights)
  states <- unique(water_rights$state)
  if (length(states) != 1L) stop("Refresh one state at a time.", call. = FALSE)
  empty_previous <- function() {
    result <- water_rights[0, , drop = FALSE]
    result$comid <- character()
    result$nldi_error <- character()
    result$fs_intersection <- logical()
    result
  }
  previous <- if (file.exists(cache_path)) {
    cached <- tryCatch(read_prepped_water_rights(cache_path, layer), error = function(error) NULL)
    if (is.null(cached)) empty_previous() else cached[cached$state == states, , drop = FALSE]
  } else {
    empty_previous()
  }
  if (is.null(comid_index)) {
    comid_index <- index_water_right_comids(water_rights, cache_path = nldi_cache_path, ...)
  }
  index_fields <- c("record_id", "comid", "nldi_error")
  if (length(setdiff(index_fields, names(comid_index))) || anyDuplicated(comid_index$record_id)) {
    stop("`comid_index` must have unique record_id, comid, and nldi_error fields.", call. = FALSE)
  }
  prepped <- prepare_network_water_rights(water_rights, comid_index, fs_boundary)
  signature <- function(x) {
    if (!nrow(x)) return(stats::setNames(character(), character()))
    geometry <- sf::st_as_text(sf::st_geometry(sf::st_transform(x, 4326)))
    fields <- setdiff(names(x), attr(x, "sf_column"))
    values <- lapply(x[fields], function(column) {
      value <- as.character(column)
      value[is.na(value)] <- "<NA>"
      value
    })
    stats::setNames(
      do.call(paste, c(values, list(geometry, sep = "|"))),
      x$record_id
    )
  }
  old <- signature(previous)
  new <- signature(prepped)
  ids <- union(names(old), names(new))
  changes <- data.frame(
    record_id = ids,
    change_type = ifelse(
      !ids %in% names(old), "added",
      ifelse(!ids %in% names(new), "retired", ifelse(old[ids] != new[ids], "changed", NA_character_))
    ),
    old_comid = unname(previous$comid[match(ids, previous$record_id)]),
    new_comid = unname(prepped$comid[match(ids, prepped$record_id)]),
    stringsAsFactors = FALSE
  )
  changes <- changes[!is.na(changes$change_type), , drop = FALSE]
  write_prepped_water_rights(prepped, cache_path, layer = layer, replace_states = TRUE)
  list(cache_path = cache_path, changes = changes, prepped_water_rights = prepped)
}

#' Read a prepped water-right GeoPackage cache
#'
#' @param path GeoPackage created by [write_prepped_water_rights()].
#' @param layer Layer name to read.
#' @return Prepared canonical water-right records.
#' @export
read_prepped_water_rights <- function(path, layer = "water_rights_prepped") {
  if (!file.exists(path)) stop("Prepped water-right cache does not exist: ", path, call. = FALSE)
  water_rights <- sf::read_sf(path, layer = layer, quiet = TRUE)
  validate_water_rights(water_rights)
  required <- c("comid", "nldi_error", "fs_intersection")
  missing <- setdiff(required, names(water_rights))
  if (length(missing)) {
    stop("`path` is not a prepared water-right cache. Missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  water_rights
}
