#' Get filtered Montana points of diversion
#'
#' Read a local Montana `WRQS_PODS` layer and apply the analysis eligibility
#' filters. The AOI is passed to GDAL as a spatial filter before records are
#' materialized, then checked with `sf` for exact intersection.
#'
#' @param filter_geom An `sf` geometry used to retain intersecting PODs.
#' @param layer Layer name within the Montana water-right geodatabase.
#' @param local_path Path to the Montana water-right geodatabase.
#' @param active_only Whether to restrict the returned data to rights currently
#'   eligible for analysis: `WR_STATUS = "ACTIVE"`, non-missing
#'   `MAX_FLOW_CFS`, `SOURCE_TYPE = "SURFACE"`, and a purpose other than
#'   `POWER GENERATION, NONCONSUMPTIVE`. Set to `FALSE` when creating an update
#'   snapshot so retired and otherwise ineligible rights are retained for
#'   comparison.
#' @return An `sf` object of filtered Montana PODs.
#' @export
get_mtwr <- function(filter_geom, layer, local_path, active_only = TRUE) {
  if (!file.exists(local_path)) {
    stop("`local_path` must be an existing local vector datasource.", call. = FALSE)
  }
  layer_schema <- sf::read_sf(
    local_path,
    query = paste0("SELECT * FROM ", layer, " LIMIT 0"),
    quiet = TRUE
  )
  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(layer_schema))
  pods <- sf::read_sf(
    local_path, layer = layer,
    wkt_filter = sf::st_as_text(sf::st_union(filter_geom)),
    quiet = TRUE
  )
  required <- c("WR_STATUS", "MAX_FLOW_CFS", "SOURCE_TYPE", "PURPOSES")
  missing <- setdiff(required, names(pods))
  if (length(missing)) {
    stop(
      "`", layer, "` is not a Montana POD layer with required fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }

  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(pods))
  pods <- pods[lengths(sf::st_intersects(pods, filter_geom)) > 0L, , drop = FALSE]
  if (active_only) {
    pods <- pods[
      toupper(trimws(pods$WR_STATUS)) == "ACTIVE" &
        !is.na(pods$MAX_FLOW_CFS) &
        toupper(trimws(pods$SOURCE_TYPE)) == "SURFACE" &
        toupper(trimws(pods$PURPOSES)) != "POWER GENERATION, NONCONSUMPTIVE",
      , drop = FALSE
    ]
  }
  pods
}

#' Refresh prepared Montana water rights
#'
#' Apply the Montana eligibility and month filters, index each physical POD to
#' select the downstream-most location for repeated `WRKEY` values, then
#' refresh Montana's rows in the combined prepared cache. Candidate COMIDs are
#' assigned from local NHDPlus catchments; unresolved locations use checkpointed
#' NLDI fallback lookups.
#'
#' @param local_path Path to the Montana water-right geodatabase.
#' @param filter_geom Montana analysis boundary.
#' @param fs_boundary Forest Service ownership geometry for Montana.
#' @param network NHDPlus flowlines with `comid` and `hydroseq`.
#' @param nldi_cache_path Candidate-POD NLDI checkpoint path.
#' @param catchments Optional local NHDPlus catchments used for primary COMID
#'   assignment; NLDI is used only for unmatched PODs.
#' @param cache_path Combined prepped water-right GeoPackage path.
#' @param month Analysis month.
#' @param layer Montana source layer name.
#' @param max_pods Optional maximum number of filtered POD locations to process.
#'   Intended for small smoke tests; leave `NULL` for a complete refresh.
#' @param ... Arguments passed to [index_water_right_comids()].
#' @return A list from [refresh_network_water_rights()] with additional
#'   `candidate_pod_count`.
#' @export
refresh_mt_network_water_rights <- function(
    local_path, filter_geom, fs_boundary, network, nldi_cache_path,
    cache_path = file.path("data", "cache", "prepped", "water_rights.gpkg"),
    month = 8L, layer = "WRQS_PODS", max_pods = NULL, catchments = NULL, ...) {
  pods <- get_mtwr(
    filter_geom = filter_geom, layer = layer, local_path = local_path
  ) |>
    date_cleaning(month = month)
  if (!is.null(max_pods)) {
    max_pods <- as.integer(max_pods)
    if (length(max_pods) != 1L || is.na(max_pods) || max_pods < 1L) {
      stop("`max_pods` must be one positive integer or NULL.", call. = FALSE)
    }
    pods <- pods[seq_len(min(nrow(pods), max_pods)), , drop = FALSE]
  }
  candidates <- standardize_mt_pod_candidates(pods)
  candidate_index <- index_water_right_comids(
    candidates, cache_path = nldi_cache_path, catchments = catchments, ...
  )
  selected <- select_mt_downstream_pods(pods, candidate_index, network)
  water_rights <- standardize_mt_water_rights(selected)
  selected_candidate_id <- paste("MT", selected$PODV_ID_SEQ, sep = ":")
  candidate_row <- match(selected_candidate_id, candidate_index$record_id)
  selected_index <- data.frame(
    record_id = water_rights$record_id,
    comid = as.character(selected$selected_comid),
    nldi_error = as.character(candidate_index$nldi_error[candidate_row]),
    stringsAsFactors = FALSE
  )
  result <- refresh_network_water_rights(
    water_rights, fs_boundary = fs_boundary, nldi_cache_path = nldi_cache_path,
    cache_path = cache_path, comid_index = selected_index
  )
  result$candidate_pod_count <- nrow(candidates)
  result
}
