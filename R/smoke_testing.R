#' Prepare a small Montana and Idaho targets smoke test
#'
#' Select a deterministic handful of eligible PODs from Montana and Idaho, make
#' compact buffered analysis boundaries around them, and create a small Idaho
#' canonical cache. This permits an end-to-end targets run without a state-wide
#' Idaho scrape or state-wide spatial processing.
#'
#' @param mt_path Path to Montana's `WRQS_PODS` geodatabase.
#' @param id_path Path to Idaho's `PODRight` geodatabase.
#' @param states_path Path to the state boundary GeoPackage.
#' @param output_states_path Output GeoPackage for compact smoke boundaries.
#' @param id_cache_path Output GeoPackage for the compact Idaho canonical cache.
#' @param sites_per_state Number of eligible PODs to select per state.
#' @param buffer_m Buffer distance in metres around each selected POD.
#' @param retries Number of retry attempts for each selected Idaho report.
#' @param candidate_scan Number of source records to scan when selecting each
#'   state's deterministic sample. This prevents smoke-test setup from reading
#'   an entire state geodatabase merely to choose a few sites.
#' @return A list of generated paths and selected site identifiers.
#' @export
prepare_smoke_test <- function(
    mt_path = "data/WRQS_Dataset_GDB.gdb",
    id_path = "data/_ags_data25175638F3334C56901BED08795AB38A.gdb",
    states_path = "data/states.gpkg",
    output_states_path = "data/smoke_states.gpkg",
    id_cache_path = "data/idaho_water_rights_smoke.gpkg",
    sites_per_state = 3L,
    buffer_m = 10000,
    retries = 2L,
    candidate_scan = 5000L) {
  states <- sf::read_sf(states_path, layer = "states", quiet = TRUE)
  state_boundary <- function(state) {
    states[states$state_abbr == state, , drop = FALSE]
  }
  sample_rows <- function(x, n) x[seq_len(min(nrow(x), as.integer(n))), , drop = FALSE]
  make_boundary <- function(points, state) {
    boundary <- sf::st_as_sf(sf::st_union(sf::st_buffer(points, dist = buffer_m))) |>
      sf::st_transform(4326)
    boundary$state_abbr <- state
    boundary
  }

  mt_candidates <- sf::read_sf(
    mt_path,
    query = paste0("SELECT * FROM WRQS_PODS LIMIT ", as.integer(candidate_scan)),
    quiet = TRUE
  )
  mt_pods <- mt_candidates[
    toupper(trimws(mt_candidates$WR_STATUS)) == "ACTIVE" &
      !is.na(mt_candidates$MAX_FLOW_CFS) &
      toupper(trimws(mt_candidates$SOURCE_TYPE)) == "SURFACE",
    , drop = FALSE
  ]
  mt_pods <- sample_rows(mt_pods, sites_per_state)
  if (!nrow(mt_pods)) stop("No eligible Montana PODs were found for the smoke test.", call. = FALSE)

  id_candidates <- sf::read_sf(
    id_path,
    query = paste0("SELECT * FROM PODRight LIMIT ", as.integer(candidate_scan)),
    quiet = TRUE
  )
  id_pods <- id_candidates[
    toupper(trimws(id_candidates$Status)) == "ACTIVE" &
      toupper(trimws(id_candidates$Source)) != "GROUND WATER" &
      !toupper(trimws(id_candidates$DiversionType)) %in% .idwr_excluded_diversion_types,
    , drop = FALSE
  ]
  id_pods <- sample_rows(id_pods, sites_per_state)
  if (!nrow(id_pods)) stop("No eligible Idaho PODs were found for the smoke test.", call. = FALSE)

  smoke_boundaries <- dplyr::bind_rows(
    make_boundary(mt_pods, "MT"),
    make_boundary(id_pods, "ID")
  )
  dir.create(dirname(output_states_path), recursive = TRUE, showWarnings = FALSE)
  sf::write_sf(
    smoke_boundaries, output_states_path, layer = "states",
    delete_dsn = file.exists(output_states_path), quiet = TRUE
  )

  id_water_rights <- id_pods |>
    expand_idwr_pods_by_use(retries = retries) |>
    standardize_idwr_water_rights() |>
    filter_water_rights_month(month = 8L)
  dir.create(dirname(id_cache_path), recursive = TRUE, showWarnings = FALSE)
  sf::write_sf(
    id_water_rights, id_cache_path, layer = "water_rights",
    delete_dsn = file.exists(id_cache_path), quiet = TRUE
  )

  list(
    states_path = output_states_path,
    id_cache_path = id_cache_path,
    mt_site_ids = mt_pods$PODV_ID_SEQ,
    id_site_ids = id_pods$PointOfDiversionID
  )
}
