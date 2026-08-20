# State-specific ingestion and preparation pipeline. Run it independently from
# network analysis with:
# targets::tar_make(script = "prep_targets.R", store = "_targets_prep")
#
# Every state adapter follows the same handoff:
# source data -> state filtering/standardization -> COMID + FS ownership ->
# data/cache/prepped/water_rights.gpkg. `_targets.R` only routes that combined
# prepared layer. Add future states here as another state adapter target that
# calls `refresh_network_water_rights()` (or a state-specific wrapper) and
# depends on the preceding cache target to avoid concurrent GeoPackage writes.

library(targets)

clip_forest_service_boundary <- function(forest_service, boundary) {
  boundary_crs <- sf::st_crs(boundary)
  forest_service <- sf::st_transform(sf::st_make_valid(forest_service), 5070)
  boundary <- sf::st_transform(sf::st_make_valid(boundary), 5070)
  clipped <- sf::st_as_sf(
    sf::st_transform(
      sf::st_union(sf::st_intersection(forest_service, boundary)),
      boundary_crs
    )
  )
  # A planar-valid polygon can become invalid for s2 after transformation back
  # to geographic coordinates. Repair it before point-in-polygon FS checks.
  clipped <- sf::st_make_valid(clipped)
  clipped[!sf::st_is_empty(clipped), , drop = FALSE]
}

states_path <- Sys.getenv("WATERRAT_STATES_PATH", unset = file.path("data", "states.gpkg"))
# Point these optional settings at a smaller AOI layer to smoke test preparation
# without reading either full state. The AOI needs only geometry: it is
# intersected with the authoritative `states` layer to retain state_abbr.
prep_aoi_path <- Sys.getenv("WATERRAT_PREP_AOI_PATH", unset = states_path)
prep_aoi_layer <- Sys.getenv("WATERRAT_PREP_AOI_LAYER", unset = "states")
prep_max_pods <- suppressWarnings(as.integer(Sys.getenv("WATERRAT_PREP_MAX_PODS", unset = "")))
if (is.na(prep_max_pods)) prep_max_pods <- NULL
# Temporary migration option for an existing combined cache created before the
# final cache was the sole file-owning target. Set to true once to preserve the
# completed Idaho rows while repairing/rebuilding a downstream state.
reuse_existing_idaho_cache <- tolower(Sys.getenv("WATERRAT_REUSE_EXISTING_ID_CACHE", unset = "false")) %in% c(
  "true", "1", "yes"
)
admin_path <- Sys.getenv("WATERRAT_ADMIN_SHP", unset = file.path("data", "admin.shp"))
prepped_path <- Sys.getenv(
  "WATERRAT_PREPPED_WATER_RIGHTS",
  unset = file.path("data", "cache", "prepped", "water_rights.gpkg")
)

# Idaho inputs and checkpoints.
idwr_path <- Sys.getenv("WATERRAT_IDWR_GDB", unset = file.path("data", "idwr.gdb"))
id_nldi_path <- Sys.getenv("WATERRAT_ID_NLDI_CACHE", unset = file.path("data", "cache", "nldi", "id.rds"))

# Montana inputs and checkpoints. The NHDPlus source is only used here to
# choose the downstream-most POD for repeated WRKEY sites; `_targets.R` reads
# its own AOI-filtered NHDPlus network for final multi-state routing.
mtwr_path <- Sys.getenv("WATERRAT_MTWR_GDB", unset = file.path("data", "WRQS_Dataset_GDB.gdb"))
mt_nldi_path <- Sys.getenv("WATERRAT_MT_NLDI_CACHE", unset = file.path("data", "cache", "nldi", "mt_candidates.rds"))
nhdplus_path <- Sys.getenv(
  "WATERRAT_NHDPLUS_GDB",
  unset = file.path(
    "data", "NHDPlusNationalData",
    "NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
  )
)
nhdplus_layer <- Sys.getenv("WATERRAT_NHDPLUS_LAYER", unset = "NHDFlowline_Network")

has_prepped_state <- function(path, state) {
  if (!file.exists(path)) return(FALSE)
  cached <- tryCatch(read_prepped_water_rights(path), error = function(error) NULL)
  !is.null(cached) && any(cached$state == state)
}

tar_option_set(packages = c("WaterRAT", "dplyr", "sf"))

list(
  tar_target(states_source, states_path, format = "file"),
  tar_target(prep_aoi_source, prep_aoi_path, format = "file"),
  tar_target(
    admin_source,
    c(
      admin_path,
      sub("\\.shp$", ".dbf", admin_path),
      sub("\\.shp$", ".shx", admin_path)
    ),
    format = "file"
  ),
  tar_target(idwr_source, idwr_path, format = "file"),
  tar_target(mtwr_source, mtwr_path, format = "file"),
  tar_target(nhdplus_source, nhdplus_path, format = "file"),
  tar_target(
    supported_states,
    sf::read_sf(states_source, layer = "states", quiet = TRUE) |>
      dplyr::filter(.data$state_abbr %in% c("ID", "MT"))
  ),
  tar_target(
    state_boundaries,
    if (identical(prep_aoi_path, states_path) && identical(prep_aoi_layer, "states")) {
      supported_states
    } else {
      sf::st_intersection(
        supported_states,
        sf::read_sf(prep_aoi_source, layer = prep_aoi_layer, quiet = TRUE)
      )
    }
  ),
  tar_target(idaho_boundary, dplyr::filter(state_boundaries, .data$state_abbr == "ID")),
  tar_target(montana_boundary, dplyr::filter(state_boundaries, .data$state_abbr == "MT")),
  tar_target(
    forest_service_boundary,
    sf::read_sf(admin_source[[1L]], quiet = TRUE) |>
      sf::st_set_crs(4326) |>
      dplyr::filter(toupper(.data$ownerclass) == "USDA FOREST SERVICE")
  ),
  tar_target(
    idaho_fs_boundary,
    clip_forest_service_boundary(forest_service_boundary, idaho_boundary)
  ),
  tar_target(
    montana_fs_boundary,
    clip_forest_service_boundary(forest_service_boundary, montana_boundary)
  ),
  tar_target(
    montana_selection_network,
    read_nhdplus_flowlines(
      path = nhdplus_source, filter_geom = montana_boundary,
      layer = nhdplus_layer, clip = FALSE
    )
  ),
  tar_target(
    idaho_catchments,
    read_nhdplus_catchments(nhdplus_source, idaho_boundary, clip = FALSE)
  ),
  tar_target(
    montana_catchments,
    read_nhdplus_catchments(nhdplus_source, montana_boundary, clip = FALSE)
  ),
  # Idaho writes first. This target deliberately returns a path as an ordinary
  # R object: the final target below is the sole `format = "file"` owner of the
  # shared GeoPackage after Montana has upserted its rows.
  tar_target(
    idaho_prepped_cache,
    if (reuse_existing_idaho_cache && has_prepped_state(prepped_path, "ID")) {
      prepped_path
    } else {
      refresh_idwr_network_water_rights(
        idwr_source, idaho_boundary, idaho_fs_boundary, id_nldi_path,
        catchments = idaho_catchments, cache_path = prepped_path,
        max_pods = prep_max_pods, workers = 10L, throttle_seconds = 0.5
      )$cache_path
    }
  ),
  tar_target(
    prepped_water_rights_cache,
    {
      idaho_prepped_cache
      refresh_mt_network_water_rights(
        mtwr_source, montana_boundary, montana_fs_boundary,
        montana_selection_network, mt_nldi_path,
        catchments = montana_catchments, cache_path = prepped_path,
        max_pods = prep_max_pods, workers = 10L, throttle_seconds = 0.5
      )$cache_path
    },
    format = "file"
  )
)
