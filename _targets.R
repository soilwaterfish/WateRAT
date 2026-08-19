# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

library(targets)
library(tarchetypes)
library(crew)

# Set WATERRAT_RUN_MODE=smoke only after `prepare_smoke_test()` creates the
# ignored smoke inputs. Full mode remains the default.
run_mode <- Sys.getenv("WATERRAT_RUN_MODE", unset = "full")
if (!run_mode %in% c("full", "smoke")) {
  stop("WATERRAT_RUN_MODE must be `full` or `smoke`.", call. = FALSE)
}
compute_workers <- if (run_mode == "smoke") 2L else 88L
nldi_workers <- if (run_mode == "smoke") 2L else 8L
# Set this to a routing-capable local NHDPlus geodatabase to avoid downloading
# NHDPlus for each state. The selected layer is spatially filtered at the GDAL
# source before it is read. Leave unset to retain the NLDI download behavior.
nhdplus_gdb_path <- Sys.getenv("WATERRAT_NHDPLUS_GDB", unset = "")
nhdplus_layer <- Sys.getenv("WATERRAT_NHDPLUS_LAYER", unset = "NHDFlowline_Network")

controller_compute <- crew::crew_controller_local(
  name = "compute",
  workers = compute_workers
)

controller_nldi <- crew::crew_controller_local(
  name = "nldi",
  workers = nldi_workers
)

controller <- crew::crew_controller_group(
  controller_compute,
  controller_nldi
)

tar_option_set(
  packages = c(
    "WaterRAT",
    "nhdplusTools",
    "dplyr",
    "purrr",
    "sf"
  ),
  controller = controller,
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "compute"
    )
  )
)

# One row per state. Add states here only after a state adapter returns the
# canonical schema from `water_right_schema()`.
state_values <- if (run_mode == "smoke") {
  dplyr::tibble(
    state_code = c("MT", "ID"),
    water_right_path = c(
      "data/WRQS_Dataset_GDB.gdb",
      "data/_ags_data25175638F3334C56901BED08795AB38A.gdb"
    ),
    canonical_cache_path = c(NA_character_, "data/idaho_water_rights_smoke.gpkg"),
    boundary_path = "data/smoke_states.gpkg"
  )
} else {
  dplyr::tibble(
    state_code = c("MT", "ID"),
    water_right_path = c(
      "data/WRQS_Dataset_GDB.gdb",
      "data/_ags_data25175638F3334C56901BED08795AB38A.gdb"
    ),
    canonical_cache_path = c(NA_character_, "data/idaho_water_rights.gpkg"),
    boundary_path = "data/states.gpkg"
  )
}

targets <- tar_map(

values = state_values,

tar_target(
  state_boundary,
    sf::read_sf(boundary_path, layer = "states") %>%
    dplyr::filter(.data$state_abbr == state_code)
),

tar_target(state_crs, sf::st_crs(state_boundary)),

tar_target(admin_int, suppressMessages(sf::read_sf(file.path("data", "admin.shp")) %>%
                                         sf::st_set_crs(4326) %>%
                                         sf::st_transform(crs = state_crs) %>%
                                         sf::st_make_valid() %>%
                                         dplyr::filter(toupper(ownerclass) == "USDA FOREST SERVICE") %>%
                                         sf::st_intersection(state_boundary) %>%
                                         sf::st_union() %>%
                                         sf::st_as_sf())),

tar_target(flowmet_intersect, get_flowmet(filter_geom = state_boundary,
                                          layer = 'mean_summer_flow_historical_hires',
                                          local_path = file.path("data", "flowmet.gpkg"))  %>%
             sf::st_zm() %>%
             sf::st_cast('LINESTRING') %>%
             sf::st_set_crs(4326) %>%
             dplyr::select(c("maug_hist", "comid")) %>%
                                      sf::st_transform(crs = state_crs) %>%
                                      sf::st_intersection(state_boundary)),

tar_target(
  nhdplus,
  if (nzchar(nhdplus_gdb_path)) {
    read_nhdplus_flowlines(
      path = nhdplus_gdb_path,
      filter_geom = state_boundary,
      layer = nhdplus_layer
    )
  } else {
    nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(flowmet_intersect)))
  }
),

tar_target(flowmet_join_nhdplus, flowmet_intersect %>% dplyr::select(maug_hist, comid) %>%
    dplyr::left_join(nhdplus %>%
                       sf::st_drop_geometry() %>%
                       dplyr::mutate(comid = as.character(comid)), by = c('comid' = 'comid'))
),

# Montana's WRKEY can contain several physical PODs. Index only the repeated
# groups, then retain the downstream-most one before assigning WRKEY as the
# canonical site identifier.
tar_target(
  mt_pods,
  if (state_code == "MT") {
    get_mtwr(
      filter_geom = state_boundary,
      layer = "WRQS_PODS",
      local_path = water_right_path
    ) |>
      date_cleaning()
  } else {
    NULL
  }
),

tar_target(
  mt_pod_candidates,
  if (state_code == "MT") {
    repeated <- duplicated(mt_pods$WRKEY) | duplicated(mt_pods$WRKEY, fromLast = TRUE)
    standardize_mt_pod_candidates(mt_pods[repeated, , drop = FALSE])
  } else {
    NULL
  }
),

tar_target(
  mt_pod_comid_index,
  if (state_code == "MT") {
    index_water_right_comids(
      mt_pod_candidates,
      cache_path = file.path("data", paste0("mt_pod_comids_", run_mode, ".rds")),
      workers = 1L,
      throttle_seconds = 0.5,
      retries = 3L
    )
  } else {
    NULL
  },
  resources = tar_resources(
    crew = tar_resources_crew(controller = "nldi")
  )
),

tar_target(
  state_water_rights,
  if (state_code == "ID") {
    if (!file.exists(canonical_cache_path)) {
      stop(
        "Idaho canonical cache is missing: ", canonical_cache_path,
        ". Build it explicitly with `cache_idwr_water_rights()` before tar_make().",
        call. = FALSE
      )
    }
    cached_water_rights <- sf::read_sf(
      canonical_cache_path, layer = "water_rights", quiet = TRUE
    )
    validate_water_rights(cached_water_rights)
    cached_water_rights
  } else {
    select_mt_downstream_pods(mt_pods, mt_pod_comid_index, nhdplus) |>
      standardize_mt_water_rights()
  }
),

tar_target(flowmet_grt_strahler_1_order, flowmet_join_nhdplus %>% dplyr::filter(streamorde > 1)),

tar_target(crs, sf::st_crs(state_water_rights)),

tar_target(
  comids,
  unique(
    as.character(
      flowmet_grt_strahler_1_order$comid
    )
  )
),

tar_target(
  pod_basin,
  get_pod_basin(
    comids,
    crs
  ),
  pattern = map(comids),
  iteration = "list",
  error = "continue",
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "nldi"
    )
  )
),

tar_target(
  basins,
  pod_basin %>%
    purrr::compact() %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf()
),

tar_target(state_water_rights_fs_intersection, fs_logic(state_water_rights, admin_int)),

tar_target(
  basin_comids,
  split(
    basins,
    basins$comid
  ),
  iteration = "list"
),

tar_target(
  captured_sites,
  capture_sites_within(
    basin_comids,
    state_water_rights_fs_intersection
  ),
  pattern = map(basin_comids),
  iteration = "list"
),

tar_target(
  adding_intersecting_flows,
  captured_sites %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf()
),
tar_target(state_water_rights_final_joined, adding_intersecting_flows %>%
             sf::st_drop_geometry() %>%
             dplyr::left_join(flowmet_grt_strahler_1_order %>% dplyr::select(comid,maug_hist, gnis_name, qe_08)) %>%
             sf::st_as_sf() %>%
             dplyr::mutate(
               intersecting_flow_all_together_percent = (intersecting_flow_all_together/maug_hist)*100,
               intersecting_flow_fs_percent = (intersecting_flow_fs/maug_hist)*100,
               intersecting_flow_private_percent = (intersecting_flow_private/maug_hist)*100
             ))
)


list(targets)
































