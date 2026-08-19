# Network analysis begins at the durable, multi-state preparation cache.
# Build or refresh that cache with `cache_network_water_rights()` before running
# this pipeline. State-specific filtering, scraping, COMID indexing, and FS
# intersection are intentionally outside this graph.

library(targets)
library(crew)

run_mode <- Sys.getenv("WATERRAT_RUN_MODE", unset = "full")
if (!run_mode %in% c("full", "smoke")) {
  stop("WATERRAT_RUN_MODE must be `full` or `smoke`.", call. = FALSE)
}

prepped_water_rights_path <- Sys.getenv(
  "WATERRAT_PREPPED_WATER_RIGHTS",
  unset = file.path("data", "cache", "prepped", "water_rights.gpkg")
)
states_path <- Sys.getenv("WATERRAT_STATES_PATH", unset = file.path("data", "states.gpkg"))
analysis_aoi_path <- Sys.getenv("WATERRAT_ANALYSIS_AOI_PATH", unset = states_path)
analysis_aoi_layer <- Sys.getenv("WATERRAT_ANALYSIS_AOI_LAYER", unset = "states")
flowmet_path <- Sys.getenv("WATERRAT_FLOWMET_PATH", unset = file.path("data", "flowmet.gpkg"))
nhdplus_gdb_path <- Sys.getenv("WATERRAT_NHDPLUS_GDB", unset = "")
nhdplus_layer <- Sys.getenv("WATERRAT_NHDPLUS_LAYER", unset = "NHDFlowline_Network")
result_path <- Sys.getenv(
  "WATERRAT_NETWORK_RESULT",
  unset = file.path("data", "cache", "results", paste0("network_all_states_", run_mode, ".gpkg"))
)

compute_workers <- if (run_mode == "smoke") 2L else 88L
controller_compute <- crew::crew_controller_local(name = "compute", workers = compute_workers)

tar_option_set(
  packages = c("WaterRAT", "dplyr", "sf"),
  controller = controller_compute,
  resources = tar_resources(crew = tar_resources_crew(controller = "compute"))
)

list(
  tar_target(prepped_water_rights_source, prepped_water_rights_path, format = "file"),
  tar_target(states_source, states_path, format = "file"),
  tar_target(analysis_aoi_source, analysis_aoi_path, format = "file"),
  tar_target(flowmet_source, flowmet_path, format = "file"),
  if (nzchar(nhdplus_gdb_path)) {
    tar_target(nhdplus_source, nhdplus_gdb_path, format = "file")
  } else {
    tar_target(nhdplus_source, nhdplus_gdb_path)
  },

  # This is the single multi-state simple-feature input to network routing.
  tar_target(
    prepped_water_rights,
    read_prepped_water_rights(prepped_water_rights_source)
  ),
  tar_target(
    analysis_states,
    sf::read_sf(states_source, layer = "states", quiet = TRUE) |>
      dplyr::filter(.data$state_abbr %in% unique(prepped_water_rights$state))
  ),
  tar_target(
    analysis_aoi,
    if (identical(analysis_aoi_path, states_path) && identical(analysis_aoi_layer, "states")) {
      sf::st_as_sf(sf::st_union(analysis_states))
    } else {
      sf::st_as_sf(sf::st_union(sf::st_intersection(
        analysis_states,
        sf::read_sf(analysis_aoi_source, layer = analysis_aoi_layer, quiet = TRUE)
      )))
    }
  ),

  tar_target(
    nhdplus_network,
    if (nzchar(nhdplus_gdb_path)) {
      read_nhdplus_flowlines(
        path = nhdplus_source,
        filter_geom = analysis_aoi,
        layer = nhdplus_layer,
        clip = FALSE
      )
    } else {
      nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(analysis_aoi)))
    }
  ),
  tar_target(
    flowmet_aoi,
    get_flowmet(
      filter_geom = analysis_aoi,
      layer = "mean_summer_flow_historical_hires",
      local_path = flowmet_source
    ) |>
      sf::st_zm() |>
      sf::st_cast("LINESTRING") |>
      sf::st_set_crs(4326) |>
      dplyr::select(dplyr::all_of(c("maug_hist", "comid"))) |>
      dplyr::mutate(comid = as.character(.data$comid)) |>
      sf::st_transform(sf::st_crs(nhdplus_network))
  ),
  tar_target(
    flowmet_network,
    {
      result <- dplyr::left_join(
        flowmet_aoi,
        dplyr::select(
          dplyr::mutate(sf::st_drop_geometry(nhdplus_network), comid = as.character(.data$comid)),
          dplyr::any_of(c("comid", "qe_08", "streamorde"))
        ),
        by = "comid"
      )
      if (!"qe_08" %in% names(result)) result$qe_08 <- NA_real_
      result
    }
  ),

  tar_target(
    local_allocations,
    aggregate_local_allocations(
      prepped_water_rights,
      dplyr::select(prepped_water_rights, dplyr::all_of(c("record_id", "comid")))
    )
  ),
  tar_target(
    accumulated_network,
    accumulate_downstream_allocations(nhdplus_network, local_allocations)
  ),
  tar_target(
    reportable_network,
    filter_reportable_reaches(accumulated_network, minimum_streamorder = 2L)
  ),
  tar_target(
    network_allocation,
    format_capture_sites_output(
      accumulated_network = reportable_network,
      flowmet = dplyr::filter(flowmet_network, .data$streamorde >= 2L)
    )
  ),
  tar_target(
    network_allocation_gpkg,
    {
      dir.create(dirname(result_path), recursive = TRUE, showWarnings = FALSE)
      sf::write_sf(network_allocation, result_path, layer = "network_allocation",
                   delete_dsn = file.exists(result_path), quiet = TRUE)
      result_path
    },
    format = "file"
  )
)
