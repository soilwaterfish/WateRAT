# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

library(targets)
library(tarchetypes)
library(crew)

controller_compute <- crew::crew_controller_local(
  name = "compute",
  workers = 88
)

controller_nldi <- crew::crew_controller_local(
  name = "nldi",
  workers = 8
)

controller <- crew::crew_controller_group(
  controller_compute,
  controller_nldi
)

tar_option_set(
  packages = c(
    "WateRAT",
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

### This `basin_entry` will depend on the user defined watershed boundary.
values = dplyr::tibble(values = c("data/kootenai.shp",
                                  "data/clark_fork.shp",
                                  "data/missouri.shp",
                                  "data/yellowstone.shp",
                                  "data/little_missouri.shp"
                                  ))
targets <- tar_map(

values = values,

tar_target(basin, sf::read_sf(values)),

tar_target(basin_crs, sf::st_crs(basin)),

tar_target(admin_int, suppressMessages(sf::read_sf(file.path("data", "admin.shp")) %>%
                                         sf::st_set_crs(4326) %>%
                                         sf::st_transform(crs = basin_crs) %>%
                                         sf::st_make_valid() %>%
                                         sf::st_intersection(basin) %>%
                                         sf::st_union() %>%
                                         sf::st_as_sf())),

tar_target(flowmet_intersect, get_flowmet(filter_geom = basin,
                                          layer = 'mean_summer_flow_historical_hires',
                                          local_path = file.path("data", "flowmet.gpkg"))  %>%
             sf::read_sf()%>%
             sf::st_zm() %>%
             sf::st_cast('LINESTRING') %>%
             sf::st_set_crs(4326) %>%
             dplyr::select(c("maug_hist", "comid")) %>%
                                      sf::st_transform(crs = basin_crs) %>%
                                      sf::st_intersection(basin)),

tar_target(nhdplus, nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(flowmet_intersect)), streamorder = 2)),

tar_target(flowmet_join_nhdplus, flowmet_intersect %>% dplyr::select(maug_hist, comid) %>%
    dplyr::left_join(nhdplus %>%
                       sf::st_drop_geometry() %>%
                       dplyr::mutate(comid = as.character(comid)), by = c('comid' = 'comid'))
),

# tar_target(pou_pod_together, get_mtwr(basin, layer = 'WR1POU', local_path = file.path("data", "WRQS_Dataset_GDB.gdb")) %>%
#              sf::read_sf() %>%
#              dplyr::group_by(WRKEY) %>%
#              dplyr::slice(1) %>%
#              dplyr::ungroup()),

tar_target(pou_pod_together, get_mtwr(basin, layer = 'WRQS_PODS', local_path = file.path("data", "WRQS_Dataset_GDB.gdb")) %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup()),

# tar_target(pou_pod_together, suppressMessages(pod %>%
#                                                   dplyr::left_join(pou %>%
#                                                                      sf::st_drop_geometry() %>%
#                                                                      dplyr::select(c("WRKEY", "PURPOSE", "IRRTYPE", "MAXACRES", "FLWRTGPM", "FLWRTCFS", "VOL", "ACREAGE"))))
# ),

tar_target(pou_pod_together_sf, date_cleaning(pou_pod_together)),

tar_target(flowmet_grt_strahler_1_order, flowmet_join_nhdplus %>% dplyr::filter(streamorde > 1)),

tar_target(crs, sf::st_crs(pou_pod_together_sf)),

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

tar_target(pou_pod_together_fs_intersection, fs_logic(pou_pod_together_sf, admin_int)),

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
    pou_pod_together_fs_intersection
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
tar_target(pou_pod_together_sf_final_joined, adding_intersecting_flows %>%
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















































