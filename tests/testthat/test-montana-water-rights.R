test_that("Montana POD filtering uses sf and the analysis eligibility rules", {
  source_pods <- sf::st_as_sf(
    data.frame(
      WR_STATUS = c("ACTIVE", "ACTIVE", "ACTIVE", "RETIRED", "ACTIVE", "ACTIVE"),
      MAX_FLOW_CFS = c(1, NA, 3, 4, 5, 6),
      SOURCE_TYPE = c("SURFACE", "SURFACE", "GROUNDWATER", "SURFACE", "SURFACE", "SURFACE"),
      PURPOSES = c("IRRIGATION", "IRRIGATION", "IRRIGATION", "IRRIGATION", "IRRIGATION", "POWER GENERATION, NONCONSUMPTIVE"),
      x = c(0, 1, 2, 3, 20, 2.5), y = 0
    ),
    coords = c("x", "y"), crs = 4326
  )
  data_path <- tempfile(fileext = ".gpkg")
  sf::write_sf(source_pods, data_path, layer = "WRQS_PODS", quiet = TRUE)
  boundary <- sf::st_as_sf(
    data.frame(wkt = "POLYGON((-1 -1, 4 -1, 4 1, -1 1, -1 -1))"),
    wkt = "wkt", crs = 4326
  )

  eligible <- get_mtwr(boundary, "WRQS_PODS", data_path)
  all_records <- get_mtwr(boundary, "WRQS_PODS", data_path, active_only = FALSE)

  expect_equal(nrow(eligible), 1L)
  expect_equal(eligible$MAX_FLOW_CFS, 1)
  expect_equal(nrow(all_records), 5L)
})

test_that("Montana selection retains the downstream-most POD for each WRKEY", {
  pods <- sf::st_as_sf(
    data.frame(
      WR_NUMBER = c("76J 1", "76J 1", "76J 2"), WRKEY = c("right-a", "right-a", "right-b"),
      PODV_ID_SEQ = c(11L, 12L, 20L), WR_STATUS = "ACTIVE", SOURCE_NAME = "EXAMPLE CREEK",
      MEANS_OF_DIV = "DIVERSION", MAX_FLOW_CFS = c(2, 2, 1), MAX_FLOW_RT = c(2, 2, 1),
      FLOW_RT_UNIT = "CFS", MAX_VOL = NA_real_, x = c(0, 1, 2), y = 0
    ),
    coords = c("x", "y"), crs = 4326
  )
  candidates <- standardize_mt_pod_candidates(pods)
  index <- data.frame(
    record_id = candidates$record_id, comid = c("100", "200", NA_character_)
  )
  network <- sf::st_as_sf(
    data.frame(comid = c("100", "200"), hydroseq = c(20, 10), x = c(0, 1), y = 0),
    coords = c("x", "y"), crs = 4326
  )

  selected <- select_mt_downstream_pods(pods, index, network)
  standardized <- standardize_mt_water_rights(selected)

  expect_equal(selected$PODV_ID_SEQ, c(12L, 20L))
  expect_equal(selected$pod_selection_method, c("lowest_hydroseq", "single_pod"))
  expect_equal(standardized$right_id, c("76J 1", "76J 2"))
  expect_equal(standardized$site_id, c("right-a", "right-b"))
  expect_equal(standardized$record_id, c("MT:right-a", "MT:right-b"))
  expect_error(standardize_mt_water_rights(pods), "values must be unique")
})
