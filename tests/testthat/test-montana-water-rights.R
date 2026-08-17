test_that("Montana POD filtering uses sf and the analysis eligibility rules", {
  source_pods <- sf::st_as_sf(
    data.frame(
      WR_STATUS = c("ACTIVE", "ACTIVE", "ACTIVE", "RETIRED", "ACTIVE"),
      MAX_FLOW_CFS = c(1, NA, 3, 4, 5),
      SOURCE_TYPE = c("SURFACE", "SURFACE", "GROUNDWATER", "SURFACE", "SURFACE"),
      x = c(0, 1, 2, 3, 20), y = 0
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
  expect_equal(nrow(all_records), 4L)
})
