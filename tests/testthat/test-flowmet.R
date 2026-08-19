test_that("FlowMet retrieval uses a local sf datasource and AOI", {
  flowmet <- sf::st_as_sf(
    data.frame(
      comid = c("inside", "outside"), maug_hist = c(10, 20),
      wkt = c("LINESTRING (0 0, 2 0)", "LINESTRING (10 0, 12 0)")
    ),
    wkt = "wkt", crs = 4326
  )
  path <- tempfile(fileext = ".gpkg")
  sf::write_sf(flowmet, path, layer = "flowmet", quiet = TRUE)
  aoi <- sf::st_as_sfc("POLYGON ((0.5 -1, 1.5 -1, 1.5 1, 0.5 1, 0.5 -1))", crs = 4326)

  result <- get_flowmet(filter_geom = aoi, layer = "flowmet", local_path = path)

  expect_s3_class(result, "sf")
  expect_equal(result$comid, "inside")
})
