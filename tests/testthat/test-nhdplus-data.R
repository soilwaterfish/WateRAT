test_that("local NHDPlus layers are source-filtered and clipped to an AOI", {
  path <- tempfile(fileext = ".gpkg")
  flowlines <- sf::st_as_sf(
    data.frame(
      COMID = c(1, 2), HydroSeq = c(2, 1), DnHydroSeq = c(1, 0), StreamOrde = c(1, 2),
      wkt = c("LINESTRING (0 0, 2 0)", "LINESTRING (10 0, 12 0)")
    ),
    wkt = "wkt", crs = 4326
  )
  sf::write_sf(flowlines, path, layer = "NHDFlowline_Network", quiet = TRUE)
  aoi <- sf::st_as_sfc("POLYGON ((0.5 -1, 1.5 -1, 1.5 1, 0.5 1, 0.5 -1))", crs = 4326)

  result <- read_nhdplus_flowlines(path, aoi)

  expect_equal(nrow(result), 1)
  expect_named(result, c("comid", "hydroseq", "dnhydroseq", "streamorde", "geom"))
  expect_equal(as.numeric(sf::st_bbox(result)[c("xmin", "xmax")]), c(0.5, 1.5))
})

test_that("routing reader explains when a local layer lacks routing fields", {
  path <- tempfile(fileext = ".gpkg")
  catchments <- sf::st_as_sf(
    data.frame(FEATUREID = 1, wkt = "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"),
    wkt = "wkt", crs = 4326
  )
  sf::write_sf(catchments, path, layer = "Catchment", quiet = TRUE)
  aoi <- sf::st_as_sfc("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", crs = 4326)

  expect_error(
    read_nhdplus_flowlines(path, aoi, layer = "Catchment"),
    "missing required fields"
  )
})

test_that("local catchment reader standardizes FEATUREID as a COMID", {
  path <- tempfile(fileext = ".gpkg")
  catchments <- sf::st_as_sf(
    data.frame(
      FEATUREID = c(101L, 202L),
      wkt = c(
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((2 0, 3 0, 3 1, 2 1, 2 0))"
      )
    ), wkt = "wkt", crs = 4326
  )
  sf::write_sf(catchments, path, layer = "Catchment", quiet = TRUE)
  aoi <- sf::st_as_sfc("POLYGON ((-0.1 -0.1, 1.1 -0.1, 1.1 1.1, -0.1 1.1, -0.1 -0.1))", crs = 4326)

  result <- read_nhdplus_catchments(path, aoi)

  expect_equal(result$featureid, "101")
  expect_true(is.character(result$featureid))
})

test_that("local catchment reader repairs invalid source polygons", {
  path <- tempfile(fileext = ".gpkg")
  catchment <- sf::st_as_sf(
    data.frame(
      FEATUREID = 101L,
      wkt = "POLYGON ((0 0, 1 0, 1 1, 1 1, 0 1, 0 0))"
    ), wkt = "wkt", crs = 4326
  )
  sf::write_sf(catchment, path, layer = "Catchment", quiet = TRUE)
  aoi <- sf::st_as_sfc("POLYGON ((-1 -1, 2 -1, 2 2, -1 2, -1 -1))", crs = 4326)

  result <- read_nhdplus_catchments(path, aoi)

  expect_true(all(sf::st_is_valid(result)))
})

test_that("the shipped basin fixture exercises the local NHDPlus reader", {
  fixture <- testthat::test_path("..", "..", "inst", "extdata", "test_basin_nhdplus.gpkg")
  basin <- sf::read_sf(fixture, layer = "basin_boundary", quiet = TRUE)

  result <- read_nhdplus_flowlines(fixture, basin)

  expect_equal(nrow(result), 66)
  expect_true(all(c("comid", "hydroseq", "dnhydroseq", "streamorde", "qe_08") %in% names(result)))
  expect_true(all(sf::st_is_valid(result)))
})

test_that("the two-state basin fixture supports a small-scale walkthrough", {
  fixture <- testthat::test_path("..", "..", "inst", "extdata", "pod_example_basins.gpkg")
  basins <- sf::read_sf(fixture, layer = "pod_example_basins", quiet = TRUE)

  network <- read_nhdplus_flowlines(fixture, basins)

  expect_equal(basins$state_code, c("MT", "ID"))
  expect_equal(basins$example_basin, c("montana", "idaho"))
  expect_equal(nrow(network), 1551)
  expect_true(all(c("comid", "hydroseq", "dnhydroseq", "streamorde", "qe_08") %in% names(network)))
})
