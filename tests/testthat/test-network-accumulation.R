test_that("network accumulation adds tributary allocations at confluences", {
  network <- sf::st_as_sf(
    data.frame(
      comid = c("A", "B", "C", "D"),
      hydroseq = c(4, 3, 2, 1), dnhydroseq = c(2, 2, 1, NA_real_),
      wkt = c(
        "LINESTRING (0 1, 1 0)", "LINESTRING (0 -1, 1 0)",
        "LINESTRING (1 0, 2 0)", "LINESTRING (2 0, 3 0)"
      )
    ), wkt = "wkt", crs = 4326
  )
  local <- data.frame(
    comid = c("A", "B", "C"),
    local_record_count = c(1, 1, 1), local_flow_cfs = c(2, 3, 1),
    local_instream_cfs = c(0, 3, 0), local_non_instream_cfs = c(2, 0, 1)
  )
  result <- accumulate_downstream_allocations(network, local)
  expect_equal(result$cumulative_flow_cfs[result$comid == "C"], 6)
  expect_equal(result$cumulative_flow_cfs[result$comid == "D"], 6)
  expect_equal(result$cumulative_instream_cfs[result$comid == "D"], 3)
})

test_that("first-order allocations propagate before report filtering", {
  network <- sf::st_as_sf(
    data.frame(
      comid = c("headwater", "mainstem"),
      hydroseq = c(2, 1), dnhydroseq = c(1, 0), streamorde = c(1, 2),
      wkt = c("LINESTRING(0 0, 1 0)", "LINESTRING(1 0, 2 0)"),
      stringsAsFactors = FALSE
    ), wkt = "wkt", crs = 4326
  )
  local <- data.frame(
    comid = "headwater", local_record_count = 1,
    local_flow_cfs = 4, local_instream_cfs = 0, local_non_instream_cfs = 4
  )
  accumulated <- accumulate_downstream_allocations(network, local)
  reported <- filter_reportable_reaches(accumulated, minimum_streamorder = 2)
  expect_equal(nrow(reported), 1)
  expect_equal(reported$cumulative_flow_cfs, 4)
})
