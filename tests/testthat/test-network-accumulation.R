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

test_that("local NHDPlus catchments assign PODs to FEATUREID COMIDs", {
  water_rights <- sf::st_as_sf(
    data.frame(
      state = c("MT", "MT"), right_id = c("A", "B"), site_id = c("1", "2"),
      record_id = c("MT:1", "MT:2"), status = "ACTIVE", source = "CREEK",
      beneficial_use = NA_character_, diversion_start = "08/01", diversion_end = "08/31",
      max_flow_cfs = c(1, 1), diversion_rate = c(1, 1), diversion_rate_unit = "CFS",
      volume = NA_real_, volume_unit = NA_character_, is_instream = FALSE,
      report_url = NA_character_, x = c(0.5, 3), y = c(0.5, 0.5)
    ), coords = c("x", "y"), crs = 4326
  )
  catchments <- sf::st_as_sf(
    data.frame(
      FEATUREID = 101L,
      wkt = "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"
    ), wkt = "wkt", crs = 4326
  )

  indexed <- index_water_right_catchments(water_rights, catchments)
  primary_index <- index_water_right_comids(
    water_rights[1, , drop = FALSE], catchments = catchments
  )

  expect_equal(indexed$comid, c("101", NA_character_))
  expect_true(is.na(indexed$nldi_error[[1L]]))
  expect_match(indexed$nldi_error[[2L]], "No intersecting local")
  expect_equal(primary_index$comid, "101")
  expect_true(is.na(primary_index$nldi_error))
})

test_that("final output pairs every allocation field with its percent", {
  accumulated <- sf::st_as_sf(
    data.frame(
      comid = "A",
      cumulative_intersecting_flow_all_together = 5,
      cumulative_intersecting_flow_all_together_instream = 2,
      cumulative_intersecting_flow_private = 3,
      x = 0, y = 0
    ), coords = c("x", "y"), crs = 4326
  )
  flowmet <- sf::st_as_sf(
    data.frame(comid = "A", maug_hist = 20, qe_08 = 7, x = 0, y = 0),
    coords = c("x", "y"), crs = 4326
  )

  output <- format_capture_sites_output(accumulated, flowmet)

  expect_equal(
    names(output)[seq_len(9L)],
    c(
      "comid", "maug_hist", "qe_08",
      "intersecting_flow_all_together", "intersecting_flow_all_together_percent",
      "intersecting_flow_all_together_instream", "intersecting_flow_all_together_instream_percent",
      "intersecting_flow_private", "intersecting_flow_private_percent"
    )
  )
  expect_equal(output$intersecting_flow_all_together_percent, 25)
  expect_equal(output$intersecting_flow_all_together_instream_percent, 10)
  expect_equal(output$intersecting_flow_private_percent, 15)
})
