test_that("canonical schema validates records and drives allocation summaries", {
  water_rights <- sf::st_as_sf(
    data.frame(
      state = c("ID", "ID"), right_id = c("A", "B"), site_id = c("1", "2"),
      record_id = c("ID:1:1", "ID:2:1"), status = "Active", source = "STREAM",
      beneficial_use = c("IRRIGATION", "INSTREAM FLOW"), diversion_start = "1/01",
      diversion_end = "12/31", max_flow_cfs = c(1, 2), diversion_rate = c(1, 2),
      diversion_rate_unit = "CFS", volume = NA_real_, volume_unit = NA_character_,
      is_instream = c(FALSE, TRUE), report_url = NA_character_, x = c(0, 5), y = c(0, 0)
    ),
    coords = c("x", "y"), crs = 4326
  )
  validate_water_rights(water_rights)
  water_rights$fs_intersection <- c(FALSE, TRUE)
  basin <- sf::st_as_sf(data.frame(comid = "1", wkt = "POLYGON((-1 -1, 6 -1, 6 1, -1 1, -1 -1))"),
                         wkt = "wkt", crs = 4326)
  result <- capture_sites_within(basin, water_rights)
  expect_equal(result$intersecting_flow_all_together, 3)
  expect_equal(result$intersecting_flow_all_together_instream, 2)
  expect_equal(result$intersecting_flow_private, 1)
})

test_that("month filtering selects August and retains annual records", {
  water_rights <- sf::st_as_sf(
    data.frame(
      state = "ID", right_id = c("A", "A", "A"), site_id = c("7", "8", "annual"),
      record_id = c("ID:1:7", "ID:1:8", "ID:1:annual"), status = "Active",
      source = "STREAM", beneficial_use = "MINIMUM STREAM FLOW",
      diversion_start = c("7/1", "8/1", "1/01"),
      diversion_end = c("7/31", "8/31", "12/31"),
      max_flow_cfs = c(44, 17, 1), diversion_rate = c(44, 17, 1),
      diversion_rate_unit = "CFS", volume = NA_real_, volume_unit = NA_character_,
      is_instream = TRUE, report_url = NA_character_, x = c(0, 1, 2), y = 0
    ),
    coords = c("x", "y"), crs = 4326
  )
  august <- filter_water_rights_month(water_rights, month = 8)
  expect_setequal(august$record_id, c("ID:1:8", "ID:1:annual"))
  expect_equal(august$max_flow_cfs[august$record_id == "ID:1:8"], 17)
})

test_that("month filtering averages segmented Idaho rates into one POD", {
  water_rights <- sf::st_as_sf(
    data.frame(
      state = c("ID", "ID"), right_id = c("83-11945", "83-11945"),
      site_id = c("10", "10"), record_id = c("ID:10:1", "ID:10:2"),
      status = "Active", source = "EXAMPLE CREEK", beneficial_use = "WILD AND SCENIC RIVER",
      diversion_start = c("08/01", "08/16"), diversion_end = c("08/15", "08/31"),
      max_flow_cfs = c(431, 333), diversion_rate = c(431, 333), diversion_rate_unit = "CFS",
      volume = NA_real_, volume_unit = NA_character_, is_instream = TRUE,
      report_url = "https://example.test", x = c(-116, -116), y = c(46, 46)
    ),
    coords = c("x", "y"), crs = 4326
  )

  august <- filter_water_rights_month(water_rights, month = 8L)

  expect_equal(nrow(august), 1L)
  expect_equal(august$record_id, "ID:10")
  expect_equal(august$max_flow_cfs, 382)
  expect_equal(august$diversion_start, "08/01")
  expect_equal(august$diversion_end, "08/31")
})
