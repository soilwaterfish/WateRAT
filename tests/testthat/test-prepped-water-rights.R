test_that("prepped water-right cache joins COMIDs and FS ownership", {
  water_rights <- sf::st_as_sf(
    data.frame(
      state = c("MT", "MT"), right_id = c("A", "B"), site_id = c("1", "2"),
      record_id = c("MT:1", "MT:2"), status = "ACTIVE", source = "EXAMPLE CREEK",
      beneficial_use = NA_character_, diversion_start = "08/01", diversion_end = "08/31",
      max_flow_cfs = c(1, 2), diversion_rate = c(1, 2), diversion_rate_unit = "CFS",
      volume = NA_real_, volume_unit = NA_character_, is_instream = FALSE,
      report_url = NA_character_, x = c(0, 2), y = 0
    ),
    coords = c("x", "y"), crs = 4326
  )
  comid_index <- data.frame(
    record_id = c("MT:1", "MT:2"), comid = c("10", NA_character_),
    nldi_error = c(NA_character_, "No linked flowline")
  )
  fs_boundary <- sf::st_as_sf(
    data.frame(wkt = "POLYGON((-1 -1, 1 -1, 1 1, -1 1, -1 -1))"),
    wkt = "wkt", crs = 4326
  )

  prepped <- prepare_network_water_rights(water_rights, comid_index, fs_boundary)
  cache_path <- tempfile(fileext = ".gpkg")
  write_prepped_water_rights(prepped, cache_path)
  cached <- read_prepped_water_rights(cache_path)

  expect_equal(prepped$comid, c("10", NA_character_))
  expect_equal(prepped$fs_intersection, c(TRUE, FALSE))
  expect_equal(cached$record_id, prepped$record_id)
  expect_equal(cached$fs_intersection, prepped$fs_intersection)
})

test_that("prepped cache replaces only the states supplied in an update", {
  make_prepped <- function(state, record_id, x) {
    sf::st_as_sf(
      data.frame(
        state = state, right_id = "right", site_id = "site", record_id = record_id,
        status = "ACTIVE", source = "EXAMPLE CREEK", beneficial_use = NA_character_,
        diversion_start = "08/01", diversion_end = "08/31", max_flow_cfs = 1,
        diversion_rate = 1, diversion_rate_unit = "CFS", volume = NA_real_,
        volume_unit = NA_character_, is_instream = FALSE, report_url = NA_character_,
        comid = "10", nldi_error = NA_character_, fs_intersection = FALSE, x = x, y = 0
      ),
      coords = c("x", "y"), crs = 4326
    )
  }
  cache_path <- tempfile(fileext = ".gpkg")
  write_prepped_water_rights(make_prepped("MT", "MT:old", 0), cache_path)
  write_prepped_water_rights(make_prepped("ID", "ID:one", 1), cache_path)
  write_prepped_water_rights(make_prepped("MT", "MT:new", 2), cache_path)
  cached <- read_prepped_water_rights(cache_path)

  expect_setequal(cached$record_id, c("MT:new", "ID:one"))
})

test_that("first refresh writes added records without a prior cache", {
  water_rights <- sf::st_as_sf(
    data.frame(
      state = "MT", right_id = "A", site_id = "1", record_id = "MT:1",
      status = "ACTIVE", source = "EXAMPLE CREEK", beneficial_use = NA_character_,
      diversion_start = "08/01", diversion_end = "08/31", max_flow_cfs = 1,
      diversion_rate = 1, diversion_rate_unit = "CFS", volume = NA_real_,
      volume_unit = NA_character_, is_instream = FALSE, report_url = NA_character_,
      x = 0, y = 0
    ),
    coords = c("x", "y"), crs = 4326
  )
  fs_boundary <- sf::st_as_sf(
    data.frame(wkt = "POLYGON((-1 -1, 1 -1, 1 1, -1 1, -1 -1))"),
    wkt = "wkt", crs = 4326
  )
  cache_path <- tempfile(fileext = ".gpkg")
  result <- refresh_network_water_rights(
    water_rights, fs_boundary = fs_boundary,
    nldi_cache_path = tempfile(fileext = ".rds"), cache_path = cache_path,
    comid_index = data.frame(record_id = "MT:1", comid = "10", nldi_error = NA_character_)
  )

  expect_equal(result$changes$change_type, "added")
  expect_true(file.exists(cache_path))
})

test_that("combined cache preserves state-specific audit fields", {
  make_prepped <- function(state, record_id, x) {
    sf::st_as_sf(
      data.frame(
        state = state, right_id = "right", site_id = "site", record_id = record_id,
        status = "ACTIVE", source = "EXAMPLE CREEK", beneficial_use = NA_character_,
        diversion_start = "08/01", diversion_end = "08/31", max_flow_cfs = 1,
        diversion_rate = 1, diversion_rate_unit = "CFS", volume = NA_real_,
        volume_unit = NA_character_, is_instream = FALSE, report_url = NA_character_,
        comid = "10", nldi_error = NA_character_, fs_intersection = FALSE, x = x, y = 0
      ),
      coords = c("x", "y"), crs = 4326
    )
  }
  mt <- make_prepped("MT", "MT:1", 0)
  mt$pod_selection_method <- "lowest_hydroseq"
  id <- make_prepped("ID", "ID:1", 1)
  id$condition_codes <- "148"
  cache_path <- tempfile(fileext = ".gpkg")
  write_prepped_water_rights(mt, cache_path)
  write_prepped_water_rights(id, cache_path)
  cached <- read_prepped_water_rights(cache_path)

  expect_true(all(c("pod_selection_method", "condition_codes") %in% names(cached)))
  expect_equal(cached$condition_codes[cached$state == "ID"], "148")
  expect_equal(cached$pod_selection_method[cached$state == "MT"], "lowest_hydroseq")
})
