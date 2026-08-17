test_that("water-right changes select their downstream basins", {
  old_pods <- sf::st_as_sf(
    data.frame(
      PODV_ID_SEQ = c(1L, 2L),
      WRKEY = c("A", "B"),
      WR_STATUS = c("ACTIVE", "ACTIVE"),
      MAX_FLOW_CFS = c(1, 2),
      SOURCE_TYPE = c("SURFACE", "SURFACE"),
      x = c(0, 10),
      y = c(0, 0)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
  previous <- water_right_snapshot(old_pods, "2026-01-01")

  current_pods <- old_pods
  current_pods$WR_STATUS[2] <- "RETIRED"
  current_pods <- rbind(
    current_pods,
    sf::st_as_sf(
      data.frame(
        PODV_ID_SEQ = 3L,
        WRKEY = "C",
        WR_STATUS = "ACTIVE",
        MAX_FLOW_CFS = 1,
        SOURCE_TYPE = "SURFACE",
        x = 1,
        y = 1
      ),
      coords = c("x", "y"),
      crs = 4326
    )
  )
  current <- water_right_snapshot(current_pods, "2026-08-12")

  changes <- detect_water_right_changes(previous, current)
  expect_setequal(changes$change_type, c("added", "retired"))

  basins <- sf::st_as_sf(
    data.frame(
      comid = c("lower_a", "lower_b", "unaffected"),
      wkt = c(
        "POLYGON((-2 -2, 2 -2, 2 2, -2 2, -2 -2))",
        "POLYGON((8 -2, 12 -2, 12 2, 8 2, 8 -2))",
        "POLYGON((20 -2, 22 -2, 22 2, 20 2, 20 -2))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )
  expect_setequal(affected_downstream_comids(changes, basins), c("lower_a", "lower_b"))
  expect_setequal(affected_downstream_basins(changes, basins)$comid, c("lower_a", "lower_b"))
})
