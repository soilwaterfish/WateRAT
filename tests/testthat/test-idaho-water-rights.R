test_that("IDWR Water Uses tables are parsed and totals are omitted", {
  html <- rvest::read_html(paste0(
    "<table><thead><tr><th>Beneficial Use</th><th>From</th><th>To</th>",
    "<th>Diversion Rate</th><th>Volume</th></tr></thead><tbody>",
    "<tr><td>IRRIGATION</td><td>3/15</td><td>11/15</td><td>1,200.5 CFS</td><td>67.5 AFA</td></tr>",
    "<tr><td>DOMESTIC</td><td>1/01</td><td>12/31</td><td>0.02 CFS</td><td> </td></tr>",
    "<tr><td>TOTAL</td><td></td><td></td><td>1200.52 CFS</td><td></td></tr>",
    "</tbody></table>"
  ))

  uses <- WateRAT:::.idwr_water_uses_table(html)

  expect_equal(nrow(uses), 2L)
  expect_equal(uses$beneficial_use, c("IRRIGATION", "DOMESTIC"))
  expect_equal(uses$diversion_rate, c(1200.5, 0.02))
  expect_equal(uses$diversion_rate_unit, c("CFS", "CFS"))
  expect_equal(uses$volume, c(67.5, NA_real_))
  expect_equal(uses$vol_unit, c("AFA", NA_character_))
})

test_that("IDWR POD filters are case-insensitive for status", {
  pods <- sf::st_as_sf(
    data.frame(
      Source = c("GROUND WATER", "SPRING"),
      Status = c("Active", "inactive"),
      WRReport = c("one", "two"), x = c(0, 1), y = c(0, 1)
    ),
    coords = c("x", "y"), crs = 4326
  )
  expect_equal(
    nrow(pods[toupper(trimws(pods$Status)) %in% toupper(trimws("active")), ]),
    1L
  )
})

test_that("IDWR POD source exclusions are case-insensitive", {
  pods <- sf::st_as_sf(
    data.frame(
      Source = c("GROUND WATER", "SPRING"),
      Status = c("Active", "Active"),
      WRReport = c("one", "two"), x = c(0, 1), y = c(0, 1)
    ),
    coords = c("x", "y"), crs = 4326
  )
  retained <- pods[!toupper(trimws(pods$Source)) %in% toupper(trimws("ground water")), ]
  expect_equal(retained$Source, "SPRING")
})

test_that("Idaho records standardize to the canonical schema", {
  raw <- sf::st_as_sf(
    data.frame(
      WaterRightNumber = "85-11482", PointOfDiversionID = 10L,
      Status = "Active", Source = "SCHMIDT CREEK", WRReport = "https://example.test",
      beneficial_use = "STOCKWATER", from = "1/01", to = "12/31",
      diversion_rate = 0.02, diversion_rate_unit = "CFS",
      volume = NA_real_, vol_unit = NA_character_, x = -116, y = 46
    ),
    coords = c("x", "y"), crs = 4326
  )
  standardized <- standardize_idwr_water_rights(raw)
  expect_true(all(names(water_right_schema()) %in% names(standardized)))
  expect_equal(standardized$state, "ID")
  expect_equal(standardized$max_flow_cfs, 0.02)
  expect_false(standardized$is_instream)
})
