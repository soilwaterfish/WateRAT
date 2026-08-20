test_that("network allocation metadata writes an FGDC XML sidecar", {
  allocation <- sf::st_as_sf(
    data.frame(
      comid = "123",
      maug_hist = 20,
      qe_08 = 7,
      intersecting_flow_all_together = 5,
      intersecting_flow_all_together_percent = 25,
      x = -112, y = 46
    ),
    coords = c("x", "y"), crs = 4326
  )
  metadata_path <- tempfile(fileext = ".xml")

  returned_path <- write_network_allocation_metadata(
    allocation,
    path = metadata_path,
    gpkg_path = "network_allocation.gpkg"
  )
  metadata <- xml2::read_xml(metadata_path)

  expect_equal(returned_path, metadata_path)
  expect_equal(xml2::xml_name(metadata), "metadata")
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(metadata, ".//metstdn")),
    "Content Standard for Digital Geospatial Metadata (CSDGM)"
  )
  attribute_names <- xml2::xml_text(xml2::xml_find_all(metadata, ".//attr/attrlabl"))
  expect_true(all(names(sf::st_drop_geometry(allocation)) %in% attribute_names))
  expect_match(
    xml2::xml_text(xml2::xml_find_first(metadata, ".//attr[attrlabl = 'intersecting_flow_all_together_percent']/attrdef")),
    "maug_hist"
  )
  expect_match(
    xml2::xml_text(xml2::xml_find_first(metadata, ".//srcinfo[1]/srccite/citeinfo/title")),
    "Montana DNRC"
  )
})

test_that("prepared water-right metadata describes the reusable POD cache", {
  prepped <- sf::st_as_sf(
    data.frame(
      state = "ID", right_id = "83-11945", site_id = "12", record_id = "ID:12",
      status = "ACTIVE", source = "EXAMPLE CREEK", beneficial_use = "STOCKWATER",
      diversion_start = "08/01", diversion_end = "08/31", max_flow_cfs = 1,
      diversion_rate = 1, diversion_rate_unit = "CFS", volume = NA_real_,
      volume_unit = NA_character_, is_instream = FALSE, report_url = NA_character_,
      comid = "123", nldi_error = NA_character_, fs_intersection = FALSE,
      x = -112, y = 46
    ),
    coords = c("x", "y"), crs = 4326
  )
  metadata_path <- tempfile(fileext = ".xml")

  write_prepped_water_rights_metadata(prepped, metadata_path, "water_rights.gpkg")
  metadata <- xml2::read_xml(metadata_path)

  expect_equal(
    xml2::xml_text(xml2::xml_find_first(metadata, ".//enttypl")),
    "water_rights_prepped"
  )
  expect_match(
    xml2::xml_text(xml2::xml_find_first(metadata, ".//attr[attrlabl = 'comid']/attrdef")),
    "NHDPlus"
  )
  expect_match(
    xml2::xml_text(xml2::xml_find_first(metadata, ".//srcinfo[2]/srccite/citeinfo/title")),
    "Idaho Department"
  )
})
