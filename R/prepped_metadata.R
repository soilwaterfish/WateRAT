#' Write FGDC metadata for a prepared water-right cache
#'
#' Creates a companion XML metadata record using the FGDC Content Standard for
#' Digital Geospatial Metadata (CSDGM). The record describes WaterRAT's
#' combined prepared POD cache, including its canonical fields, COMID indexing,
#' Forest Service intersection, and state-source provenance.
#'
#' @param water_rights Prepared canonical WaterRAT water-right records.
#' @param path Path for the XML metadata file.
#' @param gpkg_path Path to the companion GeoPackage, when available.
#' @param layer GeoPackage layer name.
#' @return `path`, invisibly. The XML file is written as a sidecar to the
#'   GeoPackage.
#' @export
write_prepped_water_rights_metadata <- function(
    water_rights,
    path,
    gpkg_path = NA_character_,
    layer = "water_rights_prepped") {
  validate_water_rights(water_rights)
  required <- c("comid", "nldi_error", "fs_intersection")
  missing <- setdiff(required, names(water_rights))
  if (length(missing)) {
    stop("`water_rights` is not prepared. Missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be one non-empty XML file path.", call. = FALSE)
  }

  add_node <- function(parent, name, value = NULL) {
    child <- xml2::xml_add_child(parent, name)
    if (!is.null(value) && length(value) && !is.na(value[[1L]])) {
      xml2::xml_set_text(child, as.character(value[[1L]]))
    }
    child
  }
  add_citation <- function(parent, title, url = NULL, element = "citation") {
    citation <- add_node(parent, element)
    citeinfo <- add_node(citation, "citeinfo")
    add_node(citeinfo, "origin", "WaterRAT")
    add_node(citeinfo, "pubdate", format(Sys.Date(), "%Y%m%d"))
    add_node(citeinfo, "title", title)
    if (!is.null(url)) add_node(citeinfo, "onlink", url)
    citation
  }
  field_definition <- function(field) {
    canonical <- water_right_schema()
    prepared <- c(
      comid = "NHDPlus catchment FEATUREID used as the routing COMID; NLDI is used only as a fallback for unresolved points.",
      nldi_error = "COMID-assignment audit message; populated when no single local catchment match or fallback NLDI result is available.",
      fs_intersection = "TRUE when the point of diversion intersects the supplied USDA Forest Service ownership boundary."
    )
    if (field %in% names(canonical)) return(unname(canonical[[field]]))
    if (field %in% names(prepared)) return(unname(prepared[[field]]))
    if (field %in% c("condition_codes", "condition_text", "idwr_uses")) {
      return("Idaho Water Right Report audit value retained from the source report page.")
    }
    if (field %in% c("selected_comid", "pod_candidate_count", "pod_selection_method")) {
      return("Montana repeated-POD selection audit value used to retain the downstream-most eligible POD for a WRKEY site.")
    }
    paste0("State-specific WaterRAT preparation audit attribute: ", field, ".")
  }
  source_records <- list(
    list("Montana DNRC Water Rights Query System water-right geodatabase", "https://opendata-mtdnrc.hub.arcgis.com/maps/edf5bfa459304c3bb8af3361b21e92f2/about"),
    list("Idaho Department of Water Resources points of diversion and Water Right Reports", "https://data-idwr.hub.arcgis.com/datasets/f0b37d653f8249a4945d61bdb98dc4a7_0/explore"),
    list("NHDPlus Version 2.1 National Seamless Lower 48 geodatabase", "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"),
    list("USDA Forest Service administrative boundaries and ownership data", "https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries")
  )

  bbox_layer <- if (!is.na(sf::st_crs(water_rights))) sf::st_transform(water_rights, 4326) else water_rights
  bbox <- sf::st_bbox(bbox_layer)
  crs_input <- sf::st_crs(water_rights)$input
  if (is.null(crs_input) || is.na(crs_input) || !nzchar(crs_input)) crs_input <- "Not reported"
  attributes <- sf::st_drop_geometry(water_rights)

  root <- xml2::xml_new_root("metadata")
  idinfo <- add_node(root, "idinfo")
  add_citation(idinfo, "WaterRAT prepared water-right cache", gpkg_path)
  descript <- add_node(idinfo, "descript")
  add_node(descript, "abstract", paste(
    "Combined, reusable WaterRAT point-of-diversion cache for network allocation.",
    "State records are filtered and standardized to a canonical schema, assigned a routing COMID,",
    "and classified for Forest Service intersection before entering the network-analysis pipeline."
  ))
  add_node(descript, "purpose", "Provide an auditable, incremental handoff from state ingestion to multi-state network accumulation.")
  status <- add_node(idinfo, "status")
  add_node(status, "progress", "Complete")
  add_node(status, "update", "As needed")
  bounding <- add_node(add_node(idinfo, "spdom"), "bounding")
  add_node(bounding, "westbc", bbox[["xmin"]])
  add_node(bounding, "eastbc", bbox[["xmax"]])
  add_node(bounding, "northbc", bbox[["ymax"]])
  add_node(bounding, "southbc", bbox[["ymin"]])
  add_node(idinfo, "accconst", "None")
  add_node(idinfo, "useconst", "Use with authoritative state-source terms. This cache supports analysis and review; it is not a legal water-right determination.")

  dataqual <- add_node(root, "dataqual")
  lineage <- add_node(dataqual, "lineage")
  for (source in source_records) {
    srcinfo <- add_node(lineage, "srcinfo")
    add_citation(srcinfo, source[[1L]], source[[2L]], element = "srccite")
    add_node(srcinfo, "typesrc", "Digital vector data")
  }
  process <- add_node(lineage, "procstep")
  add_node(process, "procdesc", paste(
    "WaterRAT filters state PODs to eligible August surface-water records, standardizes fields,",
    "reuses unchanged Idaho report scrapes, assigns local NHDPlus Catchment FEATUREID COMIDs,",
    "uses NLDI only for unresolved points, and records Forest Service intersection. State updates",
    "replace only their own rows in the combined GeoPackage cache."
  ))
  add_node(process, "procdate", format(Sys.Date(), "%Y%m%d"))

  spdoinfo <- add_node(root, "spdoinfo")
  add_node(spdoinfo, "direct", "Vector")
  ptvctinf <- add_node(spdoinfo, "ptvctinf")
  sdtsterm <- add_node(ptvctinf, "sdtsterm")
  add_node(sdtsterm, "sdtstype", "Point")
  esriterm <- add_node(add_node(ptvctinf, "ptvctcnt"), "esriterm")
  add_node(esriterm, "efeageom", "Point")
  add_node(esriterm, "efeacnt", nrow(water_rights))
  geodetic <- add_node(add_node(root, "spref"), "horizsys") |> add_node("geodetic")
  add_node(geodetic, "horizdn", crs_input)

  detailed <- add_node(add_node(root, "eainfo"), "detailed")
  enttyp <- add_node(detailed, "enttyp")
  add_node(enttyp, "enttypl", layer)
  add_node(enttyp, "enttypd", "Prepared WaterRAT point-of-diversion records.")
  for (field in names(attributes)) {
    attr <- add_node(detailed, "attr")
    add_node(attr, "attrlabl", field)
    add_node(attr, "attrdef", field_definition(field))
    add_node(attr, "attrdefs", "WaterRAT canonical schema and preparation contract")
    domain <- add_node(attr, "attrdomv")
    add_node(domain, if (is.numeric(attributes[[field]])) "rdom" else "udom", "See attribute definition.")
  }
  metainfo <- add_node(root, "metainfo")
  add_node(metainfo, "metd", format(Sys.Date(), "%Y%m%d"))
  add_node(metainfo, "metc", "WaterRAT package")
  add_node(metainfo, "metstdn", "Content Standard for Digital Geospatial Metadata (CSDGM)")
  add_node(metainfo, "metstdv", "FGDC-STD-001-1998")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  xml2::write_xml(root, path, options = "format")
  invisible(path)
}
