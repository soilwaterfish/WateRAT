#' Read a clipped layer from a local NHDPlus geodatabase
#'
#' Applies an AOI as a GDAL spatial filter while reading, rather than loading a
#' national layer and clipping it in memory. Set `clip = TRUE` to also trim the
#' returned geometries to the exact AOI after the source-side filter.
#'
#' @param path Path to a local NHDPlus geodatabase or other GDAL-supported
#'   vector datasource.
#' @param filter_geom An `sf` or `sfc` AOI.
#' @param layer Layer to read.
#' @param clip Whether to trim selected geometries to the exact AOI.
#' @param required_fields Optional field names that must be present after field
#'   names are standardized to lower case.
#' @return An `sf` object limited to `filter_geom`.
#' @export
read_nhdplus_aoi <- function(
    path,
    filter_geom,
    layer = "NHDFlowline_Network",
    clip = TRUE,
    required_fields = NULL) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop("`path` must be an existing local vector datasource.", call. = FALSE)
  }
  if (!inherits(filter_geom, "sf") && !inherits(filter_geom, "sfc")) {
    stop("`filter_geom` must be an sf or sfc AOI.", call. = FALSE)
  }
  if (length(filter_geom) == 0L) {
    stop("`filter_geom` must contain at least one geometry.", call. = FALSE)
  }
  layers <- sf::st_layers(path)$name
  if (!layer %in% layers) {
    stop(
      "Layer `", layer, "` was not found in `path`. Available layers: ",
      paste(layers, collapse = ", "), call. = FALSE
    )
  }
  if (grepl('"', layer, fixed = TRUE)) {
    stop("`layer` cannot contain a double quote.", call. = FALSE)
  }

  # Reading zero records obtains the layer CRS without materializing the layer.
  schema <- sf::read_sf(
    path, query = paste0('SELECT * FROM "', layer, '" LIMIT 0'),
    quiet = TRUE
  )
  aoi <- sf::st_as_sf(filter_geom)
  aoi <- sf::st_transform(aoi, sf::st_crs(schema))
  aoi <- sf::st_union(aoi)
  result <- sf::read_sf(
    path, layer = layer, wkt_filter = sf::st_as_text(aoi), quiet = TRUE
  )
  geometry_column <- attr(result, "sf_column")
  names(result) <- tolower(names(result))
  sf::st_geometry(result) <- tolower(geometry_column)

  missing <- setdiff(tolower(required_fields), names(result))
  if (length(missing)) {
    stop(
      "NHDPlus layer `", layer, "` is missing required fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  if (clip && nrow(result)) {
    result <- suppressWarnings(sf::st_intersection(result, aoi))
  }
  result
}

#' Read routing-ready local NHDPlus flowlines for an AOI
#'
#' This is the local-dataset counterpart to `nhdplusTools::get_nhdplus()` for
#' the fields required by WateRAT's downstream accumulation workflow.
#'
#' @inheritParams read_nhdplus_aoi
#' @return An `sf` flowline layer with lower-case NHDPlus field names.
#' @export
read_nhdplus_flowlines <- function(
    path,
    filter_geom,
    layer = "NHDFlowline_Network",
    clip = TRUE) {
  read_nhdplus_aoi(
    path = path,
    filter_geom = filter_geom,
    layer = layer,
    clip = clip,
    required_fields = c("comid", "hydroseq", "dnhydroseq", "streamorde")
  )
}
