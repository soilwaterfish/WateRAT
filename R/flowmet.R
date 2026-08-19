#' Get local FlowMet data for an area of interest
#'
#' Read a local FlowMet layer and spatially filter it with `sf`. When an AOI is
#' supplied, GDAL applies a WKT spatial filter before the layer is read, so the
#' full national dataset is not loaded into memory.
#'
#' @param filter_geom Optional `sf` geometry used to retain intersecting flow
#'   lines.
#' @param layer Layer name within the local FlowMet datasource.
#' @param local_path Path to a local FlowMet datasource.
#' @return An `sf` object of FlowMet lines intersecting `filter_geom`.
#' @export
get_flowmet <- function(filter_geom = NULL, layer, local_path) {
  if (!file.exists(local_path)) {
    stop("`local_path` must be an existing local vector datasource.", call. = FALSE)
  }
  if (is.null(filter_geom)) {
    return(sf::read_sf(local_path, layer = layer, quiet = TRUE))
  }
  layer_schema <- sf::read_sf(
    local_path,
    query = paste0("SELECT * FROM ", layer, " LIMIT 0"),
    quiet = TRUE
  )
  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(layer_schema))
  flowmet <- sf::read_sf(
    local_path, layer = layer,
    wkt_filter = sf::st_as_text(sf::st_union(filter_geom)),
    quiet = TRUE
  )
  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(flowmet))
  flowmet[lengths(sf::st_intersects(flowmet, filter_geom)) > 0L, , drop = FALSE]
}
