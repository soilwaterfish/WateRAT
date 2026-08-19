#' Get filtered Montana points of diversion
#'
#' Read a local Montana `WRQS_PODS` layer and apply the analysis eligibility
#' filters. The AOI is passed to GDAL as a spatial filter before records are
#' materialized, then checked with `sf` for exact intersection.
#'
#' @param filter_geom An `sf` geometry used to retain intersecting PODs.
#' @param layer Layer name within the Montana water-right geodatabase.
#' @param local_path Path to the Montana water-right geodatabase.
#' @param active_only Whether to restrict the returned data to rights currently
#'   eligible for analysis: `WR_STATUS = "ACTIVE"`, non-missing
#'   `MAX_FLOW_CFS`, `SOURCE_TYPE = "SURFACE"`, and a purpose other than
#'   `POWER GENERATION, NONCONSUMPTIVE`. Set to `FALSE` when creating an update
#'   snapshot so retired and otherwise ineligible rights are retained for
#'   comparison.
#' @return An `sf` object of filtered Montana PODs.
#' @export
get_mtwr <- function(filter_geom, layer, local_path, active_only = TRUE) {
  if (!file.exists(local_path)) {
    stop("`local_path` must be an existing local vector datasource.", call. = FALSE)
  }
  layer_schema <- sf::read_sf(
    local_path,
    query = paste0("SELECT * FROM ", layer, " LIMIT 0"),
    quiet = TRUE
  )
  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(layer_schema))
  pods <- sf::read_sf(
    local_path, layer = layer,
    wkt_filter = sf::st_as_text(sf::st_union(filter_geom)),
    quiet = TRUE
  )
  required <- c("WR_STATUS", "MAX_FLOW_CFS", "SOURCE_TYPE", "PURPOSES")
  missing <- setdiff(required, names(pods))
  if (length(missing)) {
    stop(
      "`", layer, "` is not a Montana POD layer with required fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }

  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(pods))
  pods <- pods[lengths(sf::st_intersects(pods, filter_geom)) > 0L, , drop = FALSE]
  if (active_only) {
    pods <- pods[
      toupper(trimws(pods$WR_STATUS)) == "ACTIVE" &
        !is.na(pods$MAX_FLOW_CFS) &
        toupper(trimws(pods$SOURCE_TYPE)) == "SURFACE" &
        toupper(trimws(pods$PURPOSES)) != "POWER GENERATION, NONCONSUMPTIVE",
      , drop = FALSE
    ]
  }
  pods
}
