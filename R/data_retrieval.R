#' Get Montana Water Rights Data
#' @param filter_geom an object of class bbox be used for clipping.
#' @param layer Layer name within the `https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip` gdb.
#' @param local_path A file path character string to the MT gdb for water rights POU and POD layers. 'Z:/some_path/wr_rights.gdb'
#' @param active_only Whether to restrict the returned data to rights currently
#'   eligible for analysis. Set to `FALSE` when creating an update snapshot so
#'   retired rights are retained for comparison.
#' @return
#' @export
#'
get_mtwr <- function(filter_geom, layer, local_path = NULL, active_only = TRUE) {

  tmp <- tempfile(fileext = ".gpkg")

  x <- st_read(local_path,
    layer = layer,
    query = paste0("SELECT * FROM ",layer," LIMIT 0"),
    quiet = TRUE
  )

  filter_geom <- sf::st_transform(filter_geom, sf::st_crs(x))

  bb <- sf::st_bbox(filter_geom)

  tmpclp <- tempfile(fileext = ".shp")

  sf::write_sf(filter_geom, tmpclp)


    where_clause <- if (!active_only) {
      ""
    } else if (layer == "WRQS_PODS") {
      "-where \"WR_STATUS = 'ACTIVE' AND MAX_FLOW_RT IS NOT NULL\""
    } else {
      "-where \"WR_STATUS = 'ACTIVE' AND SOURCE_TYPE = 'SURFACE'\""
    }

    system(paste("ogr2ogr -spat ", paste(bb[[1]], bb[[2]],
                                         bb[[3]], bb[[4]]), "-clipsrc ", tmpclp, where_clause,
                 " -f \"GPKG\"", tmp, local_path, paste(layer, sep = " ",
                                                        collapse = " ")), intern = TRUE)

  tmp
}


#' Get FlowMet Data
#'
#' @description
#' A map service depicting modeled streamflow metrics from the historical time period (1977-2006) in the United States.
#' In addition to standard NHD attributes, the streamflow datasets include metrics on mean daily flow (annual and seasonal),
#' flood levels associated with 1.5-year, 10-year, and 25-year floods; annual and decadal minimum weekly flows and date of
#'  minimum weekly flow, center of flow mass date; baseflow index, and average number of winter floods. These files and
#'  additional information are available on the project website, https://www.fs.usda.gov/rm/boise/AWAE/projects/modeled_stream_flow_metrics.shtml. Streams without flow metrics (null values) were removed from this dataset to improve display speed; to see all stream lines, use an NHD flowline dataset.
#'
#'
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param local_path A file path character string to the MT gdb for water rights POU and POD layers. 'Z:/some_path/wr_rights.gdb'
#' @param layer A character
#' @param ... Arguments to pass to \link[arcgislayers] package `arc_select` function.
#' @return
#' @export
#'
get_flowmet <- function(filter_geom,layer, local_path = NULL, ...){


  if(is.null(local_path)){

  url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_HydroFlowMetricsHistorical_01/MapServer/15')

  flowmet <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

  } else {

    tmp <- tempfile(fileext = '.shp')

    bb <- sf::st_bbox(filter_geom)

    tmpclp <- tempfile(fileext = '.shp')

    sf::write_sf(filter_geom, tmpclp)

    system(paste("ogr2ogr -spat ", paste(bb[[1]], bb[[2]],
                                         bb[[3]], bb[[4]]), "-clipsrc ", tmpclp,
                 " -f \"GPKG\"", tmp, local_path, paste(layer, sep = " ",
                                                        collapse = " ")), intern = TRUE)

    tmp
  }

}


#' Get USDA-Forest Service Administration Boundaries
#'
#' @description
#' An area depicted as surface ownership parcels dissolved on the same ownership classification. Go to this URL for full metadata description: https://data.fs.usda.gov/geodata/edw/edw_resources/meta/S_USA.BasicOwnership.xml
#'
#'
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to \link[arcgislayers] package `arc_select` function.
#' @return
#' @export
#'
get_adminboundaries <- function(filter_geom, ...) {

url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_BasicOwnership_01/MapServer/0')

admin <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

}

