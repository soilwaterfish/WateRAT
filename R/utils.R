#' Get Basin Boundary NLDI
#' @description  This function uses the USGS water data API to link a point to a realized basin. This is
#' not the same as delineating from the exact point, rather this API uses NLDI to find the closest
#' basin downstream source point. There is a lot you can do with this API and I would recommend
#' looking at {nhdplusTools} as that has a lot of functionality and better documentation.
#' @param point A sf point object.
#' @noRd
#' @return An sf object with added \code{comid} and \code{basin}.
#' @note \code{point} needs geometry column.

get_Basin <- function(point){


  if(!'POINT' %in% sf::st_geometry_type(point)){"Need a sf POINT geometry"}

  #just added indexs to group by

  original_crs <- sf::st_crs(point)

  point <- point %>% dplyr::mutate(rowid = dplyr::row_number()) %>% sf::st_transform(4326)

  final_basin <- point %>%
    split(.$rowid) %>%
    purrr::map(purrr::safely(~nldi_basin_function(.))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf() %>%
    dplyr::left_join(sf::st_drop_geometry(point), by = 'rowid') %>%
    dplyr::select(-rowid) %>%
    sf::st_transform(crs = original_crs)

}



#' Calling NLDI API
#'
#' @param point sf data.frame
#' @noRd
#' @return a sf data.frame with watershed basin
nldi_basin_function <- function(point){

  clat <- point$geometry[[1]][[2]]
  clng <- point$geometry[[1]][[1]]
  rowid <- point$rowid
  ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))


  nldiURLs <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin")

  nldi_data <- sf::read_sf(nldiURLs)

  nldi_data <- nldi_data %>%
    dplyr::mutate(comid = nld$features$properties$identifier,
                  rowid = rowid)

}

#' Index a point to an NHDPlus COMID
#'
#' A COMID-only counterpart to `nldi_basin_function()`. It calls NLDI's
#' hydrolocation endpoint, which returns a valid flowline COMID by snapping a
#' nearby point or tracing downhill to the appropriate downstream flowline. It
#' does not download a basin polygon.
#'
#' @param point A one-row `sf` point object.
#' @return The input point with a character `comid` column. `comid` is `NA`
#'   when NLDI has no indexed feature for the location.
#' @export
nldi_comid_function <- function(point) {
  if (!inherits(point, "sf") || nrow(point) != 1L ||
      !all(sf::st_geometry_type(point) %in% "POINT")) {
    stop("`point` must be a one-row sf object with POINT geometry.", call. = FALSE)
  }
  original_crs <- sf::st_crs(point)
  point_4326 <- sf::st_transform(point, 4326)
  coordinates <- sf::st_coordinates(point_4326)[1L, ]
  url <- paste0(
    "https://api.water.usgs.gov/nldi/linked-data/hydrolocation?coords=POINT%28",
    coordinates[[1L]], "%20", coordinates[[2L]], "%29"
  )
  response <- httr2::request(url) |>
    httr2::req_timeout(20) |>
    httr2::req_perform()
  payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
  properties <- payload$properties
  if (is.null(properties) && !is.null(payload$features$properties)) {
    properties <- payload$features$properties
  }
  if (is.null(properties) && length(payload$features) && !is.null(payload$features[[1L]]$properties)) {
    properties <- payload$features[[1L]]$properties
  }
  comid <- properties$comid
  if (is.null(comid)) comid <- properties$identifier
  point$comid <- if (is.null(comid) || !length(comid)) {
    NA_character_
  } else {
    as.character(comid[[1L]])
  }
  sf::st_transform(point, original_crs)
}


#' Intersecting sites within Basin
#'
#' @param x List of POLYGONS
#' @param tog Data that was joined earlier.
#' @export
#'
#' @return A POLYGON with other sites that intersect it
capture_sites_within <- function(x, tog) {
  validate_water_rights(tog)
  intersecting_sites <- sf::st_intersects(tog, x)
  intersecting_sites <- purrr::map_vec(intersecting_sites,
                                       ~dplyr::if_else(length(.x) == 0, FALSE, TRUE))
  record_ids <- tog[intersecting_sites, ]$record_id
  adding_flows <- tog %>% dplyr::filter(.data$record_id %in% record_ids)
x <- x %>% dplyr::mutate(intersecting_sites = stringr::str_c(record_ids[!is.na(record_ids)],
                          collapse = ", "),
                         intersecting_flow_all_together = sum(adding_flows$max_flow_cfs,
                          na.rm = TRUE),
                         intersecting_flow_all_together_instream = sum(adding_flows[adding_flows$is_instream,
                          ]$max_flow_cfs, na.rm = TRUE),
                         intersecting_flow_all_together_non_instream = sum(adding_flows[!adding_flows$is_instream,
                          ]$max_flow_cfs, na.rm = TRUE),
                         intersecting_flow_fs = sum(adding_flows[adding_flows$fs_intersection,
                          ]$max_flow_cfs, na.rm = TRUE),
                         intersecting_flow_fs_instream = sum(adding_flows[adding_flows$fs_intersection &
                          adding_flows$is_instream, ]$max_flow_cfs,
                          na.rm = TRUE),
                         intersecting_flow_fs_non_instream = sum(adding_flows[adding_flows$fs_intersection &
                          !adding_flows$is_instream, ]$max_flow_cfs,
                          na.rm = TRUE),
                         intersecting_flow_private = sum(adding_flows[!adding_flows$fs_intersection,
                          ]$max_flow_cfs,
                          na.rm = TRUE), intersecting_flow_private_instream = sum(adding_flows[!adding_flows$fs_intersection &
                          adding_flows$is_instream, ]$max_flow_cfs,
                          na.rm = TRUE),
                         intersecting_flow_private_non_instream = sum(adding_flows[!adding_flows$fs_intersection &
                          !adding_flows$is_instream, ]$max_flow_cfs,
                                                     na.rm = TRUE))
}


#' Clean dates
#'
#' @param data A previously created joined POU and POD object.
#'
#' @return A sf object
#' @export
#'
date_cleaning <- function(data) {
  data <- data %>% dplyr::filter(!is.na(PERIOD_OF_DIVERSIONS))
  char <- data[nchar(data$PERIOD_OF_DIVERSIONS) == max(nchar(data$PERIOD_OF_DIVERSIONS),
                                                       na.rm = T), ][1, ]$PERIOD_OF_DIVERSIONS
  char_split <- strsplit(char, ";")[[1]]
  date_string <- vector()
  for (i in 1:(length(char_split) * 2)) {
    if (as.logical(i%%2)) {
      date_string[[i]] <- paste0("as.Date('2000-08-15') > ",
                                 paste0("data$date_", 1:(length(char_split) *
                                                           2))[[i]], " & as.Date('2000-08-15') < ", paste0("data$date_",
                                                                                                           1:(length(char_split) * 2))[[i + 1]])
    }
  }
  date_string <- date_string[!is.na(date_string)]
  data <- data %>% tidyr::separate_wider_delim(PERIOD_OF_DIVERSIONS,
                                               delim = stringr::regex(" to |;"), names = paste0("date_",
                                                                                                1:(length(char_split) * 2)), too_few = "align_start") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date_"),
                                ~dplyr::if_else(is.na(.x), paste0("2000-01-01"),
                                                paste0("2000-", stringr::str_replace_all(.x,
                                                                                         "/", "-"))))) %>% dplyr::mutate(dplyr::across(dplyr::starts_with("date_"),
                                                                                                                                       ~as.Date(.x)))
  date_logic <- dplyr::tibble(.rows = nrow(data))
  for (i in 1:length(date_string)) {
    date_name <- paste0("date", i)
    date_logic[[date_name]] <- eval(parse(text = date_string[[i]]))
  }
  date_logic <- date_logic %>% dplyr::rowwise() %>% dplyr::mutate(final_logic = any(dplyr::c_across(everything())))
  data <- data[date_logic$final_logic, ]
  data <- sf::st_as_sf(data)
}


#' Get POD basins
#'
#' @param data A previously filter flowmet object
#' @param crs A `sf::st_crs()` object.
#'
#' @return
#' @export
get_pod_basin <- function(comid, crs) {

  basin <- nhdplusTools::get_nldi_basin(
    list(
      featureSource = "comid",
      featureID = as.character(comid)
    )
  )

  # NLDI may legitimately return no basin.
  if (is.null(basin) || nrow(basin) == 0L) {
    return(NULL)
  }

  basin <- sf::st_zm(basin)

  basin <- basin[
    !sf::st_is_empty(basin),
    ,
    drop = FALSE
  ]

  if (nrow(basin) == 0L) {
    return(NULL)
  }

  basin %>%
    dplyr::mutate(
      comid = as.character(comid)
    ) %>%
    sf::st_transform(
      crs = crs
    )
}

#' FS Logic
#'
#' @param data A previously created joined POU and POD object.
#' @param admin_int A previously created administration intersected sf object.
#'
#' @return A sf object with fs logic of intersection
#' @export
#'
fs_logic <- function(data, admin_int) {

  pou_pod_int <- sf::st_intersects(data, sf::st_transform(admin_int, sf::st_crs(data)))

  logic <- lengths(pou_pod_int) > 0

  data['fs_intersection'] <- logic

  data
}
