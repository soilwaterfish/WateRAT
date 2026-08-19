#' Assign water rights to NHDPlus COMIDs
#'
#' Use the NLDI point index to assign each POD to the COMID identified by the
#' hydrofabric. This is a hydrologic-location lookup, not geometric snapping.
#'
#' @param water_rights Canonical point water-right records.
#' @param cache_path Optional `.rds` checkpoint path. Existing successful or
#'   unsuccessful lookups are reused and the result is saved after each
#'   indexing batch, so a long indexing run can be resumed.
#' @param workers Number of concurrent NLDI point-index requests. Values above
#'   2 are capped to protect the public service. On Windows, requests run
#'   sequentially because `mclapply()` is unavailable.
#' @param throttle_seconds Pause between request batches. With the default one
#'   worker, this is the minimum pause between requests.
#' @param retries Number of retries for a failed NLDI request. Retries use
#'   exponential backoff and unresolved errors remain in `nldi_error`.
#' @param quiet Whether to suppress progress messages.
#' @return A data frame with `record_id`, `comid`, and `nldi_error`.
#' @export
index_water_right_comids <- function(water_rights, cache_path = NULL, workers = 1L,
                                     throttle_seconds = 0.5, retries = 3L,
                                     quiet = FALSE) {
  validate_water_rights(water_rights)
  points <- sf::st_transform(water_rights, 4326)
  required <- c("record_id", "comid", "nldi_error")
  cached <- data.frame(
    record_id = character(), comid = character(), nldi_error = character(),
    stringsAsFactors = FALSE
  )
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if (length(setdiff(required, names(cached)))) {
      stop("`cache_path` does not contain a compatible NLDI index.", call. = FALSE)
    }
    if (!identical(attr(cached, "nldi_endpoint"), "hydrolocation")) {
      if (!quiet) message("Ignoring a cache created with a different NLDI endpoint.")
      cached <- cached[0, required, drop = FALSE]
    } else {
      cached <- cached[, required, drop = FALSE]
    }
  }
  cached <- cached[!duplicated(cached$record_id, fromLast = TRUE), , drop = FALSE]
  workers <- as.integer(workers)
  if (length(workers) != 1L || is.na(workers) || workers < 1L) {
    stop("`workers` must be a positive integer.", call. = FALSE)
  }
  if (workers > 2L) {
    if (!quiet) message("Capping NLDI workers at 2 to avoid overloading the public service.")
    workers <- 2L
  }
  if (length(throttle_seconds) != 1L || is.na(throttle_seconds) || throttle_seconds < 0) {
    stop("`throttle_seconds` must be one non-negative number.", call. = FALSE)
  }
  retries <- as.integer(retries)
  if (length(retries) != 1L || is.na(retries) || retries < 0L) {
    stop("`retries` must be a non-negative integer.", call. = FALSE)
  }
  pending <- which(!points$record_id %in% cached$record_id | !is.na(cached$nldi_error[match(points$record_id, cached$record_id)]))
  if (!quiet) message("Indexing ", length(pending), " of ", nrow(points), " POD records with NLDI.")
  lookup_one <- function(i) {
    last_error <- NULL
    for (attempt in seq_len(retries + 1L)) {
      result <- tryCatch(
        data.frame(
          record_id = points$record_id[[i]],
          comid = nldi_comid_function(points[i, ])$comid[[1L]],
          nldi_error = NA_character_, stringsAsFactors = FALSE
        ),
        error = function(error) error
      )
      if (!inherits(result, "error")) return(result)
      last_error <- result
      if (attempt <= retries) {
        Sys.sleep(max(throttle_seconds, 0.25) * 2 ^ (attempt - 1L))
      }
    }
    data.frame(
      record_id = points$record_id[[i]], comid = NA_character_,
      nldi_error = conditionMessage(last_error), stringsAsFactors = FALSE
    )
  }
  batches <- split(pending, ceiling(seq_along(pending) / workers))
  lookups <- list()
  for (batch_number in seq_along(batches)) {
    if (batch_number > 1L && throttle_seconds > 0) Sys.sleep(throttle_seconds)
    batch <- batches[[batch_number]]
    batch_results <- if (length(batch) && workers > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(batch, lookup_one, mc.cores = workers)
    } else {
      lapply(batch, lookup_one)
    }
    lookups <- c(lookups, batch_results)
  }
  if (length(lookups)) cached <- rbind(cached, do.call(rbind, lookups))
  cached <- cached[!duplicated(cached$record_id, fromLast = TRUE), , drop = FALSE]
  attr(cached, "nldi_endpoint") <- "hydrolocation"
  if (!is.null(cache_path)) saveRDS(cached, cache_path)
  cached[match(points$record_id, cached$record_id), required, drop = FALSE]
}

#' Aggregate local water-right allocations by COMID
#'
#' @param water_rights Canonical water-right records.
#' @param comid_index Output from [index_water_right_comids()].
#' @return A data frame with direct allocation metrics for each COMID.
#' @export
aggregate_local_allocations <- function(water_rights, comid_index) {
  validate_water_rights(water_rights)
  required <- c("record_id", "comid")
  if (length(setdiff(required, names(comid_index)))) {
    stop("`comid_index` must contain record_id and comid.", call. = FALSE)
  }
  water_rights <- dplyr::select(water_rights, -dplyr::any_of(setdiff(required, "record_id")))
  joined <- dplyr::inner_join(water_rights, comid_index[, required], by = "record_id")
  joined <- dplyr::filter(joined, !is.na(.data$comid))
  joined <- sf::st_drop_geometry(joined)
  if (!"fs_intersection" %in% names(joined)) joined$fs_intersection <- FALSE
  dplyr::summarise(
    dplyr::group_by(joined, .data$comid),
    local_record_count = dplyr::n(),
    local_flow_cfs = sum(.data$max_flow_cfs, na.rm = TRUE),
    local_instream_cfs = sum(.data$max_flow_cfs[.data$is_instream], na.rm = TRUE),
    local_non_instream_cfs = sum(.data$max_flow_cfs[!.data$is_instream], na.rm = TRUE),
    intersecting_flow_all_together = sum(.data$max_flow_cfs, na.rm = TRUE),
    intersecting_flow_all_together_instream = sum(.data$max_flow_cfs[.data$is_instream], na.rm = TRUE),
    intersecting_flow_all_together_non_instream = sum(.data$max_flow_cfs[!.data$is_instream], na.rm = TRUE),
    intersecting_flow_fs = sum(.data$max_flow_cfs[.data$fs_intersection], na.rm = TRUE),
    intersecting_flow_fs_instream = sum(.data$max_flow_cfs[.data$fs_intersection & .data$is_instream], na.rm = TRUE),
    intersecting_flow_fs_non_instream = sum(.data$max_flow_cfs[.data$fs_intersection & !.data$is_instream], na.rm = TRUE),
    intersecting_flow_private = sum(.data$max_flow_cfs[!.data$fs_intersection], na.rm = TRUE),
    intersecting_flow_private_instream = sum(.data$max_flow_cfs[!.data$fs_intersection & .data$is_instream], na.rm = TRUE),
    intersecting_flow_private_non_instream = sum(.data$max_flow_cfs[!.data$fs_intersection & !.data$is_instream], na.rm = TRUE),
    local_record_ids = paste(.data$record_id, collapse = ", "),
    .groups = "drop"
  )
}

#' Build directed NHDPlus network edges
#'
#' Build immediate upstream-to-downstream COMID edges from each reach's
#' `hydroseq` and `dnhydroseq`. A confluence naturally has multiple rows with
#' the same downstream COMID. Divergences are retained as supplied by the
#' hydrofabric; an upstream allocation therefore propagates along every listed
#' downstream edge.
#'
#' @param network NHDPlus flowlines containing `comid`, `hydroseq`, and
#'   `dnhydroseq`.
#' @return A data frame with `upstream_comid` and `downstream_comid`.
#' @export
nhdplus_network_edges <- function(network) {
  required <- c("comid", "hydroseq", "dnhydroseq")
  missing <- setdiff(required, names(network))
  if (length(missing)) {
    stop("`network` is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  nodes <- sf::st_drop_geometry(network)
  downstream <- dplyr::transmute(nodes, dnhydroseq = .data$hydroseq, downstream_comid = as.character(.data$comid))
  edges <- dplyr::transmute(nodes, upstream_comid = as.character(.data$comid), dnhydroseq = .data$dnhydroseq)
  edges <- dplyr::left_join(edges, downstream, by = "dnhydroseq")
  edges <- dplyr::filter(edges, !is.na(.data$downstream_comid), .data$upstream_comid != .data$downstream_comid)
  dplyr::distinct(edges[, c("upstream_comid", "downstream_comid")])
}

#' Accumulate allocations downstream through an NHDPlus network
#'
#' Start with local COMID allocation totals and propagate them through the
#' directed hydrofabric in topological order. At confluences, all upstream
#' totals are added. At divergences, totals propagate to every listed downstream
#' edge; this matches all-path network semantics and should be reviewed before
#' interpreting alternative-channel totals.
#'
#' @param network NHDPlus flowlines.
#' @param local_allocations Output from [aggregate_local_allocations()].
#' @return `network` with local and cumulative allocation metrics.
#' @export
accumulate_downstream_allocations <- function(network, local_allocations) {
  edges <- nhdplus_network_edges(network)
  nodes <- as.character(network$comid)
  if (anyDuplicated(nodes)) stop("`network` must have one row per COMID.", call. = FALSE)

  local <- local_allocations[match(nodes, local_allocations$comid), , drop = FALSE]
  metric_names <- setdiff(
    names(local_allocations)[vapply(local_allocations, is.numeric, logical(1))],
    "comid"
  )
  for (metric in metric_names) {
    if (!metric %in% names(local)) local[[metric]] <- 0
    local[[metric]][is.na(local[[metric]])] <- 0
  }

  indegree <- stats::setNames(integer(length(nodes)), nodes)
  for (downstream in edges$downstream_comid) indegree[[downstream]] <- indegree[[downstream]] + 1L
  queue <- names(indegree)[indegree == 0L]
  cumulative <- local[, metric_names, drop = FALSE]
  cumulative$comid <- nodes
  processed <- character()

  while (length(queue)) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    processed <- c(processed, current)
    outgoing <- edges[edges$upstream_comid == current, , drop = FALSE]
    for (downstream in outgoing$downstream_comid) {
      source_row <- match(current, cumulative$comid)
      target_row <- match(downstream, cumulative$comid)
      cumulative[target_row, metric_names] <- cumulative[target_row, metric_names] + cumulative[source_row, metric_names]
      indegree[[downstream]] <- indegree[[downstream]] - 1L
      if (indegree[[downstream]] == 0L) queue <- c(queue, downstream)
    }
  }
  if (length(processed) != length(nodes)) {
    stop("NHDPlus edges contain a cycle or references outside the supplied network.", call. = FALSE)
  }

  output <- dplyr::mutate(network, comid = as.character(.data$comid))
  output <- dplyr::left_join(output, local[, c("comid", metric_names), drop = FALSE], by = c("comid" = "comid"))
  for (metric in metric_names) output[[metric]][is.na(output[[metric]])] <- 0
  names(output)[match(metric_names, names(output))] <- metric_names
  cumulative_names <- sub("^local_", "cumulative_", metric_names)
  cumulative_names[!startsWith(metric_names, "local_")] <- paste0(
    "cumulative_", metric_names[!startsWith(metric_names, "local_")]
  )
  names(cumulative)[match(metric_names, names(cumulative))] <- cumulative_names
  dplyr::left_join(output, cumulative, by = c("comid" = "comid"))
}

#' Format network accumulation like capture-sites output
#'
#' @param accumulated_network Output from [accumulate_downstream_allocations()].
#' @param flowmet FlowMet records with `comid` and `maug_hist`.
#' @return A flowline layer with the same intersecting-flow fields and percent
#'   calculations used by the current targets workflow.
#' @export
format_capture_sites_output <- function(accumulated_network, flowmet) {
  flowmet <- sf::st_drop_geometry(flowmet)
  flowmet$comid <- as.character(flowmet$comid)
  keep <- c(
    "comid", "qe_08", attr(accumulated_network, "sf_column"),
    grep("^cumulative_intersecting_flow_", names(accumulated_network), value = TRUE)
  )
  accumulated_network <- accumulated_network[, unique(keep), drop = FALSE]
  capture_fields <- grep("^cumulative_intersecting_flow_", names(accumulated_network), value = TRUE)
  output <- dplyr::left_join(
    accumulated_network,
    dplyr::select(flowmet, dplyr::any_of(c("comid", "maug_hist", "qe_08"))),
    by = "comid"
  )
  for (field in capture_fields) {
    target <- sub("^cumulative_", "", field)
    output[[target]] <- output[[field]]
  }
  output$intersecting_flow_all_together_percent <- 100 * output$intersecting_flow_all_together / output$maug_hist
  output$intersecting_flow_fs_percent <- 100 * output$intersecting_flow_fs / output$maug_hist
  output$intersecting_flow_private_percent <- 100 * output$intersecting_flow_private / output$maug_hist
  output[, c(
    "comid",
    sub("^cumulative_", "", capture_fields),
    "maug_hist", "qe_08",
    "intersecting_flow_all_together_percent",
    "intersecting_flow_fs_percent",
    "intersecting_flow_private_percent",
    attr(output, "sf_column")
  ), drop = FALSE]
}

#' Keep reportable reaches after network accumulation
#'
#' @param capture_sites Network output from
#'   [accumulate_downstream_allocations()].
#' @param minimum_streamorder Lowest Strahler order to retain in the published
#'   output. Apply this only after accumulation so allocations on first-order
#'   reaches can still propagate downstream.
#' @return `capture_sites` filtered to the requested stream order.
#' @export
filter_reportable_reaches <- function(capture_sites, minimum_streamorder = 2L) {
  if (!"streamorde" %in% names(capture_sites)) {
    stop("`capture_sites` must contain NHDPlus `streamorde`.", call. = FALSE)
  }
  minimum_streamorder <- as.integer(minimum_streamorder)
  if (length(minimum_streamorder) != 1L || is.na(minimum_streamorder)) {
    stop("`minimum_streamorder` must be one integer.", call. = FALSE)
  }
  dplyr::filter(capture_sites, .data$streamorde >= minimum_streamorder)
}
