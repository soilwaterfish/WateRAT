#!/usr/bin/env Rscript

# Local, live-reloading preview for the Jekyll documentation site.
#
# Usage from an R console at the package root:
#   source("scripts/preview_docs.R")
#
# This project uses only a small, static subset of Jekyll/Liquid. Rendering it
# here avoids requiring a system Ruby/Jekyll install just to preview docs.

docs_dir <- normalizePath("docs", mustWork = TRUE)
preview_dir <- file.path(docs_dir, "_preview")

read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

render_page <- function(path, layout, site_title, site_description) {
  page_lines <- readLines(path, warn = FALSE)
  front_matter_end <- which(page_lines == "---")[[2L]]
  title_line <- grep("^title:", page_lines[seq_len(front_matter_end)], value = TRUE)
  title <- sub("^title:[[:space:]]*", "", title_line[[1L]])
  content <- paste(page_lines[-seq_len(front_matter_end)], collapse = "\n")
  output <- layout
  output <- gsub(
    "{% if page.title %}{{ page.title }} · {% endif %}{{ site.title }}",
    paste(title, site_title, sep = " · "), output, fixed = TRUE
  )
  output <- gsub("{{ site.description }}", site_description, output, fixed = TRUE)
  output <- gsub("{{ site.title }}", site_title, output, fixed = TRUE)
  output <- gsub("{{ page.title | default: site.title }}", title, output, fixed = TRUE)
  output <- gsub("{{ content }}", content, output, fixed = TRUE)
  replacements <- c(
    "{{ '/' | relative_url }}" = "index.html",
    "{{ '/setup/' | relative_url }}" = "setup.html",
    "{{ '/filters/' | relative_url }}" = "filters.html",
    "{{ '/schema/' | relative_url }}" = "schema.html",
    "{{ '/targets/' | relative_url }}" = "targets.html",
    "{{ '/network/' | relative_url }}" = "network.html",
    "{{ '/output-gpkg/' | relative_url }}" = "output-gpkg.html",
    "{{ '/assets/css/site.css' | relative_url }}" = "assets/css/site.css",
    "{{ '/assets/js/site.js' | relative_url }}" = "assets/js/site.js"
  )
  for (needle in names(replacements)) {
    output <- gsub(needle, replacements[[needle]], output, fixed = TRUE)
  }
  output
}

render_docs <- function(...) {
  unlink(preview_dir, recursive = TRUE, force = TRUE)
  dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)
  layout <- read_text(file.path(docs_dir, "_layouts", "default.html"))
  config <- readLines(file.path(docs_dir, "_config.yml"), warn = FALSE)
  config_value <- function(key) {
    line <- grep(paste0("^", key, ":"), config, value = TRUE)[[1L]]
    sub(paste0("^", key, ":[[:space:]]*"), "", line)
  }
  site_title <- config_value("title")
  site_description <- config_value("description")
  pages <- list.files(docs_dir, pattern = "\\.html$", full.names = TRUE, recursive = FALSE)
  for (page in pages) {
    output <- render_page(page, layout, site_title, site_description)
    writeLines(output, file.path(preview_dir, basename(page)), useBytes = TRUE)
  }
  file.copy(file.path(docs_dir, "assets"), preview_dir, recursive = TRUE)
  message("Rendered ", length(pages), " documentation pages.")
  invisible(TRUE)
}

render_docs()
servr::httw(
  dir = preview_dir,
  watch = docs_dir,
  all_files = TRUE,
  filter = function(paths) !grepl("/_preview/", paths, fixed = TRUE),
  handler = function(changed) {
    render_docs()
    message("Updated preview after: ", paste(changed, collapse = ", "))
  },
  port = 4321,
  browse = TRUE
)

# `servr` stays alive naturally in an interactive R console. Keep a terminal
# invocation alive as well, so `Rscript scripts/preview_docs.R` is usable.
if (!interactive()) {
  message("Live preview is running; press Ctrl+C here to stop it.")
  repeat Sys.sleep(60)
}
