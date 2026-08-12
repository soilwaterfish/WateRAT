#!/usr/bin/env Rscript

# Render README.Rmd without Pandoc. This uses knitr directly and removes the
# YAML front matter that Pandoc would normally consume for github_document.

source_path <- "README.Rmd"
output_path <- "README.md"
temporary_path <- tempfile(fileext = ".md")

on.exit(unlink(temporary_path), add = TRUE)

knitr::knit(source_path, output = temporary_path, quiet = TRUE)
contents <- readLines(temporary_path, warn = FALSE)

if (length(contents) >= 3L && identical(contents[[1L]], "---")) {
  closing_markers <- which(contents[-1L] %in% c("---", "..."))
  if (!length(closing_markers)) {
    stop("README.Rmd has an opening YAML delimiter but no closing delimiter.")
  }
  contents <- contents[-seq_len(closing_markers[[1L]] + 1L)]
}

writeLines(contents, output_path)
