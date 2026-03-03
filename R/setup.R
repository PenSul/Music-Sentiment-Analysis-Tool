# setup.R
# ---------------------------------------------------------------------------
# Environment-preparation utilities: directory creation, package checking,
# lexicon loading, and stopword assembly.
# ---------------------------------------------------------------------------


#' Create all project directories required for data and output storage.
#'
#' Checks for the existence of each directory defined in config.R and creates
#' any that are missing.  Called once at the start of the pipeline.
#'
#' @return TRUE invisibly on success.
CreateDirectories <- function() {
  dirs <- list(
    "Data"           = kDataDir,
    "Lyrics"         = kLyricsDir,
    "Comments"       = kCommentsDir,
    "Output"         = kOutputDir,
    "Visualizations" = kVisualsDir
  )

  for (label in names(dirs)) {
    path <- dirs[[label]]
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    cat(sprintf("  [OK] %s directory: %s\n", label, path))
  }

  return(invisible(TRUE))
}


#' Verify that every required R package is installed; install any that are
#' missing, then load them all.
#'
#' The required-package list covers data wrangling, text mining, API access,
#' visualisation, and report generation.
#'
#' @return TRUE invisibly on success.
CheckAndInstallPackages <- function() {
  requiredPackages <- c(
    "tidyverse", "tidytext", "tuber", "rvest", "httr", "jsonlite",
    "textdata", "sentimentr", "wordcloud", "plotly", "knitr",
    "rmarkdown", "dplyr", "stringr", "ggplot2", "scales",
    "ggrepel", "htmlwidgets", "RColorBrewer", "reticulate"
  )

  missingPackages <- requiredPackages[
    !requiredPackages %in% installed.packages()[, "Package"]
  ]

  if (length(missingPackages) > 0) {
    cat("Installing missing packages:",
        paste(missingPackages, collapse = ", "), "\n")
    install.packages(missingPackages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }

  for (pkg in requiredPackages) {
    library(pkg, character.only = TRUE)
  }

  return(invisible(TRUE))
}


#' Download and return the four supported sentiment lexicons.
#'
#' Attempts to load the NRC, AFINN, Bing, and Loughran lexicons from the
#' tidytext ecosystem.  Failures are reported as warnings so the pipeline
#' can continue with whichever lexicons are available.
#'
#' @return A named list of data frames, one entry per successfully loaded
#'   lexicon.
PrepareTextData <- function() {
  lexicons <- list()

  for (name in c("nrc", "afinn", "bing", "loughran")) {
    tryCatch({
      lexicons[[name]] <- get_sentiments(name)
      cat(sprintf("  [OK] %s lexicon loaded.\n", toupper(name)))
    }, error = function(e) {
      warning(sprintf("Failed to load %s lexicon: %s", name, e$message))
    })
  }

  return(lexicons)
}


#' Build a comprehensive stopword vector.
#'
#' Merges the standard English stopwords shipped with tidytext and the
#' domain-specific stopwords defined in kCustomStopwords (config.R).
#'
#' @return A character vector of unique stopwords.
GetStopwords <- function() {
  baseStopwords <- tidytext::stop_words$word
  allStopwords  <- unique(c(baseStopwords, kCustomStopwords))
  return(allStopwords)
}
