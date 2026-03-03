# lyrics_scraper.R
# ---------------------------------------------------------------------------
# Multi-method lyrics retrieval: direct Genius web scraping with an optional
# Python lyricsgenius fallback via reticulate.
# ---------------------------------------------------------------------------

library(rvest)
library(httr)
library(dplyr)


#' Scrape lyrics for a single song directly from the Genius website.
#'
#' Sends a search request to Genius's public web API, follows the URL of the
#' first result, and extracts lyrics from the page HTML.  Several CSS
#' selectors are tried to accommodate layout changes over time.
#'
#' @param artist Character. Artist name (parenthetical credits are stripped).
#' @param title  Character. Song title.
#' @return A character string of cleaned lyrics, or NA on failure.
ScrapeLyrics <- function(artist, title) {
  artist <- trimws(gsub("\\(.*?\\)", "", artist))
  title  <- trimws(title)

  searchTerm <- paste(artist, title)
  baseUrl    <- "https://genius.com/api/search/song"

  tryCatch({
    response <- GET(url = baseUrl,
                    query = list(q = searchTerm, per_page = 5))

    if (status_code(response) != 200) {
      cat(sprintf("Search request failed for '%s' by '%s'.\n",
                  title, artist))
      return(NA)
    }

    searchData <- content(response, "parsed")

    if (length(searchData$response$sections) == 0 ||
        length(searchData$response$sections[[1]]$hits) == 0) {
      cat(sprintf("No results found for '%s' by '%s'.\n",
                  title, artist))
      return(NA)
    }

    lyricsUrl <- searchData$response$sections[[1]]$hits[[1]]$result$url

    # Retry with title only if the first search returned no URL.
    if (is.null(lyricsUrl)) {
      response   <- GET(url = baseUrl,
                        query = list(q = title, per_page = 5))
      searchData <- content(response, "parsed")

      if (length(searchData$response$sections) == 0 ||
          length(searchData$response$sections[[1]]$hits) == 0) {
        cat(sprintf("Simplified search also failed for '%s'.\n", title))
        return(NA)
      }

      lyricsUrl <- searchData$response$sections[[1]]$hits[[1]]$result$url

      if (is.null(lyricsUrl)) {
        cat(sprintf("Could not resolve lyrics URL for '%s'.\n", title))
        return(NA)
      }
    }

    cat("Found lyrics URL:", lyricsUrl, "\n")
    lyricsPage <- read_html(lyricsUrl)

    # Try multiple CSS selectors to handle Genius layout changes.
    selectors <- c(
      '[data-lyrics-container="true"]',
      '.lyrics',
      '.song_body-lyrics',
      '#lyrics-root-pin'
    )

    lyricsText <- ""
    for (selector in selectors) {
      nodes <- lyricsPage %>% html_nodes(selector)
      if (length(nodes) > 0) {
        extracted <- paste(html_text(nodes), collapse = "\n")
        if (nchar(extracted) > 0) {
          lyricsText <- extracted
          break
        }
      }
    }

    # Last-resort: regex extraction from full page text.
    if (lyricsText == "") {
      pageText     <- lyricsPage %>% html_text()
      lyricsMatch  <- regmatches(
        pageText,
        regexpr("Lyrics\\s*[\\s\\S]*?(?=Related Artists|About|Comments)",
                pageText)
      )
      if (length(lyricsMatch) > 0) {
        lyricsText <- lyricsMatch[1]
      } else {
        cat(sprintf("Could not extract lyrics for '%s' by '%s'.\n",
                    title, artist))
        return(NA)
      }
    }

    # Clean section markers and collapse whitespace.
    lyricsText <- gsub("\n+", "\n", lyricsText)
    lyricsText <- gsub("\\[.*?\\]", "", lyricsText)
    lyricsText <- trimws(lyricsText)
    return(lyricsText)

  }, error = function(e) {
    cat(sprintf("Error scraping lyrics for '%s' by '%s': %s\n",
                title, artist, e$message))
    return(NA)
  })
}


#' Retrieve lyrics via the Python lyricsgenius package (reticulate bridge).
#'
#' Requires the \code{lyricsgenius} Python module to be installed and a valid
#' Genius API client ID in kApiCredentials.
#'
#' @param artist Character. Artist name.
#' @param title  Character. Song title.
#' @return A character string of cleaned lyrics, or NA on failure.
ScrapeLyricsViaLyricsGenius <- function(artist, title) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cat("Installing missing 'reticulate' package...\n")
    install.packages("reticulate")
  }

  library(reticulate)

  tryCatch({
    cat("Checking for Python module 'lyricsgenius'...\n")
    if (!py_module_available("lyricsgenius")) {
      cat("Module 'lyricsgenius' not found. ",
          "Install it via: pip install lyricsgenius\n")
      return(NA)
    }

    lg     <- import("lyricsgenius")
    genius <- lg$Genius(kApiCredentials$genius$client_id)
    song   <- genius$search_song(title, artist)

    if (is.null(song)) {
      cat(sprintf("No result via lyricsgenius for '%s' by '%s'.\n",
                  title, artist))
      return(NA)
    }

    lyricsText <- song$lyrics
    lyricsText <- gsub("\n+", "\n", lyricsText)
    lyricsText <- gsub("\\[.*?\\]", "", lyricsText)
    lyricsText <- trimws(lyricsText)
    return(lyricsText)

  }, error = function(e) {
    cat(sprintf("lyricsgenius error for '%s' by '%s': %s\n",
                title, artist, e$message))
    return(NA)
  })
}


#' Collect lyrics for every song, trying web scraping first and falling back
#' to the Python lyricsgenius module when available.
#'
#' Individual lyrics files are saved to kLyricsDir and a combined CSV is
#' written to kOutputDir.
#'
#' @param songData A data frame returned by \code{ReadSongList()}.
#' @return A data frame with columns SongID, SongName, ArtistName,
#'   MajorityGenre, MinorityGenre, LyricsStatus, and Lyrics.
CollectLyricsDirectly <- function(songData) {
  # Attempt to set up the Python runtime for the fallback method.
  tryCatch({
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      install.packages("reticulate")
    }
  }, error = function(e) {
    cat("Cannot install reticulate:", e$message, "\n")
  })

  pythonAvailable <- tryCatch({
    reticulate::use_python(Sys.which("python"), required = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })

  lyricsData <- data.frame(
    SongID        = integer(),
    SongName      = character(),
    ArtistName    = character(),
    MajorityGenre = character(),
    MinorityGenre = character(),
    LyricsStatus  = character(),
    Lyrics        = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(songData))) {
    song   <- songData$SongName[i]
    artist <- songData$ArtistName[i]

    cat(sprintf("Fetching lyrics for: %s by %s (%d / %d)\n",
                song, artist, i, nrow(songData)))

    lyricsText <- ScrapeLyrics(artist, song)

    if ((is.na(lyricsText) || nchar(lyricsText) < 10) && pythonAvailable) {
      cat("  Retrying with lyricsgenius fallback...\n")
      lyricsText <- ScrapeLyricsViaLyricsGenius(artist, song)
    }

    if (!is.na(lyricsText) && nchar(lyricsText) > 10) {
      filename <- file.path(kLyricsDir,
                            paste0(songData$SongID[i], ".txt"))
      writeLines(lyricsText, filename)

      lyricsData <- rbind(lyricsData, data.frame(
        SongID        = songData$SongID[i],
        SongName      = song,
        ArtistName    = artist,
        MajorityGenre = songData$MajorityGenre[i],
        MinorityGenre = songData$MinorityGenre[i],
        LyricsStatus  = "found",
        Lyrics        = lyricsText,
        stringsAsFactors = FALSE
      ))
      cat(sprintf("  Saved lyrics for '%s'.\n", song))
    } else {
      lyricsData <- rbind(lyricsData, data.frame(
        SongID        = songData$SongID[i],
        SongName      = song,
        ArtistName    = artist,
        MajorityGenre = songData$MajorityGenre[i],
        MinorityGenre = songData$MinorityGenre[i],
        LyricsStatus  = "not_found",
        Lyrics        = "No lyrics available",
        stringsAsFactors = FALSE
      ))
      cat(sprintf("  No lyrics found for '%s'.\n", song))
    }

    Sys.sleep(2)
  }

  write.csv(lyricsData,
            file.path(kOutputDir, "lyrics_data.csv"),
            row.names = FALSE)
  return(lyricsData)
}


#' Generate placeholder lyrics for testing when no real lyrics are available.
#'
#' Each song is assigned a short sample string that loosely corresponds to its
#' majority genre.  This function is intended for development and testing only.
#'
#' @param songData A data frame returned by \code{ReadSongList()}.
#' @return A data frame with the same schema as \code{CollectLyricsDirectly()},
#'   with LyricsStatus set to \code{"found"} for every row.
AddManualLyrics <- function(songData) {
  sampleLyrics <- list(
    Pop        = "Sunshine in my heart, I feel the rhythm start.",
    Rock       = "Thunder in the night, I stand and hold the light.",
    Electronic = "Digital waves carry us through neon days.",
    `Hip-hop`  = "Counting every beat, walking down the street.",
    Disco      = "Dancing through the light until the morning bright."
  )

  lyricsData <- data.frame(
    SongID        = integer(),
    SongName      = character(),
    ArtistName    = character(),
    MajorityGenre = character(),
    MinorityGenre = character(),
    LyricsStatus  = character(),
    Lyrics        = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(songData))) {
    song   <- songData$SongName[i]
    artist <- songData$ArtistName[i]
    genre  <- songData$MajorityGenre[i]

    matchingGenre <- names(sampleLyrics)[1]
    for (g in names(sampleLyrics)) {
      if (grepl(g, genre, ignore.case = TRUE)) {
        matchingGenre <- g
        break
      }
    }

    lyricsText <- sampleLyrics[[matchingGenre]]

    # Introduce minor variation by rotating lines for even-numbered songs.
    lines <- unlist(strsplit(lyricsText, "\n"))
    if (i %% 2 == 0 && length(lines) > 2) {
      lines <- c(lines[2:length(lines)], lines[1])
    }
    lyricsText <- paste(lines, collapse = "\n")

    lyricsData <- rbind(lyricsData, data.frame(
      SongID        = songData$SongID[i],
      SongName      = song,
      ArtistName    = artist,
      MajorityGenre = songData$MajorityGenre[i],
      MinorityGenre = songData$MinorityGenre[i],
      LyricsStatus  = "found",
      Lyrics        = lyricsText,
      stringsAsFactors = FALSE
    ))

    cat(sprintf("Added sample lyrics for '%s' by '%s'.\n", song, artist))
  }

  write.csv(lyricsData,
            file.path(kOutputDir, "lyrics_data.csv"),
            row.names = FALSE)
  return(lyricsData)
}
