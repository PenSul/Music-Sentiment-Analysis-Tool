# data_collection.R
# ---------------------------------------------------------------------------
# Functions for reading the song catalogue, setting up API tokens, extracting
# YouTube video IDs, and collecting lyrics and YouTube comments.
# ---------------------------------------------------------------------------

source("R/config.R")
source("R/genius_auth.R")


#' Read the song-list CSV file and return a tidy data frame.
#'
#' Expects columns MajorityGenre, MinorityGenre, SongName, ArtistName, and
#' SongLink.  Rows with NA in SongName or ArtistName are dropped.  A unique
#' SongID column is appended.
#'
#' @return A data frame of song metadata with an integer SongID column.
ReadSongList <- function() {
  filePath <- kProjectSettings$csv_file_path

  if (!file.exists(filePath)) {
    stop("Song-list CSV not found at: ", filePath)
  }

  songData <- read.csv(filePath, stringsAsFactors = FALSE)

  requiredCols <- c("MajorityGenre", "MinorityGenre",
                     "SongName", "ArtistName", "SongLink")
  if (!all(requiredCols %in% colnames(songData))) {
    colnames(songData) <- requiredCols
  }

  songData <- songData %>%
    filter(!is.na(SongName), !is.na(ArtistName)) %>%
    mutate(SongID = row_number())

  cat(sprintf("Loaded %d songs from %s.\n", nrow(songData), filePath))
  return(songData)
}


#' Authenticate with the Genius and YouTube APIs.
#'
#' Reads credentials from kApiCredentials (config.R) and attempts OAuth
#' handshakes for each service.  Authentication failures are reported as
#' warnings; the pipeline can still proceed with whichever service succeeds.
#'
#' @return A named list with elements \code{genius} (token or NULL) and
#'   \code{youtube} (logical).
SetupAPITokens <- function() {
  tokens <- list()

  # -- Genius OAuth ---------------------------------------------------------
  geniusCreds <- kApiCredentials$genius
  if (!is.null(geniusCreds$client_id) &&
      !is.null(geniusCreds$client_secret)) {
    cat("Authenticating with Genius API...\n")
    tokens$genius <- tryCatch({
      GeniusOAuth(
        clientId     = geniusCreds$client_id,
        clientSecret = geniusCreds$client_secret,
        redirectUri  = geniusCreds$redirect_uri %||% "http://localhost:1410/"
      )
    }, error = function(e) {
      warning("Genius authentication failed: ", e$message)
      NULL
    })
  } else {
    warning("Genius API credentials are not configured.")
  }

  # -- YouTube OAuth --------------------------------------------------------
  ytCreds <- kApiCredentials$youtube
  if (!is.null(ytCreds$client_id) &&
      !is.null(ytCreds$client_secret)) {
    cat("Authenticating with YouTube API...\n")
    tokens$youtube <- tryCatch({
      yt_oauth(
        app_id     = ytCreds$client_id,
        app_secret = ytCreds$client_secret,
        token      = ""
      )
      TRUE
    }, error = function(e) {
      warning("YouTube authentication failed: ", e$message)
      FALSE
    })
  } else {
    warning("YouTube API credentials are not configured.")
  }

  return(tokens)
}


#' Extract a YouTube video ID from a full URL.
#'
#' Supports both \code{youtube.com/watch?v=} and \code{youtu.be/} URL formats.
#'
#' @param url Character. A YouTube video URL.
#' @return A character video ID, or NA if the URL is unrecognised.
ExtractYouTubeID <- function(url) {
  if (is.na(url) || url == "") {
    return(NA)
  }

  if (grepl("youtube\\.com/watch\\?v=", url)) {
    return(gsub(".*v=([^&]+).*", "\\1", url))
  } else if (grepl("youtu\\.be/", url)) {
    return(gsub(".*youtu\\.be/([^?&]+).*", "\\1", url))
  }

  return(NA)
}


#' Collect song lyrics from the Genius API via OAuth.
#'
#' For each song in \code{songData}, searches Genius, downloads the lyrics
#' page, and extracts the text.  Results are saved to individual text files
#' and returned in a single data frame.
#'
#' @param songData    A data frame returned by \code{ReadSongList()}.
#' @param geniusToken An OAuth token returned by \code{SetupAPITokens()}.
#' @return A data frame with columns SongID, SongName, ArtistName,
#'   MajorityGenre, MinorityGenre, LyricsStatus, and Lyrics.
CollectLyrics <- function(songData, geniusToken) {
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

    # Strip featured-artist parenthetical for cleaner search queries.
    if (grepl("\\(ft\\.", artist)) {
      artist <- gsub("\\s*\\(ft\\..*\\)", "", artist)
    }

    cat(sprintf("Fetching lyrics for: %s by %s (%d / %d)\n",
                song, artist, i, nrow(songData)))

    tryCatch({
      searchQuery   <- paste(artist, song)
      searchResults <- GeniusGet(
        paste0("search?q=", URLencode(searchQuery)),
        token = geniusToken
      )

      if (length(searchResults$hits) > 0) {
        songId   <- searchResults$hits[[1]]$result$id
        songInfo <- GeniusGet(paste0("songs/", songId),
                              token = geniusToken)
        lyricsUrl <- paste0("https://genius.com", songInfo$song$path)

        page      <- read_html(lyricsUrl)
        lyricsDivs <- html_nodes(page, ".lyrics")

        if (length(lyricsDivs) > 0) {
          lyricsText <- html_text(lyricsDivs)
        } else {
          lyricsDivs <- html_nodes(
            page, '[data-lyrics-container="true"]'
          )
          lyricsText <- paste(html_text(lyricsDivs), collapse = "\n")
        }

        lyricsText <- gsub("\n+", "\n", lyricsText)
        lyricsText <- trimws(lyricsText)

        if (nchar(lyricsText) > 0) {
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
        }
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
      }
    }, error = function(e) {
      cat(sprintf("Error fetching lyrics for %s by %s: %s\n",
                  song, artist, e$message))

      lyricsData <<- rbind(lyricsData, data.frame(
        SongID        = songData$SongID[i],
        SongName      = song,
        ArtistName    = artist,
        MajorityGenre = songData$MajorityGenre[i],
        MinorityGenre = songData$MinorityGenre[i],
        LyricsStatus  = "error",
        Lyrics        = "Error retrieving lyrics",
        stringsAsFactors = FALSE
      ))
    })

    Sys.sleep(1)
  }

  write.csv(lyricsData,
            file.path(kOutputDir, "lyrics_data.csv"),
            row.names = FALSE)
  return(lyricsData)
}


#' Collect YouTube comments for every song that has a valid YouTube link.
#'
#' Uses the \code{tuber} package (OAuth-based).  Comments are sorted by like
#' count and capped at \code{maxComments} per video.  Individual CSV files
#' are written per song, and a combined data frame is returned.
#'
#' @param songData    A data frame returned by \code{ReadSongList()}.
#' @param maxComments Integer. Maximum number of comments to collect per video.
#' @return A data frame with columns SongID, VideoID, CommentID, Author,
#'   Comment, LikeCount, and PublishedAt.
CollectYouTubeComments <- function(songData,
                                   maxComments = kProjectSettings$max_comments) {
  commentData <- data.frame(
    SongID      = integer(),
    VideoID     = character(),
    CommentID   = character(),
    Author      = character(),
    Comment     = character(),
    LikeCount   = integer(),
    PublishedAt = character(),
    stringsAsFactors = FALSE
  )

  if (!requireNamespace("tuber", quietly = TRUE)) {
    warning("Package 'tuber' is not available; cannot collect comments.")
    return(commentData)
  }

  for (i in seq_len(nrow(songData))) {
    videoId <- ExtractYouTubeID(songData$SongLink[i])

    if (is.na(videoId)) {
      cat(sprintf("Skipping song %d: no valid YouTube URL.\n", i))
      next
    }

    cat(sprintf("Fetching comments for video %s (%d / %d)\n",
                videoId, i, nrow(songData)))

    commentFilePath <- file.path(
      kCommentsDir, paste0(songData$SongID[i], "_comments.csv")
    )

    tryCatch({
      comments <- get_comment_threads(
        video_id    = videoId,
        max_results = maxComments
      )

      if (!is.null(comments) && nrow(comments) > 0) {
        songComments <- data.frame(
          SongID      = songData$SongID[i],
          VideoID     = videoId,
          CommentID   = comments$id,
          Author      = comments$authorDisplayName,
          Comment     = comments$textDisplay,
          LikeCount   = as.integer(comments$likeCount),
          PublishedAt = comments$publishedAt,
          stringsAsFactors = FALSE
        ) %>%
          arrange(desc(LikeCount)) %>%
          head(maxComments)

        write.csv(songComments, commentFilePath, row.names = FALSE)
        commentData <- rbind(commentData, songComments)

        cat(sprintf("  Saved %d comments for video %s.\n",
                    nrow(songComments), videoId))
      } else {
        cat(sprintf("  No comments found for video %s.\n", videoId))
      }
    }, error = function(e) {
      cat(sprintf("Error fetching comments for video %s: %s\n",
                  videoId, e$message))
    })

    Sys.sleep(2)
  }

  write.csv(commentData,
            file.path(kOutputDir, "all_comments.csv"),
            row.names = FALSE)
  return(commentData)
}
