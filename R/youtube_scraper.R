# youtube_scraper.R
# ---------------------------------------------------------------------------
# YouTube comment collection using a plain API-key GET request (no OAuth
# required).  This is the lightweight alternative to the tuber-based
# CollectYouTubeComments() in data_collection.R.
# ---------------------------------------------------------------------------

library(httr)
library(jsonlite)


#' Collect YouTube comments for every song using a direct API-key request.
#'
#' Iterates over the songs in \code{songData}, extracts the YouTube video ID
#' from each SongLink, and calls the YouTube Data API v3 commentThreads
#' endpoint.  Results are saved per-song and in a combined CSV.
#'
#' @param songData   A data frame returned by \code{ReadSongList()}.
#' @param maxResults Integer. Maximum number of comments to fetch per video.
#' @return A data frame with columns SongID, VideoID, CommentID, Author,
#'   Comment, LikeCount, and PublishedAt.
CollectYouTubeCommentsWithAPIKey <- function(songData, maxResults = 100) {
  apiKey <- kApiCredentials$youtube$api_key

  if (is.null(apiKey) || apiKey == "") {
    stop("YouTube API key is missing from kApiCredentials.")
  }

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

  # Helper: extract video ID from a YouTube URL.
  extractId <- function(url) {
    if (is.na(url) || url == "") return(NA)
    if (grepl("youtube\\.com/watch\\?v=", url)) {
      return(gsub(".*v=([^&]+).*", "\\1", url))
    } else if (grepl("youtu\\.be/", url)) {
      return(gsub(".*youtu\\.be/([^?&]+).*", "\\1", url))
    }
    return(NA)
  }

  for (i in seq_len(nrow(songData))) {
    videoId <- extractId(songData$SongLink[i])

    if (is.na(videoId)) {
      cat(sprintf("Skipping song %d: no valid YouTube URL.\n", i))
      next
    }

    cat(sprintf("Fetching comments for video %s (%d / %d)\n",
                videoId, i, nrow(songData)))

    apiUrl <- "https://www.googleapis.com/youtube/v3/commentThreads"

    tryCatch({
      response <- GET(
        url   = apiUrl,
        query = list(
          part       = "snippet",
          videoId    = videoId,
          maxResults = maxResults,
          key        = apiKey,
          textFormat = "plainText"
        )
      )

      if (http_error(response)) {
        errorContent <- content(response, "parsed")
        cat(sprintf("  API error: %s\n", errorContent$error$message))
        next
      }

      rawContent <- content(response, "text", encoding = "UTF-8")
      parsedData <- fromJSON(rawContent)

      if (!is.null(parsedData$items) && length(parsedData$items) > 0) {
        items <- parsedData$items

        videoComments <- data.frame(
          SongID      = songData$SongID[i],
          VideoID     = videoId,
          CommentID   = items$id,
          Author      = items$snippet$topLevelComment$snippet$authorDisplayName,
          Comment     = items$snippet$topLevelComment$snippet$textDisplay,
          LikeCount   = as.integer(
            items$snippet$topLevelComment$snippet$likeCount
          ),
          PublishedAt = items$snippet$topLevelComment$snippet$publishedAt,
          stringsAsFactors = FALSE
        )

        filePath <- file.path(
          kCommentsDir,
          paste0(songData$SongID[i], "_comments.csv")
        )
        write.csv(videoComments, filePath, row.names = FALSE)

        commentData <- rbind(commentData, videoComments)

        cat(sprintf("  Saved %d comments for video %s.\n",
                    nrow(videoComments), videoId))
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
