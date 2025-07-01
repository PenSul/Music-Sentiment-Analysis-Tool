# Data collection functions - where we beg APIs for data


source("R/00_config.R")
source("R/genius_auth.R")
#' Read song list from CSV file
#'
#' Read song data from a CSV file
#' The most basic shit we need to get started
#'
#' @return data.frame containing song information with columns:
#' MajorityGenre, MinorityGenre, SongName, ArtistName, SongLink, SongID
ReadSongList <- function() {
    # Check if file exists
    if (!file.exists(PROJECT_SETTINGS$csv_file_path)) {
        stop("Song list CSV file not found at: ", PROJECT_SETTINGS$csv_file_path, " - You're screwed without data.")
    }
    
    # Read data from CSV file
    songData <- read.csv(PROJECT_SETTINGS$csv_file_path, stringsAsFactors = FALSE)
    
    # If needed, clean column names based on your CSV structure
    # If the CSV already has the correct column names, can skip this
    if (!all(c("MajorityGenre", "MinorityGenre", "SongName", "ArtistName", "SongLink") %in% colnames(songData))) {
        colnames(songData) <- c("MajorityGenre", "MinorityGenre", "SongName", 
                                "ArtistName", "SongLink")
    }
    
    # Remove any rows with NA in critical columns
    songData <- songData %>%
        filter(!is.na(SongName), !is.na(ArtistName))
    
    # Add a unique ID for each song
    songData <- songData %>%
        mutate(SongID = row_number())
    
    # Display summary of loaded data
    cat("Loaded", nrow(songData), "songs from CSV file. Let's hope they're good ones.\n")
    return(songData)
}

#' Set up API authentication for Genius and YouTube
#'
#' @return List containing API tokens
SetupAPITokens <- function() {
    cat("Setting up API authentication... pray it works.\n")
    
    # Initialize tokens list
    tokens <- list()
    
    # Genius API authentication using OAuth
    if (!is.null(API_CREDENTIALS$genius$client_id) && 
        !is.null(API_CREDENTIALS$genius$client_secret)) {
        
        cat("Authenticating with Genius API... fingers crossed.\n")
        tokens$genius <- tryCatch({
            genius_oauth(
                client_id = API_CREDENTIALS$genius$client_id,
                client_secret = API_CREDENTIALS$genius$client_secret,
                redirect_uri = API_CREDENTIALS$genius$redirect_uri %||% "http://localhost:1410/"
            )
        }, error = function(e) {
            warning("Genius API authentication failed: ", e$message, " - Fucking APIs.")
            NULL
        })
    } else {
        warning("Genius API credentials not configured. Did you forget to add them?")
    }
    
    # YouTube API setup
    if (!is.null(API_CREDENTIALS$youtube$client_id) && 
        !is.null(API_CREDENTIALS$youtube$client_secret)) {
        
        cat("Authenticating with YouTube API... this better work.\n")
        tokens$youtube <- tryCatch({
            yt_oauth(
                app_id = API_CREDENTIALS$youtube$client_id,
                app_secret = API_CREDENTIALS$youtube$client_secret,
                token = ""
            )
            TRUE
        }, error = function(e) {
            warning("YouTube API authentication failed: ", e$message, " - Google hates us.")
            FALSE
        })
    } else {
        warning("YouTube API credentials not configured. Good luck with that.")
    }
    return(tokens)
}

#' Extract YouTube video ID from URL
#'
#' Parses a YouTube URL to extract the video ID.
#' Because YouTube's URLs are a damn mess
#'
#' @param url Character string of the YouTube URL
#' @return Character string of the video ID or NA if not found
ExtractYouTubeID <- function(url) {
    if (is.na(url) || url == "") {
        return(NA)
    }
    
    # Handle different YouTube URL formats
    if (grepl("youtube\\.com/watch\\?v=", url)) {
        video_id <- gsub(".*v=([^&]+).*", "\\1", url)
    } else if (grepl("youtu\\.be/", url)) {
        video_id <- gsub(".*youtu\\.be/([^?&]+).*", "\\1", url)
    } else {
        video_id <- NA
    }
    
    return(video_id)
}

#' Collect song lyrics using Genius API
#'
#' Fetches lyrics for each song in the dataset
#' Deals with Genius's crappy API so you don't have to
#'
#' @param songData data.frame containing song information
#' @param genius_token OAuth token from SetupAPITokens()
#' @return data.frame with song info and lyrics, with columns:
#'   SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, 
#'   LyricsStatus, Lyrics
CollectLyrics <- function(songData, genius_token) {
    # Create dataframe to store lyrics
    lyricsData <- data.frame(
        SongID = integer(),
        SongName = character(),
        ArtistName = character(),
        MajorityGenre = character(),
        MinorityGenre = character(),
        LyricsStatus = character(),
        Lyrics = character(),
        stringsAsFactors = FALSE
    )
    
    # Process each song
    for (i in 1:nrow(songData)) {
        song <- songData$SongName[i]
        artist <- songData$ArtistName[i]
        
        # Handle featured artists by separating main artist
        if (grepl("\\(ft\\.", artist)) {
            artist <- gsub("\\s*\\(ft\\..*\\)", "", artist)
        }
        
        # Log
        cat(sprintf("Fetching lyrics for: %s by %s (%d of %d)\n", 
                    song, artist, i, nrow(songData)))
        
        # Try to get lyrics from Genius
        tryCatch({
            # Search query / result
            search_query <- paste(artist, song)
            search_results <- genius_get(
                paste0("search?q=", URLencode(search_query)),
                token = genius_token
            )
            
            if (length(search_results$hits) > 0) {
                # Get the song ID from the first result
                song_id <- search_results$hits[[1]]$result$id
                
                # Get song details including path
                song_info <- genius_get(
                    paste0("songs/", song_id),
                    token = genius_token
                )
                
                # Extract the relative path to the lyrics page
                path <- song_info$song$path
                
                # Construct full URL
                lyrics_url <- paste0("https://genius.com", path)
                
                # Scrape lyrics using rvest
                library(rvest)
                page <- read_html(lyrics_url)
                
                # Extract lyrics div
                lyrics_div <- html_nodes(page, ".lyrics")
                if (length(lyrics_div) > 0) {
                    lyrics_text <- html_text(lyrics_div)
                } else {
                    # Try other selectors for newer Genius layout
                    lyrics_div <- html_nodes(page, '[data-lyrics-container="true"]')
                    lyrics_text <- paste(html_text(lyrics_div), collapse = "\n")
                }
                
                # Clean up lyrics
                lyrics_text <- gsub("\n+", "\n", lyrics_text)
                lyrics_text <- trimws(lyrics_text)
                
                if (nchar(lyrics_text) > 0) {
                    # Save lyrics to file
                    filename <- file.path(LYRICS_DIR, paste0(songData$SongID[i], ".txt"))
                    writeLines(lyrics_text, filename)
                    
                    # Add to dataframe
                    lyricsData <- rbind(lyricsData, data.frame(
                        SongID = songData$SongID[i],
                        SongName = song,
                        ArtistName = artist,
                        MajorityGenre = songData$MajorityGenre[i],
                        MinorityGenre = songData$MinorityGenre[i],
                        LyricsStatus = "found",
                        Lyrics = lyrics_text,
                        stringsAsFactors = FALSE
                    ))
                } else {
                    # No lyrics found
                    lyricsData <- rbind(lyricsData, data.frame(
                        SongID = songData$SongID[i],
                        SongName = song,
                        ArtistName = artist,
                        MajorityGenre = songData$MajorityGenre[i],
                        MinorityGenre = songData$MinorityGenre[i],
                        LyricsStatus = "not_found",
                        Lyrics = "No lyrics available",
                        stringsAsFactors = FALSE
                    ))
                }
            } else {
                # No search results
                lyricsData <- rbind(lyricsData, data.frame(
                    SongID = songData$SongID[i],
                    SongName = song,
                    ArtistName = artist,
                    MajorityGenre = songData$MajorityGenre[i],
                    MinorityGenre = songData$MinorityGenre[i],
                    LyricsStatus = "not_found",
                    Lyrics = "No lyrics available",
                    stringsAsFactors = FALSE
                ))
            }
        }, error = function(e) {
            # Error handling
            cat(sprintf("Error fetching lyrics for %s by %s: %s\n", 
                        song, artist, e$message))
            
            lyricsData <- rbind(lyricsData, data.frame(
                SongID = songData$SongID[i],
                SongName = song,
                ArtistName = artist,
                MajorityGenre = songData$MajorityGenre[i],
                MinorityGenre = songData$MinorityGenre[i],
                LyricsStatus = "error",
                Lyrics = "Error retrieving lyrics",
                stringsAsFactors = FALSE
            ))
        })
        
        # Rate limiting - pause between API calls to avoid hitting limits since the API is dog shit
        Sys.sleep(1)
    }
    
    # Save the lyrics data
    write.csv(lyricsData, file.path(OUTPUT_DIR, "lyrics_data.csv"), row.names = FALSE)
    return(lyricsData)
}

#' Collect YouTube comments
#'
#' Fetches comments for each song with a YouTube link, sorted by likes.
#' Hope you're not hitting rate limits!
#'
#' @param songData data.frame containing song information
#' @param maxComments Integer maximum number of comments to collect per song
#' @return data.frame with comment information, with columns:
#'   SongID, VideoID, CommentID, Author, Comment, LikeCount, PublishedAt
CollectYouTubeComments <- function(songData, maxComments = PROJECT_SETTINGS$max_comments) {
    # Create dataframe to store comment data
    commentData <- data.frame(
        SongID = integer(),
        VideoID = character(),
        CommentID = character(),
        Author = character(),
        Comment = character(),
        LikeCount = integer(),
        PublishedAt = character(),
        stringsAsFactors = FALSE
    )
    
    # Check if tuber is loaded and authenticated
    if (!requireNamespace("tuber", quietly = TRUE)) {
        warning("Package 'tuber' is not available. Can't collect comments. We're screwed.")
        return(commentData)
    }
    
    # Process each song with a YouTube link
    for (i in 1:nrow(songData)) {
        # Extract YouTube video ID
        videoUrl <- songData$SongLink[i]
        videoId <- ExtractYouTubeID(videoUrl)
        
        if (is.na(videoId)) {
            cat(sprintf("Skipping song %d: No valid YouTube URL. Moving on.\n", i))
            next
        }
        
        cat(sprintf("Fetching comments for video %s (%d of %d)\n", 
                    videoId, i, nrow(songData)))
        
        # Create file to store comments for this song
        commentFilePath <- file.path(COMMENTS_DIR, paste0(songData$SongID[i], "_comments.csv"))
        
        # Try to get comments
        tryCatch({
            # Get comments
            comments <- get_comment_threads(
                video_id = videoId,
                max_results = maxComments
            )
            
            if (!is.null(comments) && nrow(comments) > 0) {
                # Extract relevant fields
                songComments <- data.frame(
                    SongID = songData$SongID[i],
                    VideoID = videoId,
                    CommentID = comments$id,
                    Author = comments$authorDisplayName,
                    Comment = comments$textDisplay,
                    LikeCount = as.integer(comments$likeCount),
                    PublishedAt = comments$publishedAt,
                    stringsAsFactors = FALSE
                )
                
                # Sort by like count and take top comments
                songComments <- songComments %>%
                    arrange(desc(LikeCount)) %>%
                    head(maxComments)
                
                # Save to file
                write.csv(songComments, commentFilePath, row.names = FALSE)
                
                # Add to main dataframe
                commentData <- rbind(commentData, songComments)
                
                cat(sprintf("Saved %d comments for video %s\n", 
                            nrow(songComments), videoId))
            } else {
                cat(sprintf("No comments found for video %s. Lame.\n", videoId))
            }
        }, error = function(e) {
            cat(sprintf("Error fetching comments for video %s: %s\n", 
                        videoId, e$message))
        })
        
        # Rate limiting - pause between API calls. You get it. No money!
        Sys.sleep(2)
    }
    # Save all comments
    write.csv(commentData, file.path(OUTPUT_DIR, "all_comments.csv"), row.names = FALSE)
    return(commentData)
}