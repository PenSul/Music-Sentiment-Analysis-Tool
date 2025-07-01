# YouTube comment collection using API key directly


#' Collect YouTube comments using httr direct API calls
#'
#' @param songData Data frame with song information
#' @param max_results Maximum number of comments to fetch per video
#' @return Data frame with comment data
CollectYouTubeCommentsWithAPIKey <- function(songData, max_results = 100) { # Need to be change before push
    library(httr)
    library(jsonlite)
    
    # Get API key
    api_key <- API_CREDENTIALS$youtube$api_key
    
    if (is.null(api_key) || api_key == "") {
        stop("YouTube API key not found. We're screwed.")
    }
    
    # Create dataframe to store comments
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
    
    # Extract YouTube IDs from song links
    extractYouTubeID <- function(url) {
        if (is.na(url) || url == "") {
            return(NA)
        }
        
        if (grepl("youtube\\.com/watch\\?v=", url)) {
            return(gsub(".*v=([^&]+).*", "\\1", url))
        } else if (grepl("youtu\\.be/", url)) {
            return(gsub(".*youtu\\.be/([^?&]+).*", "\\1", url))
        } else {
            return(NA)
        }
    }
    
    # Process each song
    for (i in 1:nrow(songData)) {
        # Get video ID
        video_id <- extractYouTubeID(songData$SongLink[i])
        
        if (is.na(video_id)) {
            cat(sprintf("Skipping song %d: No valid YouTube URL. Meh, whatever.\n", i))
            next
        }
        
        cat(sprintf("Fetching comments for video %s (%d of %d)\n", 
                    video_id, i, nrow(songData)))
        
        # Set up API request
        url <- "https://www.googleapis.com/youtube/v3/commentThreads"
        
        # Make the request
        tryCatch({
            response <- GET(
                url = url,
                query = list(
                    part = "snippet",
                    videoId = video_id,
                    maxResults = max_results,
                    key = api_key,
                    textFormat = "plainText"
                )
            )
            
            # Check if request was successful
            if (http_error(response)) {
                error_content <- content(response, "parsed")
                cat(sprintf("API error: %s\n", error_content$error$message))
                next
            }
            
            # Parse the response
            content <- content(response, "text", encoding = "UTF-8")
            data <- fromJSON(content)
            
            # Check if we have comments
            if (!is.null(data$items) && length(data$items) > 0) {
                # Extract comment data
                items <- data$items
                
                # Create data frame for this video's comments
                video_comments <- data.frame(
                    SongID = songData$SongID[i],
                    VideoID = video_id,
                    CommentID = items$id,
                    Author = items$snippet$topLevelComment$snippet$authorDisplayName,
                    Comment = items$snippet$topLevelComment$snippet$textDisplay,
                    LikeCount = as.integer(items$snippet$topLevelComment$snippet$likeCount),
                    PublishedAt = items$snippet$topLevelComment$snippet$publishedAt,
                    stringsAsFactors = FALSE
                )
                
                # Save
                file_path <- file.path(COMMENTS_DIR, paste0(songData$SongID[i], "_comments.csv"))
                write.csv(video_comments, file_path, row.names = FALSE)
                
                # Add to master comments data frame
                commentData <- rbind(commentData, video_comments)
                
                cat(sprintf("  Saved %d comments for video %s\n", 
                            nrow(video_comments), video_id))
            } else {
                cat(sprintf("  No comments found for video %s. Boring video?\n", video_id))
            }
            
        }, error = function(e) {
            cat(sprintf("Error fetching comments for video %s: %s\n", 
                        video_id, e$message))
        })
        
        # pause between API calls
        Sys.sleep(2)
    }
    # Save comments
    write.csv(commentData, file.path(OUTPUT_DIR, "all_comments.csv"), row.names = FALSE)
    return(commentData)
}