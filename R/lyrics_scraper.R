# Direct lyrics scraping function using Genius search


#' Scrape lyrics directly from Genius website
#'
#' @param artist Artist name
#' @param title Song title
#' @return Character string of lyrics or NA if not found
ScrapeLyrics <- function(artist, title) {
    library(rvest)
    library(httr)
    library(dplyr)
    
    # Clean artist and title for search
    artist <- gsub("\\(.*?\\)", "", artist)  # Remove stuff in parentheses
    artist <- trimws(artist)
    title <- trimws(title)
    
    # Create a URL for direct searching on Genius
    search_term <- paste(artist, title)
    base_url <- "https://genius.com/api/search/song"
    
    tryCatch({
        # Make a search request to Genius API (this is the public web API, not requiring auth, this API is trash asf)
        response <- GET(
            url = base_url,
            query = list(
                q = search_term,
                per_page = 5
            )
        )
        
        # Check if the request was successful
        if (status_code(response) != 200) {
            cat("Search request failed for", title, "by", artist, "\n")
            return(NA)
        }
        
        # Parse the JSON response
        search_data <- content(response, "parsed")
        
        # Check if we have results
        if (length(search_data$response$sections) == 0 || 
            length(search_data$response$sections[[1]]$hits) == 0) {
            cat("No search results found for", title, "by", artist, "\n")
            return(NA)
        }
        
        # Get the URL of the first result
        lyrics_url <- search_data$response$sections[[1]]$hits[[1]]$result$url
        
        # If couldn't get a URL, try again with a simpler search
        if (is.null(lyrics_url)) {
            # Try with just the title
            response <- GET(
                url = base_url,
                query = list(
                    q = title,
                    per_page = 5
                )
            )
            
            search_data <- content(response, "parsed")
            
            if (length(search_data$response$sections) == 0 || 
                length(search_data$response$sections[[1]]$hits) == 0) {
                cat("No search results found with simplified search for", title, "\n")
                return(NA)
            }
            
            lyrics_url <- search_data$response$sections[[1]]$hits[[1]]$result$url
            
            if (is.null(lyrics_url)) {
                cat("Could not find lyrics URL for", title, "\n")
                return(NA)
            }
        }
        
        cat("Found lyrics URL:", lyrics_url, "\n")
        
        # Visit the lyrics page
        lyrics_page <- read_html(lyrics_url)
        
        # Try other selectors that Genius has use
        selectors <- c(
            '[data-lyrics-container="true"]',  # Current Genius layout
            '.lyrics',                         # Old Genius layout
            '.song_body-lyrics',               # Another variant
            '#lyrics-root-pin'                 # Another possible container
        )
        
        lyrics_text <- ""
        
        for (selector in selectors) {
            lyrics_divs <- lyrics_page %>% html_nodes(selector)
            
            if (length(lyrics_divs) > 0) {
                # Found matching elements, extract the text
                extracted_text <- paste(html_text(lyrics_divs), collapse = "\n")
                
                if (nchar(extracted_text) > 0) {
                    lyrics_text <- extracted_text
                    break
                }
            }
        }
        
        if (lyrics_text == "") {
            # If all selectors fail, try to get all text from the page and extract lyrics
            page_text <- lyrics_page %>% html_text()
            
            # Try to extract lyrics section - fallback!!!!!!!!!
            # Find the lyrics by looking for common patterns
            lyrics_section <- regmatches(
                page_text,
                regexpr("Lyrics\\s*[\\s\\S]*?(?=Related Artists|About|Comments)", page_text)
            )
            
            if (length(lyrics_section) > 0) {
                lyrics_text <- lyrics_section[1]
            } else {
                cat("Could not extract lyrics for", title, "by", artist, "\n")
                return(NA)
            }
        }
        
        # Clean up lyrics
        lyrics_text <- gsub("\n+", "\n", lyrics_text)  # Remove extra line breaks
        lyrics_text <- gsub("\\[.*?\\]", "", lyrics_text)  # Remove Verse, Chorus, etc
        lyrics_text <- trimws(lyrics_text)  # Remove leading / trailing whitespace
        return(lyrics_text)
        
    }, error = function(e) {
        cat("Error scraping lyrics for", title, "by", artist, ":", e$message, "\n")
        return(NA)
    })
}

#' Another lyrics scraper using LyricsGenius package <---- This is much better
#'
#' @param artist Artist name
#' @param title Song title
#' @return Character string of lyrics or NA if not found
ScrapeLyricsViaLyricsGenius <- function(artist, title) {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
        cat("Package 'reticulate' isn't installed. Installing now...\n")
        install.packages("reticulate")
    }
    
    library(reticulate)
    
    tryCatch({
        # Skip installation checks and just try to import the module
        cat("Checking for lyricsgenius...\n")
        if (!py_module_available("lyricsgenius")) {
            cat("Python module 'lyricsgenius' not found despite manual installation.\n")
            cat("Please install it via: pip install lyricsgenius\n")
            return(NA)
        }
        
        # Import the package
        lyricsgenius <- import("lyricsgenius")
        
        # Create Genius API object
        genius <- lyricsgenius$Genius(API_CREDENTIALS$genius$client_id)
        
        # Search for the song
        song <- genius$search_song(title, artist)
        
        if (is.null(song)) {
            cat("No song found for", title, "by", artist, "via lyricsgenius\n")
            return(NA)
        }
        
        # Extract lyrics
        lyrics_text <- song$lyrics
        
        # Clean up lyrics
        lyrics_text <- gsub("\n+", "\n", lyrics_text)  # Remove extra line breaks
        lyrics_text <- gsub("\\[.*?\\]", "", lyrics_text)  # Remove [Verse], [Chorus], etc.
        lyrics_text <- trimws(lyrics_text)  # Remove leading/trailing whitespace
        return(lyrics_text)
        
    }, error = function(e) {
        cat("Error using lyricsgenius for", title, "by", artist, ":", e$message, "\n")
        return(NA)
    })
}

#' Collect lyrics by trying multiple methods
#'
#' @param songData Data frame with song information
#' @return Data frame with song info and lyrics
CollectLyricsDirectly <- function(songData) {
    # Try to install necessary Python package for alternative method
    tryCatch({
        if (!requireNamespace("reticulate", quietly = TRUE)) {
            install.packages("reticulate")
        }
    }, error = function(e) {
        cat("Cannot install reticulate package for Python integration:", e$message, "\n")
    })
    
    python_available <- tryCatch({
        reticulate::use_python(Sys.which("python"), required = TRUE)
        TRUE
    }, error = function(e) {
        FALSE
    })
    
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
        
        cat(sprintf("Fetching lyrics for: %s by %s (%d of %d)\n", 
                    song, artist, i, nrow(songData)))
        
        # First try Direct web scraping
        lyrics_text <- ScrapeLyrics(artist, song)
        
        # If failed and Python is available, try
        if ((is.na(lyrics_text) || nchar(lyrics_text) < 10) && python_available) {
            cat("Trying alternative method (lyricsgenius)...\n")
            lyrics_text <- ScrapeLyricsViaLyricsGenius(artist, song)
        }
        
        # If we found lyrics withe either 1
        if (!is.na(lyrics_text) && nchar(lyrics_text) > 10) {
            # Save
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
            
            cat(sprintf("Success! Saved lyrics for %s\n", song))
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
            
            cat(sprintf("Failed to find lyrics for %s\n", song))
        }
        
        # Pause to avoid overloading
        Sys.sleep(2)
    }
    # Save
    write.csv(lyricsData, file.path(OUTPUT_DIR, "lyrics_data.csv"), row.names = FALSE)
    return(lyricsData)
}

#' Manual lyrics collection function (FOR TESTING)
#'
#' @param songData Data frame with song information  
#' @return Data frame with song info and lyrics
AddManualLyrics <- function(songData) {
    # sample lyrics for testing
    sample_lyrics <- list(
        "Pop" = "OoooOOOOOooOoOOOo, I hate doing this project",
        
        "Rock" = "I can't believe I am saying this, but trust me, I bet you don't know I type these.",
        
        "Electronic" = "Better give us good marks.",
        
        "Hip-hop" = "Why tf we vote for music, not stock, this is much more painful then I except.",
        
        "Disco" = "I am so fuxking done..., error and error and fking error"
    )
    
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
        genre <- songData$MajorityGenre[i]
        
        # Find the closest matching genre in our samples
        matching_genre <- names(sample_lyrics)[1]  # Default to first genre
        for (g in names(sample_lyrics)) {
            if (grepl(g, genre, ignore.case = TRUE)) {
                matching_genre <- g
                break
            }
        }
        
        # Use the sample lyrics for this genre
        lyrics_text <- sample_lyrics[[matching_genre]]
        
        # Add some randomness by shuffling verses
        lines <- unlist(strsplit(lyrics_text, "\n"))
        if (i %% 2 == 0 && length(lines) > 2) {  # For every other song, if possible
            lines <- c(lines[2:length(lines)], lines[1])
        }
        lyrics_text <- paste(lines, collapse = "\n")
        
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
        
        cat(sprintf("Added sample lyrics for %s by %s\n", song, artist))
    }
    # Save the lyrics data
    write.csv(lyricsData, file.path(OUTPUT_DIR, "lyrics_data.csv"), row.names = FALSE)
    return(lyricsData)
}