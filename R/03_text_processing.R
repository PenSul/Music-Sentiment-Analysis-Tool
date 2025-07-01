# Text processing functions - where we clean up this mess


#' Processes raw lyrics text
#'
#' @param lyricsData data.frame containing lyrics information
#' @param stopwords character vector of stopwords to remove
#' @return data.frame with processed tokens
ProcessLyrics <- function(lyricsData, stopwords = GetStopwords()) {
    # Filter out songs with no lyrics or error status
    lyricsData <- lyricsData %>%
        filter(LyricsStatus == "found")
    
    # Tokenize the lyrics (split into words)
    lyrics_tokens <- lyricsData %>%
        select(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, Lyrics) %>%
        unnest_tokens(word, Lyrics)
    
    # Clean tokens: remove stopwords, numbers, short words
    cleaned_tokens <- lyrics_tokens %>%
        anti_join(data.frame(word = stopwords), by = "word") %>%
        filter(!grepl("^\\d+$", word)) %>%      # Remove numbers
        filter(nchar(word) > 2) %>%             # Remove short words - because "a" & "an" are useless
        filter(!grepl("[^a-zA-Z]", word))       # Keep only alphabetic words
    
    # Count word frequencies
    word_counts <- cleaned_tokens %>%
        count(word, sort = TRUE)
    
    # Save processed tokens to file
    write.csv(cleaned_tokens, file.path(OUTPUT_DIR, "processed_lyrics.csv"), row.names = FALSE)
    write.csv(word_counts, file.path(OUTPUT_DIR, "lyrics_word_counts.csv"), row.names = FALSE)
    
    return(cleaned_tokens)
}

#' Clean and tokenize comments text
#'
#' Processes raw comments text
#' YouTube comments are a cesspool, but we'll clean them up
#'
#' @param commentsData data.frame containing comments information
#' @param stopwords character vector of stopwords to remove
#' @return data.frame with processed tokens
ProcessComments <- function(commentsData, stopwords = GetStopwords()) {
    # Check if comments data exists
    if (is.null(commentsData) || nrow(commentsData) == 0) {
        warning("No comments data available for processing. That sucks.")
        return(NULL)
    }
    
    # Remove HTML tags from comments
    commentsData$CleanComment <- gsub("<.*?>", "", commentsData$Comment)
    
    # Tokenize the comments (split into words)
    comment_tokens <- commentsData %>%
        select(SongID, VideoID, CommentID, Author, CleanComment, LikeCount) %>%
        unnest_tokens(word, CleanComment)
    
    # Clean tokens: remove stopwords, numbers, short words
    cleaned_tokens <- comment_tokens %>%
        anti_join(data.frame(word = stopwords), by = "word") %>%
        filter(!grepl("^\\d+$", word)) %>%      # Remove numbers
        filter(nchar(word) > 2) %>%             # Remove short words
        filter(!grepl("[^a-zA-Z]", word))       # Keep only alphabetic words
    
    # Count word frequencies
    word_counts <- cleaned_tokens %>%
        count(word, sort = TRUE)
    
    # Save processed tokens to file
    write.csv(cleaned_tokens, file.path(OUTPUT_DIR, "processed_comments.csv"), row.names = FALSE)
    write.csv(word_counts, file.path(OUTPUT_DIR, "comments_word_counts.csv"), row.names = FALSE)
    
    return(cleaned_tokens)
}

#' Calculate Term Frequency-Inverse Document Frequency to identify important words by genre
#'
#' @param lyrics_tokens data.frame with processed tokens
#' @return data.frame with TF-IDF scores
CalculateGenreTFIDF <- function(lyrics_tokens) {
    # Count words by genre
    genre_words <- lyrics_tokens %>%
        count(MajorityGenre, word, sort = TRUE)
    
    # Calculate total words per genre
    total_words <- genre_words %>%
        group_by(MajorityGenre) %>%
        summarize(total = sum(n))
    
    # Join with word counts to get frequency
    genre_words <- left_join(genre_words, total_words)
    
    # Calculate TF-IDF
    genre_tf_idf <- genre_words %>%
        bind_tf_idf(word, MajorityGenre, n)
    
    # Save TF-IDF scores
    write.csv(genre_tf_idf, file.path(OUTPUT_DIR, "genre_tf_idf.csv"), row.names = FALSE)
    
    return(genre_tf_idf)
}

#' Extract n-grams from lyrics
#'
#' Creates bigrams or trigrams from lyrics text.
#' Because sometimes one word isn't enough to get the real meaning
#'
#' @param lyricsData data.frame containing lyrics information
#' @param n integer: 2 for bigrams, 3 for trigrams
#' @param stopwords character vector of stopwords to remove
#' @return data.frame with n-grams
ExtractNgrams <- function(lyricsData, n = 2, stopwords = GetStopwords()) {
    # Filter out songs with no lyrics or error status
    lyricsData <- lyricsData %>%
        filter(LyricsStatus == "found")
    
    # Extract n-grams
    if (n == 2) {
        ngrams <- lyricsData %>%
            select(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, Lyrics) %>%
            unnest_tokens(bigram, Lyrics, token = "ngrams", n = 2)
        
        # Separate bigrams into words
        bigrams_separated <- ngrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        # Remove stopwords from both words. I can use for loop but I am lazy.
        bigrams_filtered <- bigrams_separated %>%
            filter(!word1 %in% stopwords) %>%
            filter(!word2 %in% stopwords) %>%
            
            filter(nchar(word1) > 2) %>%             # Remove short words
            filter(nchar(word2) > 2) %>%             
            filter(!grepl("^\\d+$", word1)) %>%      # Remove numbers
            filter(!grepl("^\\d+$", word2))          
        
        # Unite the words back into bigrams
        bigrams_united <- bigrams_filtered %>%
            unite(bigram, word1, word2, sep = " ")
        
        # Count bigram frequencies
        bigram_counts <- bigrams_united %>%
            count(bigram, sort = TRUE)
        
        # Save to file
        write.csv(bigrams_united, file.path(OUTPUT_DIR, "lyrics_bigrams.csv"), row.names = FALSE)
        write.csv(bigram_counts, file.path(OUTPUT_DIR, "lyrics_bigram_counts.csv"), row.names = FALSE)
        
        return(bigrams_united)
        
    } else if (n == 3) {
        ngrams <- lyricsData %>%
            select(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, Lyrics) %>%
            unnest_tokens(trigram, Lyrics, token = "ngrams", n = 3)
        
        # Separate trigrams into words
        trigrams_separated <- ngrams %>%
            separate(trigram, c("word1", "word2", "word3"), sep = " ")
        
        # Remove stopwords from all words. Same here like above.
        trigrams_filtered <- trigrams_separated %>%
            filter(!word1 %in% stopwords) %>%
            filter(!word2 %in% stopwords) %>%
            filter(!word3 %in% stopwords) %>%
            filter(nchar(word1) > 2) %>%             # Remove short words
            filter(nchar(word2) > 2) %>%             
            filter(nchar(word3) > 2) %>%             
            filter(!grepl("^\\d+$", word1)) %>%      # Remove numbers
            filter(!grepl("^\\d+$", word2)) %>%      
            filter(!grepl("^\\d+$", word3))          
        
        # Unite the words back into trigrams
        trigrams_united <- trigrams_filtered %>%
            unite(trigram, word1, word2, word3, sep = " ")
        
        # Count trigram frequencies
        trigram_counts <- trigrams_united %>%
            count(trigram, sort = TRUE)
        
        # Save to file
        write.csv(trigrams_united, file.path(OUTPUT_DIR, "lyrics_trigrams.csv"), row.names = FALSE)
        write.csv(trigram_counts, file.path(OUTPUT_DIR, "lyrics_trigram_counts.csv"), row.names = FALSE)
        
        return(trigrams_united)
    } else {
        stop("n must be 2 or 3. What the hell are you trying to do?")
    }
}