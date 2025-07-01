# Sentiment analysis functions - where the magic happens


#' Analyze sentiment of lyrics using NRC lexicon
#'
#' Calculate sentiment scores for lyrics text
#' This is the heart of the project - don't fuck it up
#'
#' @param lyrics_tokens data.frame with processed lyrics tokens
#' @param lexicon character name of sentiment lexicon to use
#' @return data.frame with sentiment scores
AnalyzeLyricsSentiment <- function(lyrics_tokens, lexicon = PROJECT_SETTINGS$sentiment_lexicon) {
    # Load sentiment lexicon
    if (lexicon == "bing") { # Bing is the first choice
        sentiment_lex <- get_sentiments("bing")
    } else if (lexicon == "nrc") {
        sentiment_lex <- get_sentiments("nrc")
    } else if (lexicon == "afinn") {
        sentiment_lex <- get_sentiments("afinn")
    } else if (lexicon == "loughran") {
        sentiment_lex <- get_sentiments("loughran")
    } else {
        stop("Invalid lexicon specified. WTF are you using?")
    }
    
    # Join tokens with sentiment lexicon
    lyrics_sentiment <- lyrics_tokens %>%
        inner_join(sentiment_lex, by = "word")
    
    # Count sentiment instances per song
    if (lexicon == "nrc") {
        # For NRC, count by sentiment categories
        song_sentiment <- lyrics_sentiment %>%
            filter(sentiment != "positive" & sentiment != "negative") %>% # Focus on emotions
            count(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, sentiment) %>%
            spread(sentiment, n, fill = 0)
        
        # Calculate positive/negative sentiment separately
        pos_neg <- lyrics_sentiment %>%
            filter(sentiment %in% c("positive", "negative")) %>%
            count(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, sentiment) %>%
            spread(sentiment, n, fill = 0)
        
        # Make sure positive and negative columns exist
        if (!"positive" %in% colnames(pos_neg)) {
            pos_neg$positive <- 0
        }
        if (!"negative" %in% colnames(pos_neg)) {
            pos_neg$negative <- 0
        }
        
        # Join emotion and pos/neg sentiment
        song_sentiment <- song_sentiment %>%
            left_join(pos_neg, by = c("SongID", "SongName", "ArtistName", "MajorityGenre", "MinorityGenre"))
        
        # Calculate total words and sentiment proportions
        total_words <- lyrics_tokens %>%
            group_by(SongID) %>%
            summarize(total_words = n())
        
        song_sentiment <- song_sentiment %>%
            left_join(total_words, by = "SongID") %>%
            mutate(
                sentiment_words = positive + negative,
                positivity_ratio = positive / (positive + negative),
                sentiment_density = sentiment_words / total_words
            )
        
    } else if (lexicon == "bing") {
        # For bing, count positive and negative words
        song_sentiment <- lyrics_sentiment %>%
            count(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, sentiment) %>%
            spread(sentiment, n, fill = 0)
        
        # Make sure positive and negative columns exist
        if (!"positive" %in% colnames(song_sentiment)) {
            song_sentiment$positive <- 0
        }
        if (!"negative" %in% colnames(song_sentiment)) {
            song_sentiment$negative <- 0
        }
        
        song_sentiment <- song_sentiment %>%
            mutate(
                sentiment_words = positive + negative,
                positivity_ratio = ifelse(positive + negative > 0, 
                                          positive / (positive + negative), 0.5)
            )
        
        # Calculate total words and sentiment density
        total_words <- lyrics_tokens %>%
            group_by(SongID) %>%
            summarize(total_words = n())
        
        song_sentiment <- song_sentiment %>%
            left_join(total_words, by = "SongID") %>%
            mutate(sentiment_density = sentiment_words / total_words)
        
    } else if (lexicon == "afinn") {
        # For AFINN, calculate average sentiment score
        song_sentiment <- lyrics_sentiment %>%
            group_by(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre) %>%
            summarize(
                sentiment_sum = sum(value),
                sentiment_words = n(),
                avg_sentiment = sentiment_sum / sentiment_words,
                .groups = 'drop'
            )
        
        # Calculate total words and sentiment density
        total_words <- lyrics_tokens %>%
            group_by(SongID) %>%
            summarize(total_words = n())
        
        song_sentiment <- song_sentiment %>%
            left_join(total_words, by = "SongID") %>%
            mutate(sentiment_density = sentiment_words / total_words)
        
    } else if (lexicon == "loughran") {
        # For Loughran, count by financial sentiment categories
        song_sentiment <- lyrics_sentiment %>%
            count(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, sentiment) %>%
            spread(sentiment, n, fill = 0)
        
        # Calculate total sentiment words
        sentiment_cols <- setdiff(colnames(song_sentiment), 
                                  c("SongID", "SongName", "ArtistName", "MajorityGenre", "MinorityGenre"))
        song_sentiment$sentiment_words <- rowSums(song_sentiment[, sentiment_cols])
        
        # Calculate total words and sentiment density
        total_words <- lyrics_tokens %>%
            group_by(SongID) %>%
            summarize(total_words = n())
        
        song_sentiment <- song_sentiment %>%
            left_join(total_words, by = "SongID") %>%
            mutate(sentiment_density = sentiment_words / total_words)
    }
    
    # Save result
    write.csv(lyrics_sentiment, file.path(OUTPUT_DIR, "lyrics_sentiment_words.csv"), row.names = FALSE)
    write.csv(song_sentiment, file.path(OUTPUT_DIR, "song_sentiment_scores.csv"), row.names = FALSE)
    
    return(list(
        word_sentiment = lyrics_sentiment,
        song_sentiment = song_sentiment
    ))
}

#' Analyze sentiment of comments using NRC lexicon
#'
#' Calculate sentiment scores for YouTube comments
#' People say some wild shit in YouTube comments
#'
#' @param comment_tokens data.frame with processed comment tokens
#' @param lexicon character name of sentiment lexicon to use
#' @return data.frame with sentiment scores
AnalyzeCommentsSentiment <- function(comment_tokens, lexicon = PROJECT_SETTINGS$sentiment_lexicon) {
    # Check if comment data exist
    if (is.null(comment_tokens) || nrow(comment_tokens) == 0) {
        warning("No comment data available for sentiment analysis. Well, that's a bummer.")
        return(NULL)
    }
    
    # Load sentiment lexicon
    if (lexicon == "nrc") {
        sentiment_lex <- get_sentiments("nrc")
    } else if (lexicon == "bing") {
        sentiment_lex <- get_sentiments("bing")
    } else if (lexicon == "afinn") {
        sentiment_lex <- get_sentiments("afinn")
    } else if (lexicon == "loughran") {
        sentiment_lex <- get_sentiments("loughran")
    } else {
        stop("Invalid lexicon specified. Use 'nrc', 'bing', 'afinn', or 'loughran'. Don't get creative here.")
    }
    
    # Join tokens with sentiment lexicon
    comment_sentiment <- comment_tokens %>%
        inner_join(sentiment_lex, by = "word")
    
    # Analyze sentiment per comment
    if (lexicon == "nrc") {
        # For NRC, count by sentiment categories
        comment_emotions <- comment_sentiment %>%
            filter(sentiment != "positive" & sentiment != "negative") %>%
            count(SongID, CommentID, sentiment) %>%
            spread(sentiment, n, fill = 0)
        
        # Calculate positive/negative sentiment
        pos_neg <- comment_sentiment %>%
            filter(sentiment %in% c("positive", "negative")) %>%
            count(SongID, CommentID, sentiment) %>%
            spread(sentiment, n, fill = 0)
        
        # Join emotion and pos/neg sentiment
        comment_scores <- comment_emotions %>%
            left_join(pos_neg, by = c("SongID", "CommentID"))
        
        # Calculate total words and sentiment proportions
        total_words <- comment_tokens %>%
            group_by(SongID, CommentID) %>%
            summarize(total_words = n())
        
        comment_scores <- comment_scores %>%
            left_join(total_words, by = c("SongID", "CommentID")) %>%
            mutate(
                sentiment_words = positive + negative,
                positivity_ratio = ifelse(positive + negative > 0, positive / (positive + negative), 0.5),
                sentiment_density = sentiment_words / total_words
            )
        
    } else if (lexicon == "bing") {
        # For bing, count positive and negative words
        comment_scores <- comment_sentiment %>%
            count(SongID, CommentID, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(
                sentiment_words = positive + negative,
                positivity_ratio = ifelse(positive + negative > 0, positive / (positive + negative), 0.5)
            )
        
        # Calculate total words and sentiment density
        total_words <- comment_tokens %>%
            group_by(SongID, CommentID) %>%
            summarize(total_words = n())
        
        comment_scores <- comment_scores %>%
            left_join(total_words, by = c("SongID", "CommentID")) %>%
            mutate(sentiment_density = sentiment_words / total_words)
        
    } else if (lexicon == "afinn") {
        # For AFINN, calculate average sentiment score
        comment_scores <- comment_sentiment %>%
            group_by(SongID, CommentID) %>%
            summarize(
                sentiment_sum = sum(value),
                sentiment_words = n(),
                avg_sentiment = sentiment_sum / sentiment_words
            )
        
        # Calculate total words and sentiment density
        total_words <- comment_tokens %>%
            group_by(SongID, CommentID) %>%
            summarize(total_words = n())
        
        comment_scores <- comment_scores %>%
            left_join(total_words, by = c("SongID", "CommentID")) %>%
            mutate(sentiment_density = sentiment_words / total_words)
    }
    
    # Aggregate sentiment by song
    song_sentiment <- comment_scores %>%
        group_by(SongID) %>%
        summarize(across(where(is.numeric), mean, na.rm = TRUE))
    
    # Save result
    write.csv(comment_sentiment, file.path(OUTPUT_DIR, "comment_sentiment_words.csv"), row.names = FALSE)
    write.csv(comment_scores, file.path(OUTPUT_DIR, "comment_sentiment_scores.csv"), row.names = FALSE)
    write.csv(song_sentiment, file.path(OUTPUT_DIR, "song_comment_sentiment.csv"), row.names = FALSE)
    
    return(list(
        word_sentiment = comment_sentiment,
        comment_sentiment = comment_scores,
        song_sentiment = song_sentiment
    ))
}

#' Compare sentiment across genres
#'
#' Aggregates sentiment scores by genre
#' See which genres are happy and which ones are depressing as hell
#'
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @return data.frame with genre sentiment scores
CompareSentimentByGenre <- function(lyrics_sentiment) {
    # Extract song sentiment
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Aggregate by genre
    genre_sentiment <- song_sentiment %>%
        group_by(MajorityGenre) %>%
        summarize(across(where(is.numeric), mean, na.rm = TRUE))
    
    # Save result
    write.csv(genre_sentiment, file.path(OUTPUT_DIR, "genre_sentiment.csv"), row.names = FALSE)
    
    return(genre_sentiment)
}

#' Compare lyrics and comments sentiment
#'
#' Compares sentiment in lyrics vs YouTube comments
#' Do listeners feel what the artist intended? Let's find out.
#'
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @param comments_sentiment result from AnalyzeCommentsSentiment()
#' @return data.frame with comparison
CompareLyricsAndComments <- function(lyrics_sentiment, comments_sentiment) {
    # Check if comment data exists
    if (is.null(comments_sentiment)) {
        warning("No comment sentiment data available for comparison. Can't do shit with nothing.")
        return(NULL)
    }
    
    # Extract song-level sentiment
    lyrics_scores <- lyrics_sentiment$song_sentiment
    comment_scores <- comments_sentiment$song_sentiment
    
    # Join dataset
    comparison <- lyrics_scores %>%
        select(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, 
               positive, negative, positivity_ratio) %>%
        rename(
            lyrics_positive = positive,
            lyrics_negative = negative,
            lyrics_positivity = positivity_ratio
        ) %>%
        left_join(
            select(comment_scores, SongID, positive, negative, positivity_ratio),
            by = "SongID"
        ) %>%
        rename(
            comments_positive = positive,
            comments_negative = negative,
            comments_positivity = positivity_ratio
        ) %>%
        mutate(
            sentiment_diff = comments_positivity - lyrics_positivity
        )
    # Save result
    write.csv(comparison, file.path(OUTPUT_DIR, "lyrics_comments_comparison.csv"), row.names = FALSE)
    return(comparison)
}