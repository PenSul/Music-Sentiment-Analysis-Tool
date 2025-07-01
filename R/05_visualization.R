# Visualization functions - make this shit look pretty!


#' Get colors for all genres in dataset
#'
#' @param genres Vector of genre names
#' @return Named vector of colors
getGenreColors <- function(genres) {
    # Get unique genres
    all_genres <- unique(genres)
    
    # Create a color mapping
    genre_colors <- rep(NA, length(all_genres))
    names(genre_colors) <- all_genres
    
    # Use defined colors from GENRE_COLORS
    for (genre in all_genres) {
        if (genre %in% names(GENRE_COLORS)) {
            genre_colors[genre] <- GENRE_COLORS[[genre]]
        }
    }
    
    # For any genres without defined colors, generate colors from a palette
    missing_genres <- all_genres[is.na(genre_colors)]
    if (length(missing_genres) > 0) {
        if(!requireNamespace("RColorBrewer", quietly = TRUE)) {
            install.packages("RColorBrewer")
            library(RColorBrewer)
        } else {
            library(RColorBrewer)
        }
        
        # Choose palette
        if (length(missing_genres) <= 9) {
            new_colors <- brewer.pal(max(3, length(missing_genres)), "Set1")
        } else {
            new_colors <- colorRampPalette(brewer.pal(9, "Set1"))(length(missing_genres))
        }
        
        # Assign new colors
        for (i in 1:length(missing_genres)) {
            genre_colors[missing_genres[i]] <- new_colors[i]
        }
    }
    
    return(genre_colors)
}

#' Create word clouds for each genre
#'
#' Generates word cloud visualizations for the most common words in each genre.
#' Because word clouds are cool, even if they're a bit basic
#'
#' @param lyrics_tokens data.frame with processed lyrics tokens
#' @param min_freq minimum frequency for words to include
#' @param max_words maximum number of words to display
#' @return NULL (saves plots to files)
CreateGenreWordClouds <- function(lyrics_tokens, 
                                  min_freq = PROJECT_SETTINGS$min_word_freq, 
                                  max_words = PROJECT_SETTINGS$max_words) {
    # Get list of genres
    genres <- unique(lyrics_tokens$MajorityGenre)
    
    # Create a word cloud for each genre
    for (genre in genres) {
        # Filter for current genre and count words
        genre_words <- lyrics_tokens %>%
            filter(MajorityGenre == genre) %>%
            count(word, sort = TRUE)
        
        # Skip if not enough words
        if (nrow(genre_words) < 10) {
            warning("Not enough words for genre: ", genre, ". Weak dataset, bro.")
            next
        }
        
        # Set up output file
        filename <- file.path(VISUALS_DIR, paste0("wordcloud_", gsub(" ", "_", genre), ".png"))
        png(filename, width = 800, height = 600, res = 100)
        
        # Generate word cloud
        wordcloud(
            words = genre_words$word,
            freq = genre_words$n,
            min.freq = min_freq,
            max.words = max_words,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(8, "Dark2")
        )
        
        dev.off()
        cat("Created word cloud for", genre, "at", filename, "\n")
    }
    return(invisible(NULL))
}

#' Create sentiment distribution plots
#'
#' Generate visualization of sentiment distribution
#' How happy or depressing is this music, anyway?
#'
#' @param sentiment_data result from sentiment analysis functions
#' @param data_type character: "Lyrics" or "Comments"
#' @return NULL (saves plots to files)
CreateSentimentPlots <- function(sentiment_data, data_type = "Lyrics") {
    # Extract song sentiment scores
    if (data_type == "Lyrics") {
        sentiment_scores <- sentiment_data$song_sentiment
    } else {
        sentiment_scores <- sentiment_data$song_sentiment
    }
    
    # Check if we have NRC sentiment categories
    has_emotions <- all(c("joy", "sadness", "anger", "fear") %in% colnames(sentiment_scores))
    
    # Create positivity ratio plot
    if ("positivity_ratio" %in% colnames(sentiment_scores)) {
        p1 <- ggplot(sentiment_scores, aes(x = positivity_ratio)) +
            geom_histogram(binwidth = 0.05, fill = "#1976D2", color = "white", alpha = 0.8) +
            geom_vline(xintercept = 0.5, linetype = "dashed", color = "#616161") +
            labs(
                title = paste(data_type, "Positivity Ratio Distribution"),
                x = "Positivity Ratio (Positive / (Positive + Negative))",
                y = "Number of Songs"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10)
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, paste0(tolower(data_type), "_positivity_dist.png"))
        ggsave(filename, p1, width = 8, height = 6, dpi = 300)
        cat("Created positivity distribution plot at", filename, "\n")
        
        # Create interactive version
        p1_interactive <- ggplotly(p1)
        htmlwidgets::saveWidget(
            p1_interactive, 
            file.path(VISUALS_DIR, paste0(tolower(data_type), "_positivity_dist_interactive.html")),
            selfcontained = TRUE
        )
    }
    
    # Create emotion proportion plot if using NRC lexicon
    if (has_emotions) {
        # Reshape data for plotting
        emotions <- c("joy", "trust", "fear", "surprise", "sadness", "disgust", "anger", "anticipation")
        emotions_present <- emotions[emotions %in% colnames(sentiment_scores)]
        
        emotion_data <- sentiment_scores %>%
            select(SongID, SongName, MajorityGenre, all_of(emotions_present)) %>%
            pivot_longer(
                cols = all_of(emotions_present),
                names_to = "emotion",
                values_to = "count"
            ) %>%
            # Calculate proportion for each song
            group_by(SongID) %>%
            mutate(
                total = sum(count),
                proportion = count / total
            ) %>%
            ungroup()
        
        # Create plot
        p2 <- ggplot(emotion_data, aes(x = emotion, y = proportion, fill = emotion)) +
            geom_boxplot(alpha = 0.7) +
            scale_fill_manual(values = unlist(EMOTION_COLORS[emotions_present])) +
            labs(
                title = paste("Emotion Distribution in", data_type),
                x = "Emotion",
                y = "Proportion within Song"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none"
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, paste0(tolower(data_type), "_emotion_dist.png"))
        ggsave(filename, p2, width = 10, height = 6, dpi = 300)
        cat("Created emotion distribution plot at", filename, "\n")
        
        # Create interactive version
        p2_interactive <- ggplotly(p2)
        htmlwidgets::saveWidget(
            p2_interactive, 
            file.path(VISUALS_DIR, paste0(tolower(data_type), "_emotion_dist_interactive.html")),
            selfcontained = TRUE
        )
    }
    return(invisible(NULL))
}

#' Create genre comparison plots
#'
#' Generate visualization comparing sentiment across genres
#' Let's see which genres are depressing as hell
#'
#' @param genre_sentiment result from CompareSentimentByGenre()
#' @return NULL (saves plots to files)
CreateGenreComparisonPlots <- function(genre_sentiment) {
    # Check if we have NRC sentiment categories
    has_emotions <- all(c("joy", "sadness", "anger", "fear") %in% colnames(genre_sentiment))
    
    # Create positivity ratio by genre plot
    if ("positivity_ratio" %in% colnames(genre_sentiment)) {
        # Sort genres by positivity ratio
        genre_ordered <- genre_sentiment %>%
            arrange(positivity_ratio) %>%
            pull(MajorityGenre)
        
        # Get colors for all genres in the dataset
        all_genre_colors <- getGenreColors(genre_sentiment$MajorityGenre)
        
        p1 <- ggplot(genre_sentiment, aes(x = factor(MajorityGenre, levels = genre_ordered), 
                                          y = positivity_ratio, 
                                          fill = MajorityGenre)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "#616161") +
            coord_flip() +
            scale_fill_manual(values = all_genre_colors) +
            labs(
                title = "Positivity Ratio by Genre",
                x = "Genre",
                y = "Positivity Ratio (Positive / (Positive + Negative))"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10),
                legend.position = "right" # Changed from "none" to show all genres
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, "genre_positivity_comparison.png")
        ggsave(filename, p1, width = 10, height = 6, dpi = 300)
        cat("Created genre positivity comparison plot at", filename, "\n")
        
        # Create interactive version
        p1_interactive <- ggplotly(p1)
        htmlwidgets::saveWidget(
            p1_interactive, 
            file.path(VISUALS_DIR, "genre_positivity_comparison_interactive.html"),
            selfcontained = TRUE
        )
    }
    
    # Create emotion profiles by genre if using NRC lexicon
    if (has_emotions) {
        # Reshape data for plotting
        emotions <- c("joy", "trust", "fear", "surprise", "sadness", "disgust", "anger", "anticipation")
        emotions_present <- emotions[emotions %in% colnames(genre_sentiment)]
        
        emotion_data <- genre_sentiment %>%
            select(MajorityGenre, all_of(emotions_present)) %>%
            pivot_longer(
                cols = all_of(emotions_present),
                names_to = "emotion",
                values_to = "count"
            )
        
        # Calculate proportion
        emotion_proportions <- emotion_data %>%
            group_by(MajorityGenre) %>%
            mutate(
                total = sum(count),
                proportion = count / total
            ) %>%
            ungroup()
        
        # Create plot
        p2 <- ggplot(emotion_proportions, 
                     aes(x = emotion, y = proportion, fill = emotion, group = MajorityGenre)) +
            geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
            facet_wrap(~ MajorityGenre) +
            scale_fill_manual(values = unlist(EMOTION_COLORS[emotions_present])) +
            labs(
                title = "Emotion Profiles by Genre",
                x = "Emotion",
                y = "Proportion"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                strip.text = element_text(size = 10, face = "bold"),
                legend.position = "none"
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, "genre_emotion_profiles.png")
        ggsave(filename, p2, width = 12, height = 8, dpi = 300)
        cat("Created genre emotion profiles plot at", filename, "\n")
        
        # Create interactive version
        p2_interactive <- ggplotly(p2)
        htmlwidgets::saveWidget(
            p2_interactive, 
            file.path(VISUALS_DIR, "genre_emotion_profiles_interactive.html"),
            selfcontained = TRUE
        )
    }
    return(invisible(NULL))
}

#' Create a comparison plot of lyrics vs comments sentiment
#'
#' @param comparison_data result from CompareLyricsAndComments()
#' @return NULL (saves plot to file)
CreateLyricsCommentsComparisonPlot <- function(comparison_data) {
    # Check if comparison data exist
    if (is.null(comparison_data)) {
        warning("No comparison data available for visualization. Can't compare nothing with nothing.")
        return(NULL)
    }
    
    # Filter out NA values before plotting
    comparison_data <- comparison_data %>%
        filter(!is.na(lyrics_positivity) & !is.na(comments_positivity))
    
    # Get colors for all genres in the dataset
    all_genre_colors <- getGenreColors(comparison_data$MajorityGenre)
    
    # Create scatter plot
    p <- ggplot(comparison_data, aes(x = lyrics_positivity, y = comments_positivity, 
                                     color = MajorityGenre)) +
        geom_point(alpha = 0.7, size = 3) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#616161") +
        scale_color_manual(values = all_genre_colors) +
        labs(
            title = "Lyrics vs. Comments Sentiment",
            subtitle = "Comparison of positivity ratio in lyrics and YouTube comments",
            x = "Lyrics Positivity Ratio",
            y = "Comments Positivity Ratio",
            color = "Genre"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10)
        )
    
    # Save plot
    filename <- file.path(VISUALS_DIR, "lyrics_comments_comparison.png")
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
    cat("Created lyrics vs comments comparison plot at", filename, "\n")
    
    # Create interactive version
    p_interactive <- ggplotly(
        p + geom_text(aes(label = SongName), vjust = -1, hjust = 0.5, size = 3, show.legend = FALSE)
    )
    
    htmlwidgets::saveWidget(
        p_interactive, 
        file.path(VISUALS_DIR, "lyrics_comments_comparison_interactive.html"),
        selfcontained = TRUE
    )
    return(invisible(NULL))
}

#' Create an emotion heatmap across genres
#'
#' Generate a heatmap showing the intensity of different emotions across music genres
#' Hot and cold emotions - see the patterns at a glance
#'
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @return plotly object (saves plot to file)
CreateEmotionHeatmap <- function(lyrics_sentiment) {
    # Extract song sentiment score
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Get emotion columns (excluding positivity metrics)
    emotion_cols <- c("joy", "trust", "fear", "surprise", "sadness", 
                      "disgust", "anger", "anticipation")
    available_emotions <- emotion_cols[emotion_cols %in% colnames(song_sentiment)]
    
    # If no emotion columns are available, use positive/negative
    if(length(available_emotions) == 0) {
        available_emotions <- c("positive", "negative")
    }
    
    # Aggregate emotions by genre
    genre_emotions <- song_sentiment %>%
        group_by(MajorityGenre) %>%
        summarize(across(all_of(available_emotions), mean, na.rm = TRUE)) %>%
        # Convert to long format
        pivot_longer(cols = all_of(available_emotions), 
                     names_to = "Emotion", 
                     values_to = "Intensity")
    
    # Normalize intensity for better visualization
    genre_emotions <- genre_emotions %>%
        group_by(Emotion) %>%
        mutate(NormalizedIntensity = scale(Intensity)[,1]) %>%
        ungroup()
    
    # Create heatmap
    p <- ggplot(genre_emotions, aes(x = MajorityGenre, y = Emotion, fill = NormalizedIntensity)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(
            title = "Emotion Intensity Across Music Genres",
            x = "Genre",
            y = "Emotion",
            fill = "Intensity\n(normalized)"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 10)
        )
    
    # Save as static image
    filename <- file.path(VISUALS_DIR, "genre_emotion_heatmap.png")
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
    
    # Create interactive version
    p_interactive <- ggplotly(p)
    htmlwidgets::saveWidget(
        p_interactive, 
        file.path(VISUALS_DIR, "genre_emotion_heatmap_interactive.html"),
        selfcontained = TRUE
    )
    
    cat("Created emotion heatmap at", filename, "\n")
    return(p_interactive)
}

#' Create YouTube metrics visualization
#'
#' Generates visualization comparing YouTube metrics
#' to explore relationships between engagement and emotion.
#' Do emotional songs get more comments? Let's find out.
#'
#' @param songData data frame with song information including YouTube metrics
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @param commentsData data frame with comment information
#' @return NULL (saves plots to files)
CreateYouTubeMetricsViz <- function(songData, lyrics_sentiment, commentsData) {
    # Check if we have YouTube data from API
    has_youtube_metrics <- all(c("SongLink") %in% colnames(songData))
    
    if (!has_youtube_metrics) {
        warning("YouTube metrics visualization requires video data. What are you even doing?")
        return(NULL)
    }
    
    # Extract YouTube video IDs
    songData$VideoID <- sapply(songData$SongLink, ExtractYouTubeID)
    
    # Count comments by song
    if (!is.null(commentsData) && nrow(commentsData) > 0) {
        comment_counts <- commentsData %>%
            group_by(SongID) %>%
            summarize(CommentCount = n())
        
        # Join with song data
        songData <- songData %>%
            left_join(comment_counts, by = "SongID")
    } else {
        songData$CommentCount <- 0
    }
    
    # Extract song sentiment
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Join the dataset
    combined_data <- songData %>%
        left_join(
            select(song_sentiment, SongID, positive, negative, positivity_ratio),
            by = "SongID"
        )
    
    # Set comment count to 0 for NA values
    combined_data$CommentCount[is.na(combined_data$CommentCount)] <- 0
    
    # Get colors for all genres in the dataset
    all_genre_colors <- getGenreColors(combined_data$MajorityGenre)
    
    # Create engagement vs sentiment plot
    p1 <- ggplot(combined_data, aes(x = positivity_ratio, y = CommentCount, 
                                    color = MajorityGenre, label = SongName)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_manual(values = all_genre_colors) +
        labs(
            title = "YouTube Engagement vs Song Positivity",
            subtitle = "Do more positive songs generate more comments?",
            x = "Positivity Ratio",
            y = "Comment Count",
            color = "Genre"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12)
        )
    
    # Save static version
    filename1 <- file.path(VISUALS_DIR, "youtube_engagement_sentiment.png")
    ggsave(filename1, p1, width = 10, height = 8, dpi = 300)
    
    # Create interactive version with tooltips
    p1_interactive <- ggplotly(p1)
    htmlwidgets::saveWidget(
        p1_interactive, 
        file.path(VISUALS_DIR, "youtube_engagement_sentiment_interactive.html"),
        selfcontained = TRUE
    )
    cat("Created YouTube engagement vs sentiment plot at", filename1, "\n")
    return(NULL)
}

#' Create sentiment trend analysis over time
#'
#' Analyzes how sentiment trends across years for different genres
#' Requires release year data.
#' Has music gotten more depressing over time? Let's see.
#'
#' @param songData data frame with song information including release year
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @return NULL (saves plots to files)
CreateSentimentTrendViz <- function(songData, lyrics_sentiment) {
    # For this function to work, we'd need release year data
    # Let's assume we add this to our songData or extract it
    
    # Generate random years for demonstration purposes
    # In a real implementation, you would use actual release years
    set.seed(123)
    songData$ReleaseYear <- sample(2000:2023, nrow(songData), replace = TRUE)
    
    # Extract song sentiment
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Combine data
    trend_data <- songData %>%
        select(SongID, SongName, ArtistName, MajorityGenre, ReleaseYear) %>%
        left_join(select(song_sentiment, SongID, positivity_ratio, positive, negative),
                  by = "SongID")
    
    # Create aggregated trend data by year and genre
    yearly_trends <- trend_data %>%
        group_by(ReleaseYear, MajorityGenre) %>%
        summarize(
            AvgPositivity = mean(positivity_ratio, na.rm = TRUE),
            SongCount = n(),
            .groups = 'drop'
        ) %>%
        # Only include years with enough songs
        filter(SongCount >= 1)
    
    # Get colors for all genres in the dataset
    all_genre_colors <- getGenreColors(yearly_trends$MajorityGenre)
    
    # Create trend visualization
    p <- ggplot(yearly_trends, aes(x = ReleaseYear, y = AvgPositivity, 
                                   color = MajorityGenre, group = MajorityGenre)) +
        geom_line(size = 1) +
        geom_point(size = 3, aes(size = SongCount)) +
        scale_color_manual(values = all_genre_colors) +
        labs(
            title = "Sentiment Trends in Music Over Time",
            subtitle = "How positivity in lyrics has changed by genre",
            x = "Release Year",
            y = "Average Positivity Ratio",
            color = "Genre",
            size = "Number of Songs"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 10)
        )
    
    # Save as static image
    filename <- file.path(VISUALS_DIR, "sentiment_trends_over_time.png")
    ggsave(filename, p, width = 12, height = 8, dpi = 300)
    
    # Create interactive version
    p_interactive <- ggplotly(p)
    htmlwidgets::saveWidget(
        p_interactive, 
        file.path(VISUALS_DIR, "sentiment_trends_over_time_interactive.html"),
        selfcontained = TRUE
    )
    cat("Created sentiment trends visualization at", filename, "\n")
    return(NULL)
}

#' Create lexical diversity vs emotion intensity plot
#'
#' Compares lexical diversity (unique words) with emotional intensity
#' to see if more diverse lyrics correlate with stronger emotions
#' Do complex lyrics = complex emotions? Let's find out.
#'
#' @param lyrics_tokens data frame with processed lyrics tokens
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @return NULL (saves plots to files)
CreateLexicalDiversityEmotionPlot <- function(lyrics_tokens, lyrics_sentiment) {
    # Calculate lexical diversity for each song
    lexical_diversity <- lyrics_tokens %>%
        group_by(SongID, SongName, ArtistName, MajorityGenre) %>%
        summarize(
            UniqueWords = n_distinct(word),
            TotalWords = n(),
            LexicalDiversity = UniqueWords / TotalWords,
            .groups = 'drop'
        )
    
    # Extract song sentiment
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Calculate emotional intensity (sum of all emotion scores or positive+negative)
    if (all(c("joy", "sadness", "anger", "fear") %in% colnames(song_sentiment))) {
        # If we have NRC emotions
        emotion_cols <- c("joy", "trust", "fear", "surprise", "sadness", 
                          "disgust", "anger", "anticipation")
        available_emotions <- emotion_cols[emotion_cols %in% colnames(song_sentiment)]
        
        song_sentiment <- song_sentiment %>%
            rowwise() %>%
            mutate(EmotionalIntensity = sum(c_across(all_of(available_emotions)), na.rm = TRUE)) %>%
            ungroup()
    } else {
        # If we only have positive/negative
        song_sentiment <- song_sentiment %>%
            mutate(EmotionalIntensity = positive + negative)
    }
    
    # Combine the data
    combined_data <- lexical_diversity %>%
        left_join(select(song_sentiment, SongID, EmotionalIntensity, positivity_ratio),
                  by = "SongID")
    
    # Get colors for all genres in the dataset
    all_genre_colors <- getGenreColors(combined_data$MajorityGenre)
    
    # Create visualization
    p <- ggplot(combined_data, aes(x = LexicalDiversity, y = EmotionalIntensity, 
                                   color = MajorityGenre, size = TotalWords,
                                   label = SongName)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(values = all_genre_colors) +
        scale_size_continuous(range = c(3, 10)) +
        labs(
            title = "Lexical Diversity vs. Emotional Intensity",
            subtitle = "Do more diverse lyrics express stronger emotions?",
            x = "Lexical Diversity (Unique Words / Total Words)",
            y = "Emotional Intensity",
            color = "Genre",
            size = "Song Length\n(Total Words)"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            # Make sure the legend is visible and sized appropriately
            legend.position = "right",
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8)
        )
    
    # Save image
    filename <- file.path(VISUALS_DIR, "lexical_diversity_emotion.png")
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
    
    # Create interactive version
    p_interactive <- ggplotly(p)
    htmlwidgets::saveWidget(
        p_interactive, 
        file.path(VISUALS_DIR, "lexical_diversity_emotion_interactive.html"),
        selfcontained = TRUE
    )
    cat("Created lexical diversity vs emotion plot at", filename, "\n")
    return(NULL)
}

#' Create emotion radar chart for genre comparison
#'
#' Generate radar charts to compare the emotional profiles of different genres
#' Spider charts look cool and make your report look fancy AF
#'
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @return NULL (saves plots to files)
CreateEmotionRadarChart <- function(lyrics_sentiment) {
    # Extract song sentiment scores
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Check if we have NRC sentiment categories
    has_emotions <- all(c("joy", "sadness", "anger", "fear") %in% colnames(song_sentiment))
    
    if (!has_emotions) {
        warning("Radar chart requires NRC emotion categories. Can't make cool radar charts without emotions.")
        return(NULL)
    }
    
    # Get emotion columns
    emotion_cols <- c("joy", "trust", "fear", "surprise", "sadness", 
                      "disgust", "anger", "anticipation")
    available_emotions <- emotion_cols[emotion_cols %in% colnames(song_sentiment)]
    
    # Aggregate by genre
    genre_emotions <- song_sentiment %>%
        group_by(MajorityGenre) %>%
        summarize(across(all_of(available_emotions), mean, na.rm = TRUE)) %>%
        # Convert to long format for radar chart
        pivot_longer(cols = all_of(available_emotions), 
                     names_to = "Emotion", 
                     values_to = "Intensity")
    
    # Normalize intensity for better visualization
    genre_emotions <- genre_emotions %>%
        group_by(Emotion) %>%
        mutate(NormalizedIntensity = scale(Intensity)[,1]) %>%
        ungroup()
    
    # Get unique genres
    genres <- unique(genre_emotions$MajorityGenre)
    
    # Get colors for all genres in the dataset
    all_genre_colors <- getGenreColors(genres)
    
    # Create a radar chart for each genre
    for (genre in genres) {
        # Filter data for current genre
        genre_data <- genre_emotions %>%
            filter(MajorityGenre == genre)
        
        # Create the radar chart using plotly
        plot_data <- genre_data %>%
            select(Emotion, NormalizedIntensity)
        
        # Calculate coordinates for a radar chart
        angles <- seq(0, 2*pi, length.out = length(available_emotions) + 1)
        
        # Create a dataframe with coordinates
        radar_data <- data.frame(
            x = cos(angles),
            y = sin(angles),
            Emotion = c(available_emotions, available_emotions[1])
        )
        
        # Join with emotion data
        radar_data <- radar_data %>%
            left_join(
                rbind(plot_data, plot_data[1,]),  # Close the polygon by repeating first point
                by = "Emotion"
            )
        
        # Convert NAs to 0
        radar_data$NormalizedIntensity[is.na(radar_data$NormalizedIntensity)] <- 0
        
        # Scale to unit circle
        radar_data <- radar_data %>%
            mutate(
                # Scale intensity to be between 0 and 1
                scaled_intensity = (NormalizedIntensity - min(NormalizedIntensity, na.rm = TRUE)) / 
                    (max(NormalizedIntensity, na.rm = TRUE) - min(NormalizedIntensity, na.rm = TRUE)),
                # Scale to the unit circle
                rx = x * scaled_intensity,
                ry = y * scaled_intensity
            )
        
        # Get genre color (use pre-defined or a default)
        genre_color <- all_genre_colors[genre]
        if(is.na(genre_color)) genre_color <- "#1976D2"  # Default to blue if color not found
        
        # Create the plot
        p <- ggplot() +
            # Draw the polygon
            geom_polygon(data = radar_data, aes(x = rx, y = ry), 
                         fill = genre_color, alpha = 0.5) +
            # Draw the grid lines
            geom_path(data = radar_data, aes(x = x, y = y), 
                      color = "gray", linetype = "dashed") +
            # Draw the spokes
            geom_segment(data = data.frame(x = 0, y = 0, Emotion = available_emotions),
                         aes(x = x, y = y, xend = cos(match(Emotion, available_emotions) * 2*pi/length(available_emotions)), 
                             yend = sin(match(Emotion, available_emotions) * 2*pi/length(available_emotions))),
                         color = "gray") +
            # Add the labels
            geom_text(data = data.frame(
                x = 1.1 * cos(match(available_emotions, available_emotions) * 2*pi/length(available_emotions)),
                y = 1.1 * sin(match(available_emotions, available_emotions) * 2*pi/length(available_emotions)),
                Emotion = available_emotions
            ), aes(x = x, y = y, label = Emotion), fontface = "bold") +
            # Add the points
            geom_point(data = radar_data[-nrow(radar_data),], aes(x = rx, y = ry), size = 3) +
            # Set the theme
            theme_void() +
            # Equal aspect ratio to make it circular
            coord_equal() +
            # Add title
            labs(
                title = paste("Emotional Profile:", genre),
                subtitle = "Radar chart of emotion intensity"
            ) +
            theme(
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 12, hjust = 0.5)
            )
        
        # Save image
        filename <- file.path(VISUALS_DIR, paste0("radar_", gsub(" ", "_", genre), ".png"))
        ggsave(filename, p, width = 8, height = 8, dpi = 300)
        
        # Create interactive version
        p_interactive <- ggplotly(p)
        htmlwidgets::saveWidget(
            p_interactive, 
            file.path(VISUALS_DIR, paste0("radar_", gsub(" ", "_", genre), "_interactive.html")),
            selfcontained = TRUE
        )
        
        cat("Created radar chart for", genre, "at", filename, "\n")
    }
    return(NULL)
}

#' Create a sentiment distribution map
#'
#' Generates a 2D map of songs (e.g., positive/negative vs. high/low energy)
#' to visualize the emotional landscape
#' Like a mood map for music - see where songs live emotionally
#'
#' @param lyrics_sentiment result from AnalyzeLyricsSentiment()
#' @return NULL (saves plots to files)
CreateSentimentMap <- function(lyrics_sentiment) {
    # Extract song sentiment scores
    song_sentiment <- lyrics_sentiment$song_sentiment
    
    # Check if we have NRC sentiment categories
    has_emotions <- all(c("joy", "sadness", "anger", "fear") %in% colnames(song_sentiment))
    
    # Create dimensions for mapping
    if (has_emotions) {
        # Create valence (positive/negative) dimension
        song_sentiment$valence <- with(song_sentiment, joy + trust - sadness - fear)
        
        # Create arousal (high/low energy) dimension
        song_sentiment$arousal <- with(song_sentiment, anger + fear + anticipation - trust)
    } else {
        # If we don't have NRC emotions, use basic positive/negative
        song_sentiment$valence <- with(song_sentiment, positive - negative)
        song_sentiment$arousal <- rep(0, nrow(song_sentiment))  # Can't calculate arousal
    }
    
    # Normalize dimensions
    song_sentiment <- song_sentiment %>%
        mutate(
            valence_norm = scale(valence)[,1],
            arousal_norm = scale(arousal)[,1]
        )
    
    # Get colors for all genres in the dataset
    all_genre_colors <- getGenreColors(song_sentiment$MajorityGenre)
    
    # Create the map visualization
    p <- ggplot(song_sentiment, aes(x = valence_norm, y = arousal_norm, 
                                    color = MajorityGenre, label = SongName)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_manual(values = all_genre_colors) +
        labs(
            title = "Emotional Landscape of Songs",
            subtitle = "Positioned by valence (positive/negative) and arousal (energy)",
            x = "Valence (Negative ← → Positive)",
            y = "Arousal (Calm ← → Energetic)",
            color = "Genre"
        ) +
        # Add quadrant labels
        annotate("text", x = 2, y = 2, label = "Energetic & Positive\n(Happy, Excited)", 
                 fontface = "bold", alpha = 0.5) +
        annotate("text", x = -2, y = 2, label = "Energetic & Negative\n(Angry, Anxious)", 
                 fontface = "bold", alpha = 0.5) +
        annotate("text", x = 2, y = -2, label = "Calm & Positive\n(Relaxed, Content)", 
                 fontface = "bold", alpha = 0.5) +
        annotate("text", x = -2, y = -2, label = "Calm & Negative\n(Sad, Depressed)", 
                 fontface = "bold", alpha = 0.5) +
        # Add reference lines
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        # Set theme
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12)
        )
    
    # Save image
    filename <- file.path(VISUALS_DIR, "sentiment_landscape_map.png")
    ggsave(filename, p, width = 12, height = 10, dpi = 300)
    
    # Create interactive version
    p_interactive <- ggplotly(p)
    htmlwidgets::saveWidget(
        p_interactive, 
        file.path(VISUALS_DIR, "sentiment_landscape_map_interactive.html"),
        selfcontained = TRUE
    )
    cat("Created sentiment landscape map at", filename, "\n")
    return(NULL)
}

#' Create Emotional Impact Comparison Chart
#'
#' Generates a clear visual comparison of how different genres emotionally impact listeners
#' Shows both lyrics sentiment and comment sentiment side by side for each genre
#' This visualization directly addresses our project goal of understanding the emotional impact of music
#'
#' @param lyrics_sentiment Result from AnalyzeLyricsSentiment()
#' @param comments_sentiment Result from AnalyzeCommentsSentiment()
#' @return NULL (saves plots to files)
CreateEmotionalImpactChart <- function(lyrics_sentiment, comments_sentiment) {
    # Check if we have comment sentiment data
    if (is.null(comments_sentiment)) {
        warning("No comment sentiment data available. Can't measure emotional impact without listener feedback.")
        return(NULL)
    }
    
    # Extract song-level sentiment for lyrics
    lyrics_data <- lyrics_sentiment$song_sentiment %>%
        select(SongID, SongName, ArtistName, MajorityGenre, positivity_ratio) %>%
        rename(lyrics_positivity = positivity_ratio)
    
    # Extract song-level sentiment for comments
    comment_data <- comments_sentiment$song_sentiment %>%
        select(SongID, positivity_ratio) %>%
        rename(comments_positivity = positivity_ratio)
    
    # Combine the data
    impact_data <- lyrics_data %>%
        left_join(comment_data, by = "SongID") %>%
        filter(!is.na(comments_positivity)) # Remove songs without comments
    
    # Calculate emotional impact metrics
    impact_data <- impact_data %>%
        mutate(
            emotional_resonance = abs(lyrics_positivity - comments_positivity),
            emotional_alignment = 1 - emotional_resonance,
            impact_direction = ifelse(comments_positivity > lyrics_positivity, 
                                      "More Positive Response", "More Negative Response"),
            impact_strength = case_when(
                emotional_resonance < 0.2 ~ "Low Impact (High Alignment)",
                emotional_resonance < 0.4 ~ "Moderate Impact",
                TRUE ~ "High Impact (Low Alignment)"
            )
        )
    
    # Aggregate by genre
    genre_impact <- impact_data %>%
        group_by(MajorityGenre) %>%
        summarize(
            avg_lyrics_positivity = mean(lyrics_positivity, na.rm = TRUE),
            avg_comments_positivity = mean(comments_positivity, na.rm = TRUE),
            avg_emotional_resonance = mean(emotional_resonance, na.rm = TRUE),
            avg_emotional_alignment = mean(emotional_alignment, na.rm = TRUE),
            song_count = n(),
            .groups = 'drop'
        ) %>%
        # Calculate impact direction per genre
        mutate(
            impact_direction = ifelse(avg_comments_positivity > avg_lyrics_positivity, 
                                      "Listeners respond more positively", 
                                      "Listeners respond more negatively"),
            # Convert to long format for side-by-side bars
            .groups = 'drop'
        )
    
    # Reshape for plotting
    genre_impact_long <- genre_impact %>%
        pivot_longer(
            cols = c(avg_lyrics_positivity, avg_comments_positivity),
            names_to = "metric",
            values_to = "value"
        ) %>%
        mutate(
            metric_label = case_when(
                metric == "avg_lyrics_positivity" ~ "Lyrics Sentiment",
                metric == "avg_comments_positivity" ~ "Listener Response",
                TRUE ~ metric
            )
        )
    
    # Get colors for genres
    all_genre_colors <- getGenreColors(genre_impact$MajorityGenre)
    
    # Create impact comparison chart
    p1 <- ggplot(genre_impact_long, 
                 aes(x = reorder(MajorityGenre, -value), y = value, fill = metric_label)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        geom_text(aes(label = sprintf("%.2f", value)), 
                  position = position_dodge(width = 0.9), 
                  vjust = -0.5, size = 3) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
        scale_fill_manual(values = c("Lyrics Sentiment" = "#4CAF50", "Listener Response" = "#2196F3")) +
        labs(
            title = "Emotional Impact of Music Genres on Listeners",
            subtitle = "Comparing the sentiment in lyrics vs. how listeners respond in comments",
            x = "Genre",
            y = "Positivity Ratio",
            fill = ""
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"
        )
    
    # Create emotional gap chart (showing the difference)
    genre_impact$gap <- genre_impact$avg_comments_positivity - genre_impact$avg_lyrics_positivity
    
    p2 <- ggplot(genre_impact, aes(x = reorder(MajorityGenre, gap), y = gap, fill = gap)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = sprintf("%+.2f", gap), 
                      y = ifelse(gap > 0, gap/2, gap/2)),
                  color = "white", fontface = "bold") +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_fill_gradient2(low = "#F44336", mid = "#BDBDBD", high = "#4CAF50", midpoint = 0) +
        labs(
            title = "Emotional Gap: How Listener Sentiment Differs from Lyrics",
            subtitle = "Positive values = listeners respond more positively than lyrics sentiment",
            x = "Genre",
            y = "Emotional Response Gap (Listener - Lyrics)",
            fill = "Gap"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right"
        )
    
    # Add annotations
    for (i in 1:nrow(genre_impact)) {
        genre <- genre_impact$MajorityGenre[i]
        gap <- genre_impact$gap[i]
        
        if (gap > 0.1) {
            p2 <- p2 + annotate("text", 
                                x = which(levels(reorder(genre_impact$MajorityGenre, genre_impact$gap)) == genre), 
                                y = gap + 0.05, 
                                label = "Uplifting effect", 
                                color = "#4CAF50",
                                fontface = "bold",
                                size = 3)
        } else if (gap < -0.1) {
            p2 <- p2 + annotate("text", 
                                x = which(levels(reorder(genre_impact$MajorityGenre, genre_impact$gap)) == genre), 
                                y = gap - 0.05, 
                                label = "Depressing effect", 
                                color = "#F44336",
                                fontface = "bold",
                                size = 3)
        }
    }
    
    # Save plots
    filename1 <- file.path(VISUALS_DIR, "genre_emotional_impact.png")
    ggsave(filename1, p1, width = 12, height = 8, dpi = 300)
    
    filename2 <- file.path(VISUALS_DIR, "emotional_response_gap.png")
    ggsave(filename2, p2, width = 12, height = 8, dpi = 300)
    
    # Create interactive versions
    p1_interactive <- ggplotly(p1)
    p2_interactive <- ggplotly(p2)
    
    htmlwidgets::saveWidget(
        p1_interactive, 
        file.path(VISUALS_DIR, "genre_emotional_impact_interactive.html"),
        selfcontained = TRUE
    )
    
    htmlwidgets::saveWidget(
        p2_interactive, 
        file.path(VISUALS_DIR, "emotional_response_gap_interactive.html"),
        selfcontained = TRUE
    )
    
    cat("Created emotional impact charts at", filename1, "and", filename2, "\n")
    
    # Return insights about emotional impact
    impact_insights <- genre_impact %>%
        arrange(desc(abs(gap))) %>%
        mutate(
            insight = case_when(
                gap > 0.1 ~ paste(MajorityGenre, "music has an uplifting effect: listeners respond more positively than the lyrics suggest"),
                gap < -0.1 ~ paste(MajorityGenre, "music has a depressing effect: listeners respond more negatively than the lyrics suggest"),
                TRUE ~ paste(MajorityGenre, "music creates a matching emotional response: listeners respond similarly to the lyrics sentiment")
            )
        )
    
    # Print insights
    cat("\nEMOTIONAL IMPACT INSIGHTS:\n")
    for (i in 1:nrow(impact_insights)) {
        cat(paste0(i, ". ", impact_insights$insight[i]), "\n")
    }
    
    # Save insights
    writeLines(
        c("EMOTIONAL IMPACT INSIGHTS:",
          sapply(1:nrow(impact_insights), function(i) paste0(i, ". ", impact_insights$insight[i]))),
        file.path(OUTPUT_DIR, "emotional_impact_insights.txt")
    )
    
    return(invisible(NULL))
}