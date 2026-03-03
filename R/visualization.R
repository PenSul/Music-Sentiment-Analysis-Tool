# visualization.R
# ---------------------------------------------------------------------------
# All plotting functions for the Music Sentiment Analysis pipeline.  Each
# function saves a static PNG and, where appropriate, an interactive HTML
# (Plotly) version to kVisualsDir.
# ---------------------------------------------------------------------------


#' Map genre names to display colours.
#'
#' Genres that match an entry in kGenreColors receive the predefined colour.
#' Any remaining genres are assigned colours from an RColorBrewer palette.
#'
#' @param genres Character vector of genre names.
#' @return A named character vector mapping each unique genre to a hex colour.
GetGenreColors <- function(genres) {
  allGenres   <- unique(genres)
  genreColors <- rep(NA_character_, length(allGenres))
  names(genreColors) <- allGenres

  for (genre in allGenres) {
    if (genre %in% names(kGenreColors)) {
      genreColors[genre] <- kGenreColors[[genre]]
    }
  }

  missingGenres <- allGenres[is.na(genreColors)]
  if (length(missingGenres) > 0) {
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
      install.packages("RColorBrewer")
    }
    library(RColorBrewer)

    nMissing  <- length(missingGenres)
    newColors <- if (nMissing <= 9) {
      brewer.pal(max(3, nMissing), "Set1")
    } else {
      colorRampPalette(brewer.pal(9, "Set1"))(nMissing)
    }

    for (i in seq_along(missingGenres)) {
      genreColors[missingGenres[i]] <- newColors[i]
    }
  }

  return(genreColors)
}


#' Generate a word cloud for each majority genre.
#'
#' @param lyricsTokens Data frame of cleaned lyrics tokens.
#' @param minFreq      Integer. Minimum word frequency for inclusion.
#' @param maxWords     Integer. Maximum number of words to display.
#' @return NULL invisibly.
CreateGenreWordClouds <- function(
    lyricsTokens,
    minFreq  = kProjectSettings$min_word_freq,
    maxWords = kProjectSettings$max_words) {

  genres <- unique(lyricsTokens$MajorityGenre)

  for (genre in genres) {
    genreWords <- lyricsTokens %>%
      filter(MajorityGenre == genre) %>%
      count(word, sort = TRUE)

    if (nrow(genreWords) < 10) {
      warning("Fewer than 10 words for genre '", genre, "'; skipping.")
      next
    }

    filename <- file.path(
      kVisualsDir,
      paste0("wordcloud_", gsub(" ", "_", genre), ".png")
    )
    png(filename, width = 800, height = 600, res = 100)

    wordcloud(
      words        = genreWords$word,
      freq         = genreWords$n,
      min.freq     = minFreq,
      max.words    = maxWords,
      random.order = FALSE,
      rot.per      = 0.35,
      colors       = brewer.pal(8, "Dark2")
    )

    dev.off()
    cat("Created word cloud for", genre, "at", filename, "\n")
  }

  return(invisible(NULL))
}


#' Plot the distribution of sentiment scores (positivity ratio and emotions).
#'
#' @param sentimentData A list returned by \code{AnalyzeLyricsSentiment()} or
#'   \code{AnalyzeCommentsSentiment()}.
#' @param dataType      Character label, either \code{"Lyrics"} or
#'   \code{"Comments"}.
#' @return NULL invisibly.
CreateSentimentPlots <- function(sentimentData, dataType = "Lyrics") {
  sentimentScores <- sentimentData$song_sentiment
  hasEmotions     <- all(c("joy", "sadness", "anger", "fear") %in%
                           colnames(sentimentScores))

  # ---- Positivity ratio histogram ----------------------------------------
  if ("positivity_ratio" %in% colnames(sentimentScores)) {
    p1 <- ggplot(sentimentScores, aes(x = positivity_ratio)) +
      geom_histogram(binwidth = 0.05, fill = "#1976D2",
                     color = "white", alpha = 0.8) +
      geom_vline(xintercept = 0.5, linetype = "dashed",
                 color = "#616161") +
      labs(
        title = paste(dataType, "Positivity Ratio Distribution"),
        x     = "Positivity Ratio (Positive / (Positive + Negative))",
        y     = "Number of Songs"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10)
      )

    filename <- file.path(
      kVisualsDir,
      paste0(tolower(dataType), "_positivity_dist.png")
    )
    ggsave(filename, p1, width = 8, height = 6, dpi = 300)
    cat("Created positivity distribution plot at", filename, "\n")

    p1Interactive <- ggplotly(p1)
    htmlwidgets::saveWidget(
      p1Interactive,
      file.path(kVisualsDir,
                paste0(tolower(dataType),
                       "_positivity_dist_interactive.html")),
      selfcontained = TRUE
    )
  }

  # ---- Emotion box plots (NRC only) --------------------------------------
  if (hasEmotions) {
    emotionNames    <- c("joy", "trust", "fear", "surprise",
                         "sadness", "disgust", "anger", "anticipation")
    emotionsPresent <- emotionNames[emotionNames %in%
                                      colnames(sentimentScores)]

    emotionData <- sentimentScores %>%
      select(SongID, SongName, MajorityGenre,
             all_of(emotionsPresent)) %>%
      pivot_longer(
        cols      = all_of(emotionsPresent),
        names_to  = "emotion",
        values_to = "count"
      ) %>%
      group_by(SongID) %>%
      mutate(
        total      = sum(count),
        proportion = count / total
      ) %>%
      ungroup()

    p2 <- ggplot(emotionData,
                 aes(x = emotion, y = proportion, fill = emotion)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = unlist(
        kEmotionColors[emotionsPresent]
      )) +
      labs(
        title = paste("Emotion Distribution in", dataType),
        x     = "Emotion",
        y     = "Proportion within Song"
      ) +
      theme_minimal() +
      theme(
        plot.title  = element_text(size = 16, face = "bold"),
        axis.title  = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )

    filename <- file.path(
      kVisualsDir,
      paste0(tolower(dataType), "_emotion_dist.png")
    )
    ggsave(filename, p2, width = 10, height = 6, dpi = 300)
    cat("Created emotion distribution plot at", filename, "\n")

    p2Interactive <- ggplotly(p2)
    htmlwidgets::saveWidget(
      p2Interactive,
      file.path(kVisualsDir,
                paste0(tolower(dataType),
                       "_emotion_dist_interactive.html")),
      selfcontained = TRUE
    )
  }

  return(invisible(NULL))
}


#' Compare positivity ratio and emotion profiles across genres.
#'
#' @param genreSentiment A data frame returned by
#'   \code{CompareSentimentByGenre()}.
#' @return NULL invisibly.
CreateGenreComparisonPlots <- function(genreSentiment) {
  hasEmotions <- all(c("joy", "sadness", "anger", "fear") %in%
                       colnames(genreSentiment))
  allGenreColors <- GetGenreColors(genreSentiment$MajorityGenre)

  # ---- Positivity ratio bar chart ----------------------------------------
  if ("positivity_ratio" %in% colnames(genreSentiment)) {
    genreOrdered <- genreSentiment %>%
      arrange(positivity_ratio) %>%
      pull(MajorityGenre)

    p1 <- ggplot(
      genreSentiment,
      aes(x    = factor(MajorityGenre, levels = genreOrdered),
          y    = positivity_ratio,
          fill = MajorityGenre)
    ) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_hline(yintercept = 0.5, linetype = "dashed",
                 color = "#616161") +
      coord_flip() +
      scale_fill_manual(values = allGenreColors) +
      labs(
        title = "Positivity Ratio by Genre",
        x     = "Genre",
        y     = "Positivity Ratio"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10),
        legend.position = "right"
      )

    filename <- file.path(kVisualsDir,
                          "genre_positivity_comparison.png")
    ggsave(filename, p1, width = 10, height = 6, dpi = 300)
    cat("Created genre positivity comparison at", filename, "\n")

    htmlwidgets::saveWidget(
      ggplotly(p1),
      file.path(kVisualsDir,
                "genre_positivity_comparison_interactive.html"),
      selfcontained = TRUE
    )
  }

  # ---- Faceted emotion bar chart (NRC only) ------------------------------
  if (hasEmotions) {
    emotionNames    <- c("joy", "trust", "fear", "surprise",
                         "sadness", "disgust", "anger", "anticipation")
    emotionsPresent <- emotionNames[emotionNames %in%
                                      colnames(genreSentiment)]

    emotionProportions <- genreSentiment %>%
      select(MajorityGenre, all_of(emotionsPresent)) %>%
      pivot_longer(cols      = all_of(emotionsPresent),
                   names_to  = "emotion",
                   values_to = "count") %>%
      group_by(MajorityGenre) %>%
      mutate(total      = sum(count),
             proportion = count / total) %>%
      ungroup()

    p2 <- ggplot(
      emotionProportions,
      aes(x = emotion, y = proportion, fill = emotion,
          group = MajorityGenre)
    ) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      facet_wrap(~ MajorityGenre) +
      scale_fill_manual(values = unlist(
        kEmotionColors[emotionsPresent]
      )) +
      labs(
        title = "Emotion Profiles by Genre",
        x     = "Emotion",
        y     = "Proportion"
      ) +
      theme_minimal() +
      theme(
        plot.title  = element_text(size = 16, face = "bold"),
        axis.title  = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text  = element_text(size = 10, face = "bold"),
        legend.position = "none"
      )

    filename <- file.path(kVisualsDir, "genre_emotion_profiles.png")
    ggsave(filename, p2, width = 12, height = 8, dpi = 300)
    cat("Created genre emotion profiles at", filename, "\n")

    htmlwidgets::saveWidget(
      ggplotly(p2),
      file.path(kVisualsDir,
                "genre_emotion_profiles_interactive.html"),
      selfcontained = TRUE
    )
  }

  return(invisible(NULL))
}


#' Scatter plot comparing lyrics positivity with comment positivity per song.
#'
#' @param comparisonData A data frame returned by
#'   \code{CompareLyricsAndComments()}.
#' @return NULL invisibly.
CreateLyricsCommentsComparisonPlot <- function(comparisonData) {
  if (is.null(comparisonData)) {
    warning("No comparison data available for visualisation.")
    return(NULL)
  }

  comparisonData <- comparisonData %>%
    filter(!is.na(lyrics_positivity), !is.na(comments_positivity))

  allGenreColors <- GetGenreColors(comparisonData$MajorityGenre)

  p <- ggplot(
    comparisonData,
    aes(x = lyrics_positivity, y = comments_positivity,
        color = MajorityGenre)
  ) +
    geom_point(alpha = 0.7, size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "#616161") +
    scale_color_manual(values = allGenreColors) +
    labs(
      title    = "Lyrics vs. Comments Sentiment",
      subtitle = paste("Comparison of positivity ratio in lyrics",
                       "and YouTube comments"),
      x     = "Lyrics Positivity Ratio",
      y     = "Comments Positivity Ratio",
      color = "Genre"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12),
      axis.text     = element_text(size = 10),
      legend.title  = element_text(size = 12),
      legend.text   = element_text(size = 10)
    )

  filename <- file.path(kVisualsDir, "lyrics_comments_comparison.png")
  ggsave(filename, p, width = 10, height = 8, dpi = 300)
  cat("Created lyrics vs comments comparison at", filename, "\n")

  pInteractive <- ggplotly(
    p + geom_text(aes(label = SongName), vjust = -1, hjust = 0.5,
                  size = 3, show.legend = FALSE)
  )
  htmlwidgets::saveWidget(
    pInteractive,
    file.path(kVisualsDir,
              "lyrics_comments_comparison_interactive.html"),
    selfcontained = TRUE
  )

  return(invisible(NULL))
}


#' Heatmap of normalised emotion intensities across genres.
#'
#' @param lyricsSentiment A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @return A Plotly widget (also saved as PNG and HTML).
CreateEmotionHeatmap <- function(lyricsSentiment) {
  songSentiment <- lyricsSentiment$song_sentiment

  emotionCols <- c("joy", "trust", "fear", "surprise",
                   "sadness", "disgust", "anger", "anticipation")
  availableEmotions <- emotionCols[emotionCols %in%
                                     colnames(songSentiment)]

  if (length(availableEmotions) == 0) {
    availableEmotions <- c("positive", "negative")
  }

  genreEmotions <- songSentiment %>%
    group_by(MajorityGenre) %>%
    summarize(across(all_of(availableEmotions), mean, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols      = all_of(availableEmotions),
                 names_to  = "Emotion",
                 values_to = "Intensity") %>%
    group_by(Emotion) %>%
    mutate(NormalizedIntensity = scale(Intensity)[, 1]) %>%
    ungroup()

  p <- ggplot(genreEmotions,
              aes(x = MajorityGenre, y = Emotion,
                  fill = NormalizedIntensity)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0) +
    labs(
      title = "Emotion Intensity Across Music Genres",
      x     = "Genre",
      y     = "Emotion",
      fill  = "Intensity\n(normalised)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 10)
    )

  filename <- file.path(kVisualsDir, "genre_emotion_heatmap.png")
  ggsave(filename, p, width = 10, height = 8, dpi = 300)
  cat("Created emotion heatmap at", filename, "\n")

  pInteractive <- ggplotly(p)
  htmlwidgets::saveWidget(
    pInteractive,
    file.path(kVisualsDir,
              "genre_emotion_heatmap_interactive.html"),
    selfcontained = TRUE
  )

  return(pInteractive)
}


#' Scatter plot relating YouTube comment counts to lyrics positivity.
#'
#' @param songData         A data frame returned by \code{ReadSongList()}.
#' @param lyricsSentiment  A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @param commentsData     A data frame of raw comments.
#' @return NULL invisibly.
CreateYouTubeMetricsViz <- function(songData,
                                    lyricsSentiment,
                                    commentsData) {
  if (!"SongLink" %in% colnames(songData)) {
    warning("YouTube metrics visualisation requires SongLink column.")
    return(NULL)
  }

  songData$VideoID <- sapply(songData$SongLink, ExtractYouTubeID)

  if (!is.null(commentsData) && nrow(commentsData) > 0) {
    commentCounts <- commentsData %>%
      group_by(SongID) %>%
      summarize(CommentCount = n(), .groups = "drop")
    songData <- songData %>%
      left_join(commentCounts, by = "SongID")
  } else {
    songData$CommentCount <- 0
  }

  songSentiment <- lyricsSentiment$song_sentiment

  combinedData <- songData %>%
    left_join(
      select(songSentiment, SongID, positive, negative,
             positivity_ratio),
      by = "SongID"
    )
  combinedData$CommentCount[is.na(combinedData$CommentCount)] <- 0

  allGenreColors <- GetGenreColors(combinedData$MajorityGenre)

  p <- ggplot(
    combinedData,
    aes(x = positivity_ratio, y = CommentCount,
        color = MajorityGenre, label = SongName)
  ) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = allGenreColors) +
    labs(
      title    = "YouTube Engagement vs. Song Positivity",
      subtitle = "Does lyric positivity correlate with comment volume?",
      x     = "Positivity Ratio",
      y     = "Comment Count",
      color = "Genre"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12)
    )

  filename <- file.path(kVisualsDir,
                        "youtube_engagement_sentiment.png")
  ggsave(filename, p, width = 10, height = 8, dpi = 300)
  cat("Created YouTube engagement plot at", filename, "\n")

  htmlwidgets::saveWidget(
    ggplotly(p),
    file.path(kVisualsDir,
              "youtube_engagement_sentiment_interactive.html"),
    selfcontained = TRUE
  )

  return(invisible(NULL))
}


#' Line chart of average positivity by genre over (simulated) release years.
#'
#' Because the input data does not include release dates, random years in
#' 2000–2023 are generated for demonstration purposes.  Replace the random
#' assignment with real metadata for production use.
#'
#' @param songData         A data frame returned by \code{ReadSongList()}.
#' @param lyricsSentiment  A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @return NULL invisibly.
CreateSentimentTrendViz <- function(songData, lyricsSentiment) {
  # Simulated release years for demonstration.
  set.seed(123)
  songData$ReleaseYear <- sample(2000:2023, nrow(songData),
                                 replace = TRUE)

  songSentiment <- lyricsSentiment$song_sentiment

  trendData <- songData %>%
    select(SongID, SongName, ArtistName,
           MajorityGenre, ReleaseYear) %>%
    left_join(
      select(songSentiment, SongID, positivity_ratio,
             positive, negative),
      by = "SongID"
    )

  yearlyTrends <- trendData %>%
    group_by(ReleaseYear, MajorityGenre) %>%
    summarize(
      AvgPositivity = mean(positivity_ratio, na.rm = TRUE),
      SongCount     = n(),
      .groups       = "drop"
    ) %>%
    filter(SongCount >= 1)

  allGenreColors <- GetGenreColors(yearlyTrends$MajorityGenre)

  p <- ggplot(
    yearlyTrends,
    aes(x = ReleaseYear, y = AvgPositivity,
        color = MajorityGenre, group = MajorityGenre)
  ) +
    geom_line(size = 1) +
    geom_point(size = 3, aes(size = SongCount)) +
    scale_color_manual(values = allGenreColors) +
    labs(
      title    = "Sentiment Trends in Music Over Time",
      subtitle = "Average positivity ratio by genre and year",
      x     = "Release Year",
      y     = "Average Positivity Ratio",
      color = "Genre",
      size  = "Number of Songs"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12),
      legend.title  = element_text(size = 10)
    )

  filename <- file.path(kVisualsDir,
                        "sentiment_trends_over_time.png")
  ggsave(filename, p, width = 12, height = 8, dpi = 300)
  cat("Created sentiment trend plot at", filename, "\n")

  htmlwidgets::saveWidget(
    ggplotly(p),
    file.path(kVisualsDir,
              "sentiment_trends_over_time_interactive.html"),
    selfcontained = TRUE
  )

  return(invisible(NULL))
}


#' Scatter plot of lexical diversity against emotional intensity.
#'
#' Lexical diversity is measured as the ratio of unique words to total words.
#' Emotional intensity is the sum of all NRC emotion counts (or positive +
#' negative when NRC is unavailable).
#'
#' @param lyricsTokens    A data frame of cleaned tokens from
#'   \code{ProcessLyrics()}.
#' @param lyricsSentiment A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @return NULL invisibly.
CreateLexicalDiversityEmotionPlot <- function(lyricsTokens,
                                              lyricsSentiment) {
  lexicalDiversity <- lyricsTokens %>%
    group_by(SongID, SongName, ArtistName, MajorityGenre) %>%
    summarize(
      UniqueWords      = n_distinct(word),
      TotalWords       = n(),
      LexicalDiversity = UniqueWords / TotalWords,
      .groups          = "drop"
    )

  songSentiment <- lyricsSentiment$song_sentiment

  emotionCols <- c("joy", "trust", "fear", "surprise",
                   "sadness", "disgust", "anger", "anticipation")
  availableEmotions <- emotionCols[emotionCols %in%
                                     colnames(songSentiment)]

  if (length(availableEmotions) > 0) {
    songSentiment <- songSentiment %>%
      rowwise() %>%
      mutate(EmotionalIntensity = sum(
        c_across(all_of(availableEmotions)), na.rm = TRUE
      )) %>%
      ungroup()
  } else {
    songSentiment <- songSentiment %>%
      mutate(EmotionalIntensity = positive + negative)
  }

  combinedData <- lexicalDiversity %>%
    left_join(
      select(songSentiment, SongID, EmotionalIntensity,
             positivity_ratio),
      by = "SongID"
    )

  allGenreColors <- GetGenreColors(combinedData$MajorityGenre)

  p <- ggplot(
    combinedData,
    aes(x = LexicalDiversity, y = EmotionalIntensity,
        color = MajorityGenre, size = TotalWords,
        label = SongName)
  ) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = allGenreColors) +
    scale_size_continuous(range = c(3, 10)) +
    labs(
      title    = "Lexical Diversity vs. Emotional Intensity",
      subtitle = "Do more diverse lyrics express stronger emotions?",
      x     = "Lexical Diversity (Unique / Total Words)",
      y     = "Emotional Intensity",
      color = "Genre",
      size  = "Song Length\n(Total Words)"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12),
      legend.position = "right",
      legend.title  = element_text(size = 10),
      legend.text   = element_text(size = 8)
    )

  filename <- file.path(kVisualsDir, "lexical_diversity_emotion.png")
  ggsave(filename, p, width = 10, height = 8, dpi = 300)
  cat("Created lexical diversity plot at", filename, "\n")

  htmlwidgets::saveWidget(
    ggplotly(p),
    file.path(kVisualsDir,
              "lexical_diversity_emotion_interactive.html"),
    selfcontained = TRUE
  )

  return(invisible(NULL))
}


#' Radar (spider) chart of NRC emotion profiles per genre.
#'
#' One radar chart is saved for each unique genre.  Intensities are
#' z-score normalised across genres so that the shapes reflect relative
#' emphasis rather than absolute counts.
#'
#' @param lyricsSentiment A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @return NULL invisibly.
CreateEmotionRadarChart <- function(lyricsSentiment) {

  songSentiment <- lyricsSentiment$song_sentiment

  hasEmotions <- all(c("joy", "sadness", "anger", "fear") %in%
                       colnames(songSentiment))
  if (!hasEmotions) {
    warning("Radar charts require NRC emotion columns.")
    return(NULL)
  }

  emotionCols <- c("joy", "trust", "fear", "surprise",
                   "sadness", "disgust", "anger", "anticipation")
  availableEmotions <- emotionCols[emotionCols %in%
                                     colnames(songSentiment)]

  genreEmotions <- songSentiment %>%
    group_by(MajorityGenre) %>%
    summarize(across(all_of(availableEmotions), mean, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols      = all_of(availableEmotions),
                 names_to  = "Emotion",
                 values_to = "Intensity") %>%
    group_by(Emotion) %>%
    mutate(NormalizedIntensity = scale(Intensity)[, 1]) %>%
    ungroup()

  genres         <- unique(genreEmotions$MajorityGenre)
  allGenreColors <- GetGenreColors(genres)
  nEmotions      <- length(availableEmotions)
  angles         <- seq(0, 2 * pi, length.out = nEmotions + 1)

  for (genre in genres) {
    genreData <- genreEmotions %>%
      filter(MajorityGenre == genre) %>%
      select(Emotion, NormalizedIntensity)

    radarData <- data.frame(
      x       = cos(angles),
      y       = sin(angles),
      Emotion = c(availableEmotions, availableEmotions[1])
    ) %>%
      left_join(rbind(genreData, genreData[1, ]), by = "Emotion")

    radarData$NormalizedIntensity[
      is.na(radarData$NormalizedIntensity)
    ] <- 0

    minVal <- min(radarData$NormalizedIntensity, na.rm = TRUE)
    maxVal <- max(radarData$NormalizedIntensity, na.rm = TRUE)
    rangeVal <- maxVal - minVal
    if (rangeVal == 0) rangeVal <- 1

    radarData <- radarData %>%
      mutate(
        scaledIntensity = (NormalizedIntensity - minVal) / rangeVal,
        rx = x * scaledIntensity,
        ry = y * scaledIntensity
      )

    genreColor <- allGenreColors[genre]
    if (is.na(genreColor)) genreColor <- "#1976D2"

    labelCoords <- data.frame(
      x       = 1.1 * cos(match(availableEmotions,
                                 availableEmotions) *
                             2 * pi / nEmotions),
      y       = 1.1 * sin(match(availableEmotions,
                                 availableEmotions) *
                             2 * pi / nEmotions),
      Emotion = availableEmotions
    )

    spokeData <- data.frame(
      x       = 0,
      y       = 0,
      Emotion = availableEmotions,
      xend    = cos(match(availableEmotions, availableEmotions) *
                      2 * pi / nEmotions),
      yend    = sin(match(availableEmotions, availableEmotions) *
                      2 * pi / nEmotions)
    )

    p <- ggplot() +
      geom_polygon(data = radarData,
                   aes(x = rx, y = ry),
                   fill = genreColor, alpha = 0.5) +
      geom_path(data = radarData,
                aes(x = x, y = y),
                color = "gray", linetype = "dashed") +
      geom_segment(data = spokeData,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "gray") +
      geom_text(data = labelCoords,
                aes(x = x, y = y, label = Emotion),
                fontface = "bold") +
      geom_point(data = radarData[-nrow(radarData), ],
                 aes(x = rx, y = ry), size = 3) +
      coord_equal() +
      theme_void() +
      labs(
        title    = paste("Emotional Profile:", genre),
        subtitle = "Radar chart of normalised emotion intensity"
      ) +
      theme(
        plot.title    = element_text(size = 16, face = "bold",
                                     hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )

    filename <- file.path(
      kVisualsDir,
      paste0("radar_", gsub(" ", "_", genre), ".png")
    )
    ggsave(filename, p, width = 8, height = 8, dpi = 300)
    cat("Created radar chart for", genre, "at", filename, "\n")

    htmlwidgets::saveWidget(
      ggplotly(p),
      file.path(kVisualsDir,
                paste0("radar_", gsub(" ", "_", genre),
                       "_interactive.html")),
      selfcontained = TRUE
    )
  }

  return(invisible(NULL))
}


#' Two-dimensional emotional landscape map (valence × arousal).
#'
#' Projects each song onto a plane where the x-axis represents valence
#' (positive – negative) and the y-axis represents arousal (energetic –
#' calm), then colours points by genre.
#'
#' @param lyricsSentiment A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @return NULL invisibly.
CreateSentimentMap <- function(lyricsSentiment) {
  songSentiment <- lyricsSentiment$song_sentiment

  hasEmotions <- all(c("joy", "sadness", "anger", "fear") %in%
                       colnames(songSentiment))

  if (hasEmotions) {
    songSentiment$valence <- with(
      songSentiment, joy + trust - sadness - fear
    )
    songSentiment$arousal <- with(
      songSentiment, anger + fear + anticipation - trust
    )
  } else {
    songSentiment$valence <- with(
      songSentiment, positive - negative
    )
    songSentiment$arousal <- 0
  }

  songSentiment <- songSentiment %>%
    mutate(
      valenceNorm = scale(valence)[, 1],
      arousalNorm = scale(arousal)[, 1]
    )

  allGenreColors <- GetGenreColors(songSentiment$MajorityGenre)

  p <- ggplot(
    songSentiment,
    aes(x = valenceNorm, y = arousalNorm,
        color = MajorityGenre, label = SongName)
  ) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = allGenreColors) +
    annotate("text", x =  2, y =  2,
             label = "Energetic & Positive\n(Happy, Excited)",
             fontface = "bold", alpha = 0.5) +
    annotate("text", x = -2, y =  2,
             label = "Energetic & Negative\n(Angry, Anxious)",
             fontface = "bold", alpha = 0.5) +
    annotate("text", x =  2, y = -2,
             label = "Calm & Positive\n(Relaxed, Content)",
             fontface = "bold", alpha = 0.5) +
    annotate("text", x = -2, y = -2,
             label = "Calm & Negative\n(Sad, Depressed)",
             fontface = "bold", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray50") +
    labs(
      title    = "Emotional Landscape of Songs",
      subtitle = "Valence (positive/negative) vs. arousal (energy)",
      x     = "Valence (Negative \u2190 \u2192 Positive)",
      y     = "Arousal (Calm \u2190 \u2192 Energetic)",
      color = "Genre"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12)
    )

  filename <- file.path(kVisualsDir, "sentiment_landscape_map.png")
  ggsave(filename, p, width = 12, height = 10, dpi = 300)
  cat("Created sentiment landscape map at", filename, "\n")

  htmlwidgets::saveWidget(
    ggplotly(p),
    file.path(kVisualsDir,
              "sentiment_landscape_map_interactive.html"),
    selfcontained = TRUE
  )

  return(invisible(NULL))
}


#' Side-by-side and gap charts showing how listener sentiment in comments
#' compares with the sentiment expressed in lyrics, aggregated by genre.
#'
#' Produces two charts: (1) grouped bars of average lyrics vs. comment
#' positivity per genre, and (2) a diverging bar chart of the signed
#' difference.  Textual insights are printed and saved to a file.
#'
#' @param lyricsSentiment   A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @param commentsSentiment A list returned by
#'   \code{AnalyzeCommentsSentiment()}.
#' @return NULL invisibly.
CreateEmotionalImpactChart <- function(lyricsSentiment,
                                       commentsSentiment) {
  if (is.null(commentsSentiment)) {
    warning("Comment sentiment data required for emotional impact chart.")
    return(NULL)
  }

  lyricsData <- lyricsSentiment$song_sentiment %>%
    select(SongID, SongName, ArtistName, MajorityGenre,
           positivity_ratio) %>%
    rename(lyricsPositivity = positivity_ratio)

  commentData <- commentsSentiment$song_sentiment %>%
    select(SongID, positivity_ratio) %>%
    rename(commentsPositivity = positivity_ratio)

  impactData <- lyricsData %>%
    left_join(commentData, by = "SongID") %>%
    filter(!is.na(commentsPositivity)) %>%
    mutate(
      emotionalResonance  = abs(lyricsPositivity - commentsPositivity),
      emotionalAlignment  = 1 - emotionalResonance,
      impactDirection     = ifelse(
        commentsPositivity > lyricsPositivity,
        "More Positive Response", "More Negative Response"
      ),
      impactStrength = case_when(
        emotionalResonance < 0.2 ~ "Low Impact (High Alignment)",
        emotionalResonance < 0.4 ~ "Moderate Impact",
        TRUE                     ~ "High Impact (Low Alignment)"
      )
    )

  genreImpact <- impactData %>%
    group_by(MajorityGenre) %>%
    summarize(
      avgLyricsPositivity   = mean(lyricsPositivity, na.rm = TRUE),
      avgCommentsPositivity = mean(commentsPositivity, na.rm = TRUE),
      avgEmotionalResonance = mean(emotionalResonance, na.rm = TRUE),
      avgEmotionalAlignment = mean(emotionalAlignment, na.rm = TRUE),
      songCount             = n(),
      .groups               = "drop"
    ) %>%
    mutate(
      impactDirection = ifelse(
        avgCommentsPositivity > avgLyricsPositivity,
        "Listeners respond more positively",
        "Listeners respond more negatively"
      )
    )

  genreImpactLong <- genreImpact %>%
    pivot_longer(
      cols      = c(avgLyricsPositivity, avgCommentsPositivity),
      names_to  = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metricLabel = case_when(
        metric == "avgLyricsPositivity"   ~ "Lyrics Sentiment",
        metric == "avgCommentsPositivity" ~ "Listener Response",
        TRUE ~ metric
      )
    )

  # ---- Chart 1: grouped bars ---------------------------------------------
  p1 <- ggplot(
    genreImpactLong,
    aes(x = reorder(MajorityGenre, -value), y = value,
        fill = metricLabel)
  ) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.2f", value)),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    geom_hline(yintercept = 0.5, linetype = "dashed",
               color = "gray50") +
    scale_fill_manual(
      values = c("Lyrics Sentiment"  = "#4CAF50",
                 "Listener Response"  = "#2196F3")
    ) +
    labs(
      title    = "Emotional Impact of Music Genres on Listeners",
      subtitle = paste("Comparing lyric sentiment with listener",
                       "response in comments"),
      x    = "Genre",
      y    = "Positivity Ratio",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )

  # ---- Chart 2: emotional gap --------------------------------------------
  genreImpact$gap <- genreImpact$avgCommentsPositivity -
    genreImpact$avgLyricsPositivity

  p2 <- ggplot(
    genreImpact,
    aes(x = reorder(MajorityGenre, gap), y = gap, fill = gap)
  ) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(
      aes(label = sprintf("%+.2f", gap),
          y     = ifelse(gap > 0, gap / 2, gap / 2)),
      color    = "white",
      fontface = "bold"
    ) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    scale_fill_gradient2(low = "#F44336", mid = "#BDBDBD",
                         high = "#4CAF50", midpoint = 0) +
    labs(
      title    = paste("Emotional Gap: Listener Sentiment vs.",
                       "Lyrics Sentiment"),
      subtitle = paste("Positive = listeners respond more positively",
                       "than the lyrics suggest"),
      x    = "Genre",
      y    = "Emotional Response Gap (Listener \u2212 Lyrics)",
      fill = "Gap"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  # Add directional annotations to the gap chart.
  orderedGenres <- levels(reorder(genreImpact$MajorityGenre,
                                  genreImpact$gap))
  for (idx in seq_len(nrow(genreImpact))) {
    genre <- genreImpact$MajorityGenre[idx]
    gap   <- genreImpact$gap[idx]
    xPos  <- which(orderedGenres == genre)

    if (gap > 0.1) {
      p2 <- p2 + annotate(
        "text", x = xPos, y = gap + 0.05,
        label = "Uplifting effect", color = "#4CAF50",
        fontface = "bold", size = 3
      )
    } else if (gap < -0.1) {
      p2 <- p2 + annotate(
        "text", x = xPos, y = gap - 0.05,
        label = "Dampening effect", color = "#F44336",
        fontface = "bold", size = 3
      )
    }
  }

  # ---- Save outputs -------------------------------------------------------
  filename1 <- file.path(kVisualsDir, "genre_emotional_impact.png")
  ggsave(filename1, p1, width = 12, height = 8, dpi = 300)

  filename2 <- file.path(kVisualsDir, "emotional_response_gap.png")
  ggsave(filename2, p2, width = 12, height = 8, dpi = 300)

  htmlwidgets::saveWidget(
    ggplotly(p1),
    file.path(kVisualsDir,
              "genre_emotional_impact_interactive.html"),
    selfcontained = TRUE
  )
  htmlwidgets::saveWidget(
    ggplotly(p2),
    file.path(kVisualsDir,
              "emotional_response_gap_interactive.html"),
    selfcontained = TRUE
  )

  cat("Created emotional impact charts at", filename1,
      "and", filename2, "\n")

  # ---- Generate textual insights ------------------------------------------
  impactInsights <- genreImpact %>%
    arrange(desc(abs(gap))) %>%
    mutate(
      insight = case_when(
        gap >  0.1 ~ paste(
          MajorityGenre, "music has an uplifting effect:",
          "listeners respond more positively than lyrics suggest."
        ),
        gap < -0.1 ~ paste(
          MajorityGenre, "music has a dampening effect:",
          "listeners respond more negatively than lyrics suggest."
        ),
        TRUE ~ paste(
          MajorityGenre, "music elicits a matching response:",
          "listener sentiment aligns with lyrics."
        )
      )
    )

  cat("\nEMOTIONAL IMPACT INSIGHTS:\n")
  for (idx in seq_len(nrow(impactInsights))) {
    cat(sprintf("%d. %s\n", idx, impactInsights$insight[idx]))
  }

  writeLines(
    c("EMOTIONAL IMPACT INSIGHTS:",
      vapply(seq_len(nrow(impactInsights)), function(idx) {
        sprintf("%d. %s", idx, impactInsights$insight[idx])
      }, character(1))),
    file.path(kOutputDir, "emotional_impact_insights.txt")
  )

  return(invisible(NULL))
}
