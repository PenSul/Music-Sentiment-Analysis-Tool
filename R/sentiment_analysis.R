# sentiment_analysis.R
# ---------------------------------------------------------------------------
# Lexicon-based sentiment analysis for lyrics and YouTube comments, genre
# aggregation, and lyrics-versus-comments comparison.
# ---------------------------------------------------------------------------


#' Score the sentiment of lyrics tokens using a tidytext lexicon.
#'
#' Joins the cleaned token data frame with the chosen sentiment lexicon and
#' computes per-song summary statistics.  Supports the Bing, NRC, AFINN, and
#' Loughran lexicons.  Word-level and song-level results are saved to CSV.
#'
#' @param lyricsTokens A data frame of cleaned tokens from
#'   \code{ProcessLyrics()}.
#' @param lexicon      Character. One of \code{"bing"}, \code{"nrc"},
#'   \code{"afinn"}, or \code{"loughran"}.
#' @return A named list with elements \code{word_sentiment} (token-level
#'   matches) and \code{song_sentiment} (song-level summary).
AnalyzeLyricsSentiment <- function(
    lyricsTokens,
    lexicon = kProjectSettings$sentiment_lexicon) {

  sentimentLex <- switch(
    lexicon,
    bing     = get_sentiments("bing"),
    nrc      = get_sentiments("nrc"),
    afinn    = get_sentiments("afinn"),
    loughran = get_sentiments("loughran"),
    stop("Unsupported lexicon: '", lexicon, "'. ",
         "Use 'bing', 'nrc', 'afinn', or 'loughran'.")
  )

  lyricsSentiment <- lyricsTokens %>%
    inner_join(sentimentLex, by = "word")

  totalWords <- lyricsTokens %>%
    group_by(SongID) %>%
    summarize(total_words = n())

  # ---- Lexicon-specific aggregation --------------------------------------

  if (lexicon == "nrc") {
    songSentiment <- lyricsSentiment %>%
      filter(!sentiment %in% c("positive", "negative")) %>%
      count(SongID, SongName, ArtistName,
            MajorityGenre, MinorityGenre, sentiment) %>%
      spread(sentiment, n, fill = 0)

    posNeg <- lyricsSentiment %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      count(SongID, SongName, ArtistName,
            MajorityGenre, MinorityGenre, sentiment) %>%
      spread(sentiment, n, fill = 0)

    if (!"positive" %in% colnames(posNeg)) posNeg$positive <- 0
    if (!"negative" %in% colnames(posNeg)) posNeg$negative <- 0

    songSentiment <- songSentiment %>%
      left_join(posNeg,
                by = c("SongID", "SongName", "ArtistName",
                       "MajorityGenre", "MinorityGenre")) %>%
      left_join(totalWords, by = "SongID") %>%
      mutate(
        sentiment_words  = positive + negative,
        positivity_ratio = positive / (positive + negative),
        sentiment_density = sentiment_words / total_words
      )

  } else if (lexicon == "bing") {
    songSentiment <- lyricsSentiment %>%
      count(SongID, SongName, ArtistName,
            MajorityGenre, MinorityGenre, sentiment) %>%
      spread(sentiment, n, fill = 0)

    if (!"positive" %in% colnames(songSentiment)) {
      songSentiment$positive <- 0
    }
    if (!"negative" %in% colnames(songSentiment)) {
      songSentiment$negative <- 0
    }

    songSentiment <- songSentiment %>%
      mutate(
        sentiment_words  = positive + negative,
        positivity_ratio = ifelse(
          positive + negative > 0,
          positive / (positive + negative),
          0.5
        )
      ) %>%
      left_join(totalWords, by = "SongID") %>%
      mutate(sentiment_density = sentiment_words / total_words)

  } else if (lexicon == "afinn") {
    songSentiment <- lyricsSentiment %>%
      group_by(SongID, SongName, ArtistName,
               MajorityGenre, MinorityGenre) %>%
      summarize(
        sentiment_sum   = sum(value),
        sentiment_words = n(),
        avg_sentiment   = sentiment_sum / sentiment_words,
        .groups = "drop"
      ) %>%
      left_join(totalWords, by = "SongID") %>%
      mutate(sentiment_density = sentiment_words / total_words)

  } else if (lexicon == "loughran") {
    songSentiment <- lyricsSentiment %>%
      count(SongID, SongName, ArtistName,
            MajorityGenre, MinorityGenre, sentiment) %>%
      spread(sentiment, n, fill = 0)

    sentimentCols <- setdiff(
      colnames(songSentiment),
      c("SongID", "SongName", "ArtistName",
        "MajorityGenre", "MinorityGenre")
    )
    songSentiment$sentiment_words <- rowSums(
      songSentiment[, sentimentCols]
    )

    songSentiment <- songSentiment %>%
      left_join(totalWords, by = "SongID") %>%
      mutate(sentiment_density = sentiment_words / total_words)
  }

  # ---- Persist results ----------------------------------------------------

  write.csv(lyricsSentiment,
            file.path(kOutputDir, "lyrics_sentiment_words.csv"),
            row.names = FALSE)
  write.csv(songSentiment,
            file.path(kOutputDir, "song_sentiment_scores.csv"),
            row.names = FALSE)

  return(list(
    word_sentiment = lyricsSentiment,
    song_sentiment = songSentiment
  ))
}


#' Score the sentiment of YouTube comment tokens.
#'
#' Applies the same lexicon-based approach as \code{AnalyzeLyricsSentiment()}
#' but at both the individual-comment and per-song aggregate levels.
#'
#' @param commentTokens A data frame of cleaned tokens from
#'   \code{ProcessComments()}.
#' @param lexicon       Character. Lexicon name (same options as for lyrics).
#' @return A named list with elements \code{word_sentiment},
#'   \code{comment_sentiment} (per-comment scores), and
#'   \code{song_sentiment} (per-song average scores), or NULL if no data.
AnalyzeCommentsSentiment <- function(
    commentTokens,
    lexicon = kProjectSettings$sentiment_lexicon) {

  if (is.null(commentTokens) || nrow(commentTokens) == 0) {
    warning("No comment tokens available for sentiment analysis.")
    return(NULL)
  }

  sentimentLex <- switch(
    lexicon,
    nrc      = get_sentiments("nrc"),
    bing     = get_sentiments("bing"),
    afinn    = get_sentiments("afinn"),
    loughran = get_sentiments("loughran"),
    stop("Unsupported lexicon: '", lexicon, "'.")
  )

  commentSentiment <- commentTokens %>%
    inner_join(sentimentLex, by = "word")

  totalWords <- commentTokens %>%
    group_by(SongID, CommentID) %>%
    summarize(total_words = n(), .groups = "drop")

  # ---- Lexicon-specific aggregation --------------------------------------

  if (lexicon == "nrc") {
    commentEmotions <- commentSentiment %>%
      filter(!sentiment %in% c("positive", "negative")) %>%
      count(SongID, CommentID, sentiment) %>%
      spread(sentiment, n, fill = 0)

    posNeg <- commentSentiment %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      count(SongID, CommentID, sentiment) %>%
      spread(sentiment, n, fill = 0)

    commentScores <- commentEmotions %>%
      left_join(posNeg, by = c("SongID", "CommentID")) %>%
      left_join(totalWords, by = c("SongID", "CommentID")) %>%
      mutate(
        sentiment_words  = positive + negative,
        positivity_ratio = ifelse(
          positive + negative > 0,
          positive / (positive + negative),
          0.5
        ),
        sentiment_density = sentiment_words / total_words
      )

  } else if (lexicon == "bing") {
    commentScores <- commentSentiment %>%
      count(SongID, CommentID, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(
        sentiment_words  = positive + negative,
        positivity_ratio = ifelse(
          positive + negative > 0,
          positive / (positive + negative),
          0.5
        )
      ) %>%
      left_join(totalWords, by = c("SongID", "CommentID")) %>%
      mutate(sentiment_density = sentiment_words / total_words)

  } else if (lexicon == "afinn") {
    commentScores <- commentSentiment %>%
      group_by(SongID, CommentID) %>%
      summarize(
        sentiment_sum   = sum(value),
        sentiment_words = n(),
        avg_sentiment   = sentiment_sum / sentiment_words,
        .groups = "drop"
      ) %>%
      left_join(totalWords, by = c("SongID", "CommentID")) %>%
      mutate(sentiment_density = sentiment_words / total_words)
  }

  # Aggregate to song level by averaging across comments.
  songSentiment <- commentScores %>%
    group_by(SongID) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE),
              .groups = "drop")

  # ---- Persist results ----------------------------------------------------

  write.csv(commentSentiment,
            file.path(kOutputDir, "comment_sentiment_words.csv"),
            row.names = FALSE)
  write.csv(commentScores,
            file.path(kOutputDir, "comment_sentiment_scores.csv"),
            row.names = FALSE)
  write.csv(songSentiment,
            file.path(kOutputDir, "song_comment_sentiment.csv"),
            row.names = FALSE)

  return(list(
    word_sentiment    = commentSentiment,
    comment_sentiment = commentScores,
    song_sentiment    = songSentiment
  ))
}


#' Aggregate song-level sentiment by majority genre.
#'
#' Computes the mean of every numeric sentiment column for each genre,
#' providing a high-level comparison of emotional profiles.
#'
#' @param lyricsSentiment A list returned by \code{AnalyzeLyricsSentiment()}.
#' @return A data frame with one row per genre and averaged sentiment columns.
CompareSentimentByGenre <- function(lyricsSentiment) {
  songSentiment <- lyricsSentiment$song_sentiment

  genreSentiment <- songSentiment %>%
    group_by(MajorityGenre) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE),
              .groups = "drop")

  write.csv(genreSentiment,
            file.path(kOutputDir, "genre_sentiment.csv"),
            row.names = FALSE)

  return(genreSentiment)
}


#' Compare the positivity ratio in lyrics against YouTube comments for each
#' song.
#'
#' Joins song-level sentiment from both sources and computes the directional
#' difference (comments positivity minus lyrics positivity).
#'
#' @param lyricsSentiment   A list returned by
#'   \code{AnalyzeLyricsSentiment()}.
#' @param commentsSentiment A list returned by
#'   \code{AnalyzeCommentsSentiment()}, or NULL.
#' @return A data frame with lyrics and comment positivity side by side, or
#'   NULL if comment data is unavailable.
CompareLyricsAndComments <- function(lyricsSentiment, commentsSentiment) {
  if (is.null(commentsSentiment)) {
    warning("No comment sentiment data available for comparison.")
    return(NULL)
  }

  lyricsScores  <- lyricsSentiment$song_sentiment
  commentScores <- commentsSentiment$song_sentiment

  comparison <- lyricsScores %>%
    select(SongID, SongName, ArtistName, MajorityGenre, MinorityGenre,
           positive, negative, positivity_ratio) %>%
    rename(
      lyrics_positive   = positive,
      lyrics_negative   = negative,
      lyrics_positivity = positivity_ratio
    ) %>%
    left_join(
      select(commentScores, SongID, positive, negative, positivity_ratio),
      by = "SongID"
    ) %>%
    rename(
      comments_positive   = positive,
      comments_negative   = negative,
      comments_positivity = positivity_ratio
    ) %>%
    mutate(sentiment_diff = comments_positivity - lyrics_positivity)

  write.csv(comparison,
            file.path(kOutputDir, "lyrics_comments_comparison.csv"),
            row.names = FALSE)

  return(comparison)
}
