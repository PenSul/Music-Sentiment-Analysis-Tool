# text_processing.R
# ---------------------------------------------------------------------------
# Text-tokenisation, stopword removal, TF-IDF computation, and n-gram
# extraction for lyrics and YouTube comments.
# ---------------------------------------------------------------------------


#' Tokenise and clean lyrics text.
#'
#' Filters to songs whose lyrics were successfully retrieved, splits text into
#' single-word tokens, removes stopwords, numbers, non-alphabetic tokens, and
#' very short words (fewer than three characters).
#'
#' @param lyricsData A data frame produced by \code{CollectLyricsDirectly()}.
#' @param stopwords  Character vector of stopwords to exclude.
#' @return A data frame of cleaned single-word tokens with contextual columns
#'   (SongID, SongName, ArtistName, MajorityGenre, MinorityGenre, word).
ProcessLyrics <- function(lyricsData, stopwords = GetStopwords()) {
  lyricsData <- lyricsData %>%
    filter(LyricsStatus == "found")

  lyricsTokens <- lyricsData %>%
    select(SongID, SongName, ArtistName, MajorityGenre,
           MinorityGenre, Lyrics) %>%
    unnest_tokens(word, Lyrics)

  cleanedTokens <- lyricsTokens %>%
    anti_join(data.frame(word = stopwords), by = "word") %>%
    filter(!grepl("^\\d+$", word)) %>%
    filter(nchar(word) > 2) %>%
    filter(!grepl("[^a-zA-Z]", word))

  wordCounts <- cleanedTokens %>%
    count(word, sort = TRUE)

  write.csv(cleanedTokens,
            file.path(kOutputDir, "processed_lyrics.csv"),
            row.names = FALSE)
  write.csv(wordCounts,
            file.path(kOutputDir, "lyrics_word_counts.csv"),
            row.names = FALSE)

  return(cleanedTokens)
}


#' Tokenise and clean YouTube comment text.
#'
#' Strips HTML tags, then applies the same tokenisation and cleaning pipeline
#' used for lyrics.
#'
#' @param commentsData A data frame of raw comments (e.g. from
#'   \code{CollectYouTubeCommentsWithAPIKey()}).
#' @param stopwords    Character vector of stopwords to exclude.
#' @return A data frame of cleaned single-word tokens, or NULL if the input
#'   is empty.
ProcessComments <- function(commentsData, stopwords = GetStopwords()) {
  if (is.null(commentsData) || nrow(commentsData) == 0) {
    warning("No comments data available for processing.")
    return(NULL)
  }

  commentsData$CleanComment <- gsub("<.*?>", "", commentsData$Comment)

  commentTokens <- commentsData %>%
    select(SongID, VideoID, CommentID, Author,
           CleanComment, LikeCount) %>%
    unnest_tokens(word, CleanComment)

  cleanedTokens <- commentTokens %>%
    anti_join(data.frame(word = stopwords), by = "word") %>%
    filter(!grepl("^\\d+$", word)) %>%
    filter(nchar(word) > 2) %>%
    filter(!grepl("[^a-zA-Z]", word))

  wordCounts <- cleanedTokens %>%
    count(word, sort = TRUE)

  write.csv(cleanedTokens,
            file.path(kOutputDir, "processed_comments.csv"),
            row.names = FALSE)
  write.csv(wordCounts,
            file.path(kOutputDir, "comments_word_counts.csv"),
            row.names = FALSE)

  return(cleanedTokens)
}


#' Compute TF-IDF scores for words grouped by majority genre.
#'
#' Identifies the most characteristic words for each genre by calculating
#' term frequency–inverse document frequency.
#'
#' @param lyricsTokens A data frame of cleaned tokens from
#'   \code{ProcessLyrics()}.
#' @return A data frame with columns MajorityGenre, word, n, total, tf, idf,
#'   and tf_idf.
CalculateGenreTFIDF <- function(lyricsTokens) {
  genreWords <- lyricsTokens %>%
    count(MajorityGenre, word, sort = TRUE)

  totalWords <- genreWords %>%
    group_by(MajorityGenre) %>%
    summarize(total = sum(n))

  genreWords <- left_join(genreWords, totalWords, by = "MajorityGenre")

  genreTfIdf <- genreWords %>%
    bind_tf_idf(word, MajorityGenre, n)

  write.csv(genreTfIdf,
            file.path(kOutputDir, "genre_tf_idf.csv"),
            row.names = FALSE)

  return(genreTfIdf)
}


#' Extract bigrams or trigrams from lyrics text.
#'
#' Tokenises lyrics into n-grams, removes stopwords from each component word,
#' and returns the filtered result together with frequency counts saved to
#' disk.
#'
#' @param lyricsData A data frame produced by \code{CollectLyricsDirectly()}.
#' @param n          Integer (2 or 3). Size of the n-gram.
#' @param stopwords  Character vector of stopwords to exclude.
#' @return A data frame of cleaned n-grams with contextual song columns.
ExtractNgrams <- function(lyricsData, n = 2, stopwords = GetStopwords()) {
  lyricsData <- lyricsData %>%
    filter(LyricsStatus == "found")

  if (n == 2) {
    ngrams <- lyricsData %>%
      select(SongID, SongName, ArtistName, MajorityGenre,
             MinorityGenre, Lyrics) %>%
      unnest_tokens(bigram, Lyrics, token = "ngrams", n = 2)

    separated <- ngrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")

    filtered <- separated %>%
      filter(!word1 %in% stopwords,
             !word2 %in% stopwords,
             nchar(word1) > 2,
             nchar(word2) > 2,
             !grepl("^\\d+$", word1),
             !grepl("^\\d+$", word2))

    united <- filtered %>%
      unite(bigram, word1, word2, sep = " ")

    bigramCounts <- united %>%
      count(bigram, sort = TRUE)

    write.csv(united,
              file.path(kOutputDir, "lyrics_bigrams.csv"),
              row.names = FALSE)
    write.csv(bigramCounts,
              file.path(kOutputDir, "lyrics_bigram_counts.csv"),
              row.names = FALSE)

    return(united)

  } else if (n == 3) {
    ngrams <- lyricsData %>%
      select(SongID, SongName, ArtistName, MajorityGenre,
             MinorityGenre, Lyrics) %>%
      unnest_tokens(trigram, Lyrics, token = "ngrams", n = 3)

    separated <- ngrams %>%
      separate(trigram, c("word1", "word2", "word3"), sep = " ")

    filtered <- separated %>%
      filter(!word1 %in% stopwords,
             !word2 %in% stopwords,
             !word3 %in% stopwords,
             nchar(word1) > 2,
             nchar(word2) > 2,
             nchar(word3) > 2,
             !grepl("^\\d+$", word1),
             !grepl("^\\d+$", word2),
             !grepl("^\\d+$", word3))

    united <- filtered %>%
      unite(trigram, word1, word2, word3, sep = " ")

    trigramCounts <- united %>%
      count(trigram, sort = TRUE)

    write.csv(united,
              file.path(kOutputDir, "lyrics_trigrams.csv"),
              row.names = FALSE)
    write.csv(trigramCounts,
              file.path(kOutputDir, "lyrics_trigram_counts.csv"),
              row.names = FALSE)

    return(united)

  } else {
    stop("Parameter 'n' must be 2 (bigrams) or 3 (trigrams).")
  }
}
