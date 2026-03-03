# main.R
# ---------------------------------------------------------------------------
# Entry point for the Music Sentiment Analysis pipeline.
#
# Execution order:
#   1. Environment setup (directories, packages)
#   2. Data ingestion (CSV song list)
#   3. API authentication
#   4. Lyrics collection (web scraping + Python fallback)
#   5. YouTube comment collection (API key method)
#   6. Text tokenisation and cleaning
#   7. TF-IDF and n-gram extraction
#   8. Sentiment analysis (lyrics + comments)
#   9. Genre-level and cross-source comparison
#  10. Visualisation generation
# ---------------------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(tuber)
library(rvest)
library(httr)
library(jsonlite)
library(textdata)
library(sentimentr)
library(wordcloud)
library(plotly)
library(knitr)
library(rmarkdown)
library(dplyr)

source("R/config.R")
source("R/setup.R")
source("R/data_collection.R")
source("R/text_processing.R")
source("R/sentiment_analysis.R")
source("R/visualization.R")
source("R/genius_auth.R")
source("R/lyrics_scraper.R")
source("R/youtube_scraper.R")

# ---- Step 0: Prepare environment -----------------------------------------
cat("Setting up project environment...\n")
CreateDirectories()
CheckAndInstallPackages()

# ---- Step 1: Read song catalogue -----------------------------------------
cat("Reading song data...\n")
songData <- ReadSongList()

# ---- Step 2: Authenticate with APIs --------------------------------------
cat("Setting up API authentication...\n")
apiTokens <- SetupAPITokens()

# ---- Step 3: Collect lyrics ----------------------------------------------
cat("Collecting lyrics data...\n")
lyricsData <- CollectLyricsDirectly(songData)

# Fall back to placeholder lyrics if scraping found nothing.
if (nrow(lyricsData) == 0 ||
    !any(lyricsData$LyricsStatus == "found")) {
  cat("No lyrics found via scraping. ",
      "Using sample lyrics for testing.\n")
  lyricsData <- AddManualLyrics(songData)
}

# ---- Step 4: Collect YouTube comments ------------------------------------
cat("Collecting YouTube comments...\n")
commentsData <- CollectYouTubeCommentsWithAPIKey(
  songData, kProjectSettings$max_comments
)

# ---- Steps 5–10: Analysis and visualisation (lyrics required) ------------
if (nrow(lyricsData) > 0 &&
    any(lyricsData$LyricsStatus == "found")) {

  # Step 5: Tokenise text.
  cat("Processing lyrics text...\n")
  stopwords     <- GetStopwords()
  lyricsTokens  <- ProcessLyrics(lyricsData, stopwords)

  commentTokens <- NULL
  if (!is.null(commentsData) && nrow(commentsData) > 0) {
    cat("Processing comments text...\n")
    commentTokens <- ProcessComments(commentsData, stopwords)
  }

  # Step 6: TF-IDF by genre.
  cat("Calculating TF-IDF by genre...\n")
  genreTfIdf <- CalculateGenreTFIDF(lyricsTokens)

  # Step 7: Bigrams.
  cat("Extracting bigrams from lyrics...\n")
  lyricsBigrams <- ExtractNgrams(lyricsData, n = 2, stopwords)

  # Step 8: Sentiment analysis.
  cat("Analysing lyrics sentiment...\n")
  lyricsSentiment <- AnalyzeLyricsSentiment(lyricsTokens)

  commentsSentiment <- NULL
  if (!is.null(commentTokens) && nrow(commentTokens) > 0) {
    cat("Analysing comments sentiment...\n")
    commentsSentiment <- AnalyzeCommentsSentiment(commentTokens)
  }

  # Step 9: Comparisons.
  cat("Comparing sentiment across genres...\n")
  genreSentiment <- CompareSentimentByGenre(lyricsSentiment)

  lyricsCommentsComparison <- NULL
  if (!is.null(commentsSentiment)) {
    cat("Comparing lyrics and comments sentiment...\n")
    lyricsCommentsComparison <- CompareLyricsAndComments(
      lyricsSentiment, commentsSentiment
    )
  }

  # Step 10: Visualisations.
  cat("Generating visualisations...\n")

  CreateGenreWordClouds(lyricsTokens)

  CreateSentimentPlots(lyricsSentiment, "Lyrics")
  if (!is.null(commentsSentiment)) {
    CreateSentimentPlots(commentsSentiment, "Comments")
  }

  CreateGenreComparisonPlots(genreSentiment)

  if (!is.null(lyricsCommentsComparison)) {
    CreateLyricsCommentsComparisonPlot(lyricsCommentsComparison)
  }

  CreateEmotionHeatmap(lyricsSentiment)
  CreateYouTubeMetricsViz(songData, lyricsSentiment, commentsData)
  CreateSentimentTrendViz(songData, lyricsSentiment)
  CreateLexicalDiversityEmotionPlot(lyricsTokens, lyricsSentiment)
  CreateEmotionRadarChart(lyricsSentiment)

  if (!is.null(commentsSentiment)) {
    cat("Creating emotional impact chart...\n")
    CreateEmotionalImpactChart(lyricsSentiment, commentsSentiment)
  }

  cat("Analysis complete.\n")

} else {
  stop("No lyrics were found. Cannot continue with analysis.")
}
