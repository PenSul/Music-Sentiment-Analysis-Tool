# config.R
# ---------------------------------------------------------------------------
# Global constants, API credentials, file paths, colour palettes, and custom
# stopwords used throughout the Music Sentiment Analysis pipeline.
# ---------------------------------------------------------------------------

# ---- API Credentials ------------------------------------------------------
# Fill in before running the pipeline.  Never commit real secrets to version
# control; consider sourcing from environment variables in production.

kApiCredentials <- list(
  genius = list(
    client_id     = "",
    client_secret = "",
    redirect_uri  = ""
  ),
  youtube = list(
    app_name      = "",
    client_id     = "",
    client_secret = "",
    api_key       = ""
  )
)

# ---- Project Settings -----------------------------------------------------

kProjectSettings <- list(
  csv_file_path    = "song_list.csv",
  max_comments     = 100,
  sentiment_lexicon = "bing",
  min_word_freq    = 3,
  max_words        = 100
)

# ---- Directory Paths ------------------------------------------------------

kBaseDir     <- "."
kDataDir     <- file.path(kBaseDir, "Data")
kLyricsDir   <- file.path(kDataDir, "Lyrics")
kCommentsDir <- file.path(kDataDir, "Comments")
kOutputDir   <- file.path(kDataDir, "Output")
kVisualsDir  <- file.path(kBaseDir, "Visualizations")

# ---- Colour Palettes for Visualisations -----------------------------------

kEmotionColors <- list(
  joy          = "#FFD700",
  trust        = "#32CD32",
  fear         = "#800080",
  surprise     = "#FF8C00",
  sadness      = "#4169E1",
  disgust      = "#006400",
  anger        = "#FF0000",
  anticipation = "#FF69B4"
)

kSentimentColors <- list(
  positive = "#4CAF50",
  negative = "#F44336",
  neutral  = "#9E9E9E"
)

kGenreColors <- list(
  "Pop"        = "#E91E63",
  "Rock"       = "#2196F3",
  "Hip Hop"    = "#FF9800",
  "R&B"        = "#9C27B0",
  "Country"    = "#8BC34A",
  "Electronic" = "#00BCD4",
  "Jazz"       = "#FFC107",
  "Classical"  = "#607D8B",
  "Folk"       = "#795548",
  "Metal"      = "#000000",
  "Reggae"     = "#CDDC39",
  "Blues"       = "#3F51B5",
  "Other"      = "#9E9E9E"
)

# ---- Custom Stopwords -----------------------------------------------------
# Supplementary words to exclude during tokenisation, on top of the standard
# English stopwords provided by tidytext.

kCustomStopwords <- c(
  "feat", "ft", "chorus", "verse", "bridge", "intro", "outro",
  "pre-chorus", "hook", "refrain", "interlude", "remix", "edit",
  "version", "remaster", "explicit", "clean", "instrumental",
  "acoustic", "live", "studio",
  "yeah", "hey", "oh", "uh", "mm", "hmm", "ah", "la", "na",
  "da", "oo", "woah",
  "gonna", "gotta", "wanna", "cuz", "cause", "ain't", "y'all"
)

# ---- Convenience Path List ------------------------------------------------

kPaths <- list(
  base     = kBaseDir,
  data     = kDataDir,
  lyrics   = kLyricsDir,
  comments = kCommentsDir,
  output   = kOutputDir,
  visuals  = kVisualsDir
)
