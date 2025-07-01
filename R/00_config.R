# Configuration file - where all the boring shit goes


# API credentials - don't leak these or we're screwed
API_CREDENTIALS <- list(
    genius = list(
        # Fill in genius API - get your own if this one's dead
        client_id = "",
        client_secret = "",
        redirect_uri = ""
    ),
    youtube = list(
        # Fill in Google Cloud API - pray they don't rate-limit us
        app_name = "",
        client_id = "",
        client_secret = "",
        api_key = ""
    )
)

# Project settings - tweak at your own risk
PROJECT_SETTINGS <- list(
    csv_file_path = "song_list.csv",
    max_comments = 100,               # Max comments per song
    sentiment_lexicon = "bing",
    min_word_freq = 3,                # Minimum word frequency for word clouds
    max_words = 100                   # Maximum number of words in word clouds - keep it sane
)

# Define global file paths - where all the crap gets saved
BASE_DIR <- "."
DATA_DIR <- file.path(BASE_DIR, "Data")
LYRICS_DIR <- file.path(DATA_DIR, "Lyrics")
COMMENTS_DIR <- file.path(DATA_DIR, "Comments")
OUTPUT_DIR <- file.path(DATA_DIR, "Output")
VISUALS_DIR <- file.path(BASE_DIR, "Visualizations")

# Emotion categories and colors for visualization - make it pretty
EMOTION_COLORS <- list(
    joy = "#FFD700",
    trust = "#32CD32",
    fear = "#800080",
    surprise = "#FF8C00",
    sadness = "#4169E1",
    disgust = "#006400",
    anger = "#FF0000",
    anticipation = "#FF69B4"
)

# Sentiment colors for visualization
SENTIMENT_COLORS <- list(
    positive = "#4CAF50",
    negative = "#F44336",
    neutral = "#9E9E9E"
)

# Genre colors for visualization
GENRE_COLORS <- list(
    "Pop" = "#E91E63",
    "Rock" = "#2196F3",
    "Hip Hop" = "#FF9800",
    "R&B" = "#9C27B0",
    "Country" = "#8BC34A",
    "Electronic" = "#00BCD4",
    "Jazz" = "#FFC107",
    "Classical" = "#607D8B",
    "Folk" = "#795548",
    "Metal" = "#000000",
    "Reggae" = "#CDDC39",
    "Blues" = "#3F51B5",
    "Other" = "#9E9E9E"
)

# StopWords to exclude from text analysis - all the crap we don't care about
CUSTOM_STOPWORDS <- c(
    "feat", "ft", "chorus", "verse", "bridge", "intro", "outro", "pre-chorus",
    "hook", "refrain", "interlude", "remix", "edit", "version", "remaster",
    "explicit", "clean", "instrumental", "acoustic", "live", "studio",
    "yeah", "hey", "oh", "uh", "mm", "hmm", "ah", "la", "na", "da", "oo", "woah",
    "gonna", "gotta", "wanna", "cuz", "cause", "ain't", "y'all"
)

# Create paths object for easier reference
PATHS <- list(
    base = BASE_DIR,
    data = DATA_DIR,
    lyrics = LYRICS_DIR,
    comments = COMMENTS_DIR,
    output = OUTPUT_DIR,
    visuals = VISUALS_DIR
)
