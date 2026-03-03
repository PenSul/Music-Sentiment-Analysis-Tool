# Music Sentiment Analysis Tool

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://www.r-project.org/)

A text-mining and sentiment-analysis pipeline that examines emotional patterns
in song lyrics and YouTube comments across music genres. The tool scrapes
lyrics from Genius, collects YouTube comments via the Data API, runs
sentiment analysis with multiple lexicons, and produces static and
interactive visualisations of the results.

---

## Table of Contents

1. [Features](#features)
2. [Prerequisites](#prerequisites)
3. [Installation](#installation)
4. [Configuration](#configuration)
5. [Usage](#usage)
6. [Project Structure](#project-structure)
7. [Output](#output)
8. [Troubleshooting](#troubleshooting)
9. [License](#license)

---

## Features

- **Multi-method lyrics retrieval** — direct Genius web scraping with a
  Python `lyricsgenius` fallback via `reticulate`.
- **YouTube comment collection** — paginated fetching through the YouTube
  Data API v3 with automatic rate-limit pausing.
- **Flexible sentiment analysis** — supports the Bing, NRC, AFINN, and
  Loughran lexicons from the `tidytext` ecosystem.
- **Genre-level comparison** — aggregates sentiment and emotion scores by
  majority genre for cross-genre insight.
- **Lyrics-versus-comments comparison** — measures how listener sentiment
  in comments aligns with or diverges from lyric sentiment.
- **Rich visualisations** — word clouds, emotion heatmaps, radar charts,
  sentiment landscape maps, lexical-diversity plots, and an emotional-impact
  chart, each saved as both PNG and interactive HTML (Plotly).

---

## Prerequisites

| Dependency | Version | Purpose |
|---|---|---|
| **R** | ≥ 4.0.0 | Core runtime |
| **RStudio** | latest (recommended) | IDE with `.Rproj` support |
| **Python** | 3.x | `lyricsgenius` lyrics fallback |
| **pip package `lyricsgenius`** | latest | Python-based Genius scraper |
| **Google Cloud project** | — | YouTube Data API v3 key and OAuth |
| **Genius API client** | — | OAuth credentials for lyrics search |

---

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/PenSul/Music-Sentiment-Analysis-Tool.git
cd Music-Sentiment-Analysis-Tool
```

### 2. Install R Packages

Open R or RStudio and run:

```r
install.packages(c(
  "tidyverse", "tidytext", "tuber", "rvest", "httr", "jsonlite",
  "textdata", "sentimentr", "wordcloud", "plotly", "knitr",
  "rmarkdown", "dplyr", "stringr", "ggplot2", "scales",
  "ggrepel", "htmlwidgets", "RColorBrewer", "reticulate"
))
```

### 3. Install the Python Dependency

```bash
pip install lyricsgenius
```

Verify the installation:

```bash
pip list | grep lyricsgenius
```

---

## Configuration

All credentials and tuneable parameters live in `R/config.R`.

### API Credentials

Open `R/config.R` and fill in the credential placeholders:

```r
kApiCredentials <- list(
  genius = list(
    client_id     = "<YOUR_GENIUS_CLIENT_ID>",
    client_secret = "<YOUR_GENIUS_CLIENT_SECRET>",
    redirect_uri  = "http://localhost:1410/"
  ),
  youtube = list(
    app_name      = "<YOUR_APP_NAME>",
    client_id     = "<YOUR_GOOGLE_CLIENT_ID>",
    client_secret = "<YOUR_GOOGLE_CLIENT_SECRET>",
    api_key       = "<YOUR_YOUTUBE_API_KEY>"
  )
)
```

> **Important:** Never commit real credentials. Add `R/config.R` to
> `.gitignore` or use environment variables in production.

### Project Settings

Adjust the settings block in the same file to control comment limits,
the sentiment lexicon, and word-cloud thresholds.

---

## Usage

1. Open the project in RStudio (double-click `Music Sentiment Analysis.Rproj`).
2. Make sure the working directory is the project root.
3. Verify that `song_list.csv` is present at the project root.
4. Run the entry-point script:

```r
source("main.R")
```

5. When prompted, authenticate with Google/YouTube and Genius through the
   browser windows that open automatically.
6. Wait for the pipeline to finish. Progress messages are printed to the
   console at every major step.

---

## Project Structure

```
.
├── main.R                        # Entry point — orchestrates the full pipeline
├── song_list.csv                 # Input song catalogue (CSV)
├── R/
│   ├── config.R                  # Global constants, API credentials, colours
│   ├── setup.R                   # Directory creation, package checks, stopwords
│   ├── data_collection.R         # CSV reader, API auth, lyrics + comments collection
│   ├── text_processing.R         # Tokenisation, stopword removal, TF-IDF, n-grams
│   ├── sentiment_analysis.R      # Lexicon-based sentiment scoring and comparison
│   ├── visualization.R           # All static and interactive plot generators
│   ├── genius_auth.R             # Genius OAuth 2.0 helper
│   ├── lyrics_scraper.R          # Web-scraping and Python-fallback lyrics retrieval
│   └── youtube_scraper.R         # Direct YouTube Data API comment fetcher
├── Data/                         # Created at runtime
│   ├── Lyrics/                   # Individual lyrics text files
│   ├── Comments/                 # Per-song comment CSVs
│   └── Output/                   # Processed CSVs and analysis results
├── Visualizations/               # Created at runtime — PNGs and interactive HTMLs
├── LICENSE
├── README.md
└── .gitignore
```

---

## Output

After a successful run, the pipeline produces:

| Directory | Contents |
|---|---|
| `Data/Lyrics/` | One `.txt` file per song containing scraped lyrics. |
| `Data/Comments/` | One `_comments.csv` file per song with YouTube comment data. |
| `Data/Output/` | Merged CSVs for lyrics, comments, sentiment scores, TF-IDF, bigrams, genre comparisons, and emotional-impact insights. |
| `Visualizations/` | PNG images and self-contained interactive HTML files for every chart type. |

Open any `_interactive.html` file in a browser to explore the Plotly
visualisations.

---

## Troubleshooting

### API Authentication

- Confirm credentials in `R/config.R` match those in the Google Cloud
  Console and Genius API dashboard.
- If the OAuth browser window does not appear, try authenticating manually:

  ```r
  library(tuber)
  yt_oauth("<client_id>", "<client_secret>")
  ```

### Python Integration

- Ensure `python` is on your system `PATH`.
- If `reticulate` cannot find Python, set the path explicitly:

  ```r
  reticulate::use_python("/path/to/python")
  ```

### Missing R Packages

Run the built-in check:

```r
source("R/setup.R")
CheckAndInstallPackages()
```

### Non-English Songs

Lyrics retrieval from Genius may be unreliable for CJK or other
non-Latin titles. The pipeline logs each failure and continues to the
next song. A manual-lyrics fallback is available for testing purposes.

---

## License

This project is released under the [MIT License](LICENSE).
