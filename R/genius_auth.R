# genius_auth.R
# ---------------------------------------------------------------------------
# Genius API OAuth 2.0 authentication and authenticated GET helper.
# ---------------------------------------------------------------------------

library(httr)
library(jsonlite)


#' Provide a default value when the left-hand side is NULL.
#'
#' A standard null-coalescing operator for use with optional configuration
#' values.
#'
#' @param x Value to check.
#' @param y Default value to return when \code{x} is NULL.
#' @return \code{x} if it is not NULL; otherwise \code{y}.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Authenticate with the Genius API via OAuth 2.0.
#'
#' Opens a browser-based consent flow and returns a reusable token object.
#'
#' @param clientId     Character. Genius API client ID.
#' @param clientSecret Character. Genius API client secret.
#' @param redirectUri  Character. Redirect URI registered with the Genius
#'   application.  Defaults to \code{"http://localhost:1410/"}.
#' @return An httr OAuth 2.0 token object.
GeniusOAuth <- function(clientId,
                        clientSecret,
                        redirectUri = "http://localhost:1410/") {
  endpoint <- oauth_endpoint(
    authorize = "https://api.genius.com/oauth/authorize",
    access    = "https://api.genius.com/oauth/token"
  )

  app <- oauth_app(
    "genius",
    clientId,
    clientSecret,
    redirect_uri = redirectUri
  )

  token <- oauth2.0_token(
    endpoint = endpoint,
    app      = app,
    scope    = "me",
    cache    = TRUE
  )

  return(token)
}


#' Make an authenticated GET request to the Genius API.
#'
#' Appends the given endpoint path to the Genius base URL, attaches the
#' OAuth token, and returns the parsed JSON response body.
#'
#' @param endpoint Character. Relative API path (e.g. \code{"songs/123"}).
#' @param token    An httr OAuth 2.0 token returned by \code{GeniusOAuth()}.
#' @param ...      Additional arguments forwarded to \code{httr::GET()}.
#' @return A list representing the \code{response} node of the parsed JSON.
GeniusGet <- function(endpoint, token, ...) {
  baseUrl  <- "https://api.genius.com/"
  response <- GET(
    url    = paste0(baseUrl, endpoint),
    config = config(token = token),
    ...
  )

  if (http_error(response)) {
    stop(
      sprintf(
        "Genius API request failed [%d]: %s",
        status_code(response),
        content(response, "text", encoding = "UTF-8")
      )
    )
  }

  parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(parsed$response)
}
