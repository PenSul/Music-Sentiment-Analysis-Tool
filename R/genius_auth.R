# Genius API authentication function - oh boy, another API to fight with


#' Operator for providing default values for NULL
#'
#' @param x Value to check
#' @param y Default value to use if x is NULL
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Authenticate with Genius API using OAuth 2.0
#'
#' @param client_id Genius API Client ID
#' @param client_secret Genius API Client Secret  
#' @param redirect_uri Default: http://localhost:1410/
#' @return OAuth token object
#' @import httr
genius_oauth <- function(client_id, client_secret, redirect_uri = "http://localhost:1410/") {
    library(httr)
    
    # Genius OAuth endpoints
    genius <- oauth_endpoint(
        authorize = "https://api.genius.com/oauth/authorize",
        access = "https://api.genius.com/oauth/token"
    )
    
    # Create an OAuth application object
    app <- oauth_app("genius", client_id, client_secret, redirect_uri = redirect_uri)
    
    # Get OAuth token
    token <- oauth2.0_token(
        endpoint = genius,
        app = app,
        scope = "me",
        cache = TRUE
    )
    return(token)
}

#' Make an authenticated request to Genius API
#'
#' @param endpoint API endpoint path (e.g., "songs/123")
#' @param token OAuth token from genius_oauth()
#' @param ... Additional parameters to pass to httr::GET
#' @return Parsed JSON response
#' @import httr jsonlite
genius_get <- function(endpoint, token, ...) {
    library(httr)
    library(jsonlite)
    
    base_url <- "https://api.genius.com/"
    
    # Make request with the proper authentication
    response <- GET(
        url = paste0(base_url, endpoint),
        config = config(token = token),
        ...
    )
    
    # Check errors
    if (http_error(response)) {
        stop(
            "Genius API request failed [", status_code(response), "]\n", 
            content(response, "text", encoding = "UTF-8"), " - Fucking Genius API!"
        )
    }
    
    # Parse and return response
    parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(parsed$response)
}