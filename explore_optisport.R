# Exploration script for Optisport API
# Testing Bijlmer Sportcentrum data retrieval

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)

# Bijlmer page URL and API
bijlmer_page <- "https://www.optisport.nl/zwembad-bijlmer-amsterdam-zuidoost"
api_url <- "https://www.optisport.nl/api/optisport/v1/schedule"
bijlmer_location_id <- 2202

# Step 1: First, let's try to get the page and extract CSRF token
print("=== Step 1: Fetching main page to get CSRF token ===")

# Create a session to maintain cookies
session <- httr::handle("https://www.optisport.nl")

page_response <- GET(
  bijlmer_page,
  handle = session,
  user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
  add_headers(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9"
  )
)

print(paste("Page status:", status_code(page_response)))

if (status_code(page_response) == 200) {
  page_content <- content(page_response, "text", encoding = "UTF-8")
  
  # Look for CSRF token in the page
  # Usually in a meta tag or hidden input
  csrf_patterns <- c(
    'name="csrf-token" content="([^"]+)"',
    'name="csrf_token" value="([^"]+)"',
    '"csrfToken":"([^"]+)"',
    '"csrf_token":"([^"]+)"',
    'data-csrf="([^"]+)"',
    'X-CSRF-Token["\']?:\\s*["\']([^"\']+)'
  )
  
  csrf_token <- NULL
  for (pattern in csrf_patterns) {
    match <- str_match(page_content, pattern)
    if (!is.na(match[1, 2])) {
      csrf_token <- match[1, 2]
      print(paste("Found CSRF token with pattern:", pattern))
      break
    }
  }
  
  if (!is.null(csrf_token)) {
    print(paste("CSRF Token:", substr(csrf_token, 1, 50), "..."))
  } else {
    print("No CSRF token found in page")
    
    # Let's look for any token-like strings
    print("\n=== Searching for potential tokens in page ===")
    
    # Check for Drupal-style CSRF (in settings)
    drupal_match <- str_match(page_content, '"X-CSRF-Token":\\s*"([^"]+)"')
    if (!is.na(drupal_match[1, 2])) {
      print(paste("Found Drupal CSRF:", drupal_match[1, 2]))
      csrf_token <- drupal_match[1, 2]
    }
    
    # Look in script tags for any csrf references
    scripts <- str_extract_all(page_content, '<script[^>]*>.*?</script>')[[1]]
    for (i in seq_along(scripts)) {
      if (str_detect(scripts[i], regex("csrf", ignore_case = TRUE))) {
        print(paste("\nScript", i, "contains 'csrf':"))
        # Extract just the relevant part
        csrf_part <- str_extract(scripts[i], ".{0,100}csrf.{0,100}")
        if (!is.na(csrf_part)) print(csrf_part)
      }
    }
  }
  
  # Check cookies we got
  print("\n=== Cookies received ===")
  cookies_list <- cookies(page_response)
  if (nrow(cookies_list) > 0) {
    print(cookies_list)
  } else {
    print("No cookies in response")
  }
  
  # Try the API with the session
  print("\n\n=== Step 2: Trying API with session cookies ===")
  
  body <- list(
    page = 1,
    locationId = bijlmer_location_id,
    results = 20
  )
  
  # Build headers
  api_headers <- c(
    "Accept" = "application/json",
    "Content-Type" = "application/json",
    "Referer" = bijlmer_page,
    "Origin" = "https://www.optisport.nl"
  )
  
  if (!is.null(csrf_token)) {
    api_headers["X-CSRF-Token"] <- csrf_token
  }
  
  api_response <- POST(
    api_url,
    handle = session,
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json(),
    add_headers(.headers = api_headers),
    user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36")
  )
  
  print(paste("API status:", status_code(api_response)))
  
  if (status_code(api_response) == 200) {
    api_content <- content(api_response, "parsed")
    print(paste("Success! Results:", length(api_content$results)))
    print(paste("Pages:", api_content$pages))
    
    if (length(api_content$results) > 0) {
      print("\n=== First 5 results ===")
      for (i in 1:min(5, length(api_content$results))) {
        r <- api_content$results[[i]]
        print(paste(i, ":", r$start, "-", r$end, "|", r$activity$name))
      }
    }
  } else {
    print("API request failed")
    print(substr(content(api_response, "text"), 1, 500))
  }
  
} else if (status_code(page_response) == 403) {
  print("Got Cloudflare challenge (403)")
  print("The page itself is Cloudflare protected")
  print(substr(content(page_response, "text"), 1, 500))
} else {
  print(paste("Unexpected status:", status_code(page_response)))
}

# Also try: look for __NEXT_DATA__ or similar embedded data
print("\n\n=== Step 3: Checking for embedded schedule data ===")
if (exists("page_content")) {
  next_data <- str_extract(page_content, '<script id="__NEXT_DATA__"[^>]*>.*?</script>')
  if (!is.na(next_data)) {
    print("Found __NEXT_DATA__ - this is a Next.js app!")
    # Extract and parse
    json_str <- str_replace_all(next_data, '<script id="__NEXT_DATA__"[^>]*>|</script>', '')
    data <- fromJSON(json_str, simplifyVector = FALSE)
    print(paste("Build ID:", data$buildId))
    print(names(data$props$pageProps))
  } else {
    print("No __NEXT_DATA__ found - not a Next.js app")
  }
  
  # Check for Drupal/Nuxt data
  drupal_settings <- str_extract(page_content, 'drupalSettings\\s*=\\s*\\{[^}]+\\}')
  if (!is.na(drupal_settings)) {
    print("\nFound Drupal settings!")
    print(substr(drupal_settings, 1, 500))
  }
}

