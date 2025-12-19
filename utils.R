
library(httr)
library(tidyverse)
library(lubridate)

# Helper to aggregate timeframes if needed (optional for all activities)
aggregate_timeframes <- function(df) {
  df %>%
    arrange(start) %>%
    group_by(bad, dag, activity, extra) %>%
    summarise(
      start = min(start),
      end = max(end),
      .groups = "drop"
    )
}

# New function for Amsterdam Municipal Pools
get_amsterdam_timetables <- function(pool_slug) {
  # Get dates for the next 7 days
  dates <- seq(Sys.Date(), Sys.Date() + 7, by = "1 day")
  
  all_schedules <- map_dfr(dates, ~{
    date_str <- format(.x, "%Y-%m-%d")
    url <- paste0("https://zwembaden.api-amsterdam.nl/nl/api/", pool_slug, "/date/", date_str, "/")
    
    response <- GET(url)
    if (status_code(response) != 200) return(NULL)
    
    res_content <- content(response, "parsed")
    if (is.null(res_content$schedule) || length(res_content$schedule) == 0) return(NULL)
    
    map_dfr(res_content$schedule, ~{
      # Convert "7.00" to decimal 7.0, "15.30" to 15.5
      parse_time_dec <- function(t_str) {
        parts <- str_split(t_str, "\\.")[[1]]
        h <- as.numeric(parts[1])
        m <- if(length(parts) > 1) as.numeric(parts[2]) else 0
        return(h + m/60)
      }
      
      tibble(
        bad = res_content$pool,
        dag = .x$dow,
        date = date_str,
        activity = .x$activity,
        extra = .x$extra,
        start = parse_time_dec(.x$start),
        end = parse_time_dec(.x$end)
      )
    })
  })
  
  return(all_schedules)
}

# Function for Het Marnix
get_marnix_timetable <- function() {
  url <- "https://hetmarnix.nl/wp-admin/admin-ajax.php"
  
  # Current week
  start_date <- floor_date(Sys.Date(), "week", week_start = 1)
  end_date <- start_date + days(7)
  
  params <- list(
    action = "getlessons",
    start = format(start_date, "%Y-%m-%dT00:00:00Z"),
    end = format(end_date, "%Y-%m-%dT23:59:59Z")
  )
  
  response <- GET(url, query = params)
  if (status_code(response) != 200) return(NULL)
  
  marnixres <- content(response, "parsed")
  
  if (is.null(marnixres) || length(marnixres) == 0) return(NULL)
  
  map_dfr(marnixres, ~{
    # Handle nested structure - use purrr::flatten explicitly
    item <- purrr::flatten(.x)
    # Filter only relevant fields
    if (is.null(item$start) || is.null(item$title)) return(NULL)
    
    start_time <- ymd_hms(item$start)
    end_time <- ymd_hms(item$end)
    
    tibble(
      bad = "Het Marnix",
      dag = format(start_time, "%A"),
      date = as.character(as.Date(start_time)),
      activity = item$title,
      extra = "",
      start = hour(start_time) + minute(start_time)/60,
      end = hour(end_time) + minute(end_time)/60
    )
  }) %>%
    # Translate Dutch days if necessary, but format(%A) depends on locale. 
    # Let's normalize days to Dutch.
    mutate(dag = case_when(
      str_detect(dag, "Monday|maandag") ~ "Maandag",
      str_detect(dag, "Tuesday|dinsdag") ~ "Dinsdag",
      str_detect(dag, "Wednesday|woensdag") ~ "Woensdag",
      str_detect(dag, "Thursday|donderdag") ~ "Donderdag",
      str_detect(dag, "Friday|vrijdag") ~ "Vrijdag",
      str_detect(dag, "Saturday|zaterdag") ~ "Zaterdag",
      str_detect(dag, "Sunday|zondag") ~ "Zondag",
      TRUE ~ dag
    ))
}

# Function for Sportfondsen (Oost, Mercator, etc.)
# NOTE: The Sportfondsen websites have migrated to new URL structure:
# - sportfondsenbadamsterdamoost.nl -> amsterdamoost.sportfondsen.nl
# - sportplazamercator.nl -> mercator.sportfondsen.nl
# Each pool has its own URL path for the schedule page
get_sportfondsen_timetable <- function(base_url, pool_name, schedule_path = "/tijden-tarieven/") {
  
  # Use the provided path for the schedule page
  full_url <- paste0(base_url, schedule_path)
  message(paste("Fetching", pool_name, "from", full_url))
  
  dodo <- GET(full_url, user_agent("Mozilla/5.0"))
  
  if (status_code(dodo) != 200) {
    message(paste("Could not fetch schedule page for", pool_name, "- Status:", status_code(dodo)))
    return(NULL)
  }
  
  page_content <- content(dodo, "text")
  
  # Extract __NEXT_DATA__ which contains the schedule data
  next_data <- str_extract(page_content, '<script id="__NEXT_DATA__" type="application/json">[^<]+</script>')
  if (is.na(next_data)) {
    message(paste("Could not find __NEXT_DATA__ for", pool_name))
    return(NULL)
  }
  
  json_str <- str_replace_all(next_data, '<script id="__NEXT_DATA__" type="application/json">|</script>', '')
  
  tryCatch({
    data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    
    # Extract scheduleData from extraPageProps
    slots <- data$props$pageProps$extraPageProps$scheduleData$timeSlots
    if (is.null(slots)) slots <- data$props$pageProps$scheduleData$timeSlots
    
    if (is.null(slots) || length(slots) == 0) {
      message(paste("No schedule data found for", pool_name))
      return(NULL)
    }
    
    message(paste("Found", length(slots), "time slots for", pool_name))
    
    # Calculate dates for current week based on day names
    week_start <- floor_date(Sys.Date(), "week", week_start = 1)
    day_to_date <- c(
      "Maandag" = 0, "Dinsdag" = 1, "Woensdag" = 2, "Donderdag" = 3,
      "Vrijdag" = 4, "Zaterdag" = 5, "Zondag" = 6
    )
    
    map_dfr(slots, ~{
      # Check if required fields exist
      if (is.null(.x$startTime) || is.null(.x$endTime)) return(NULL)
      
      # Calculate date from day name
      dag <- .x$day
      day_offset <- day_to_date[dag]
      slot_date <- if (!is.na(day_offset)) as.character(week_start + days(day_offset)) else NA
      
      tibble(
        bad = pool_name,
        dag = dag,
        date = slot_date,
        activity = if(!is.null(.x$activitySchedule$activity$title)) .x$activitySchedule$activity$title else "Onbekend",
        extra = if(!is.null(.x$occupationDisplay)) .x$occupationDisplay else "",
        start = as.numeric(str_replace(.x$startTime, ":", "")) / 100,
        end = as.numeric(str_replace(.x$endTime, ":", "")) / 100
      )
    }) %>%
      mutate(
        # Convert time format: 12:00 -> 1200/100 = 12.0, 12:30 -> 1230/100 = 12.30 -> need to convert minutes
        start = floor(start) + (start %% 1 * 100 / 60),
        end = floor(end) + (end %% 1 * 100 / 60)
      )
  }, error = function(e) {
    message(paste("Error parsing data for", pool_name, ":", e$message))
    return(NULL)
  })
}

# Function to read Optisport data (fetched by Playwright script)
# The Playwright script must be run first: node explore_optisport.js
get_optisport_data <- function(json_path = "data/optisport_data.json") {
  if (!file.exists(json_path)) {
    message("Optisport data not found. Run 'node explore_optisport.js' first.")
    return(NULL)
  }
  
  data <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  
  all_sessions <- map_dfr(names(data), function(pool_name) {
    pool_data <- data[[pool_name]]
    events <- pool_data$events
    
    if (is.null(events) || length(events) == 0) return(NULL)
    
    map_dfr(events, function(event) {
      tryCatch({
        # Parse ISO datetime
        start_dt <- ymd_hms(event$start, tz = "Europe/Amsterdam")
        end_dt <- ymd_hms(event$end, tz = "Europe/Amsterdam")
        
        # Get Dutch day name
        day_num <- wday(start_dt, week_start = 1)
        dag <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")[day_num]
        
        tibble(
          bad = pool_name,
          dag = dag,
          date = as.character(as.Date(start_dt)),
          activity = event$title,
          extra = event$internalLocation %||% "",
          start = hour(start_dt) + minute(start_dt) / 60,
          end = hour(end_dt) + minute(end_dt) / 60
        )
      }, error = function(e) {
        return(NULL)
      })
    })
  })
  
  message(paste("Loaded", nrow(all_sessions), "Optisport sessions"))
  return(all_sessions)
}

# Function for Optisport pools (Bijlmer, Sloterparkbad, etc.)
# Uses their API: https://www.optisport.nl/api/optisport/v1/schedule
get_optisport_timetable <- function(location_id, pool_name, referer_slug) {
  url <- "https://www.optisport.nl/api/optisport/v1/schedule"
  
  # We need to fetch multiple pages to get all results
  all_results <- list()
  page <- 1
  max_pages <- 10  # Safety limit
  
  while (page <= max_pages) {
    body <- list(
      page = page,
      locationId = location_id,
      results = 50  # Get more results per page
    )
    
    response <- tryCatch({
      POST(
        url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        content_type_json(),
        add_headers(
          "Accept" = "*/*",
          "Referer" = paste0("https://www.optisport.nl/", referer_slug)
        ),
        user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36")
      )
    }, error = function(e) {
      message(paste("Error fetching Optisport data for", pool_name, ":", e$message))
      return(NULL)
    })
    
    if (is.null(response) || status_code(response) != 200) {
      if (page == 1) {
        message(paste("Could not fetch Optisport schedule for", pool_name, "- Status:", 
                      if(!is.null(response)) status_code(response) else "NULL"))
      }
      break
    }
    
    res_content <- content(response, "parsed")
    
    # Check if we have results
    if (is.null(res_content$results) || length(res_content$results) == 0) {
      break
    }
    
    all_results <- c(all_results, res_content$results)
    
    # Check if there are more pages
    if (is.null(res_content$pages) || page >= res_content$pages) {
      break
    }
    
    page <- page + 1
  }
  
  if (length(all_results) == 0) {
    message(paste("No schedule data found for", pool_name))
    return(NULL)
  }
  
  message(paste("Found", length(all_results), "schedule items for", pool_name))
  
  # Parse the results
  map_dfr(all_results, ~{
    tryCatch({
      # Parse datetime
      start_dt <- ymd_hms(.x$start, tz = "Europe/Amsterdam")
      end_dt <- ymd_hms(.x$end, tz = "Europe/Amsterdam")
      
      # Get day name in Dutch
      day_num <- wday(start_dt, week_start = 1)
      dag <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")[day_num]
      
      tibble(
        bad = pool_name,
        dag = dag,
        date = as.character(as.Date(start_dt)),
        activity = .x$activity$name %||% "Onbekend",
        extra = .x$description %||% "",
        start = hour(start_dt) + minute(start_dt) / 60,
        end = hour(end_dt) + minute(end_dt) / 60
      )
    }, error = function(e) {
      return(NULL)
    })
  })
}
