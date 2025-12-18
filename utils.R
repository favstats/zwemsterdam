
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
get_sportfondsen_timetable <- function(base_url, pool_name) {
  
  # Try the tijden-tarieven page directly
  dodo <- GET(paste0(base_url, "/tijden-tarieven/"), user_agent("Mozilla/5.0"))
  if (status_code(dodo) != 200) {
     # try alternative URL
     dodo <- GET(paste0(base_url, "/tijden-en-tarieven/"), user_agent("Mozilla/5.0"))
  }
  
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
    
    map_dfr(slots, ~{
      # Check if required fields exist
      if (is.null(.x$startTime) || is.null(.x$endTime)) return(NULL)
      
      tibble(
        bad = pool_name,
        dag = .x$day,
        date = NA, # Sportfondsen doesn't provide specific dates
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
