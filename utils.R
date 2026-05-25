
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

zwemsterdam_day_names <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")

get_dutch_day_name <- function(date) {
  zwemsterdam_day_names[wday(date, week_start = 1)]
}

parse_pool_time <- function(time_str) {
  clean_time <- str_replace_all(as.character(time_str), "\\.", ":")
  parts <- str_split(clean_time, ":", simplify = TRUE)
  as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
}

expand_weekly_pool_schedule <- function(pool_name, schedule, target_dates = seq(Sys.Date(), Sys.Date() + 7, by = "1 day")) {
  if (length(target_dates) == 0) {
    return(tibble(
      bad = character(),
      dag = character(),
      date = character(),
      activity = character(),
      extra = character(),
      start = numeric(),
      end = numeric()
    ))
  }

  map_dfr(target_dates, function(slot_date) {
    day_name <- get_dutch_day_name(slot_date)
    day_schedule <- schedule %>%
      filter(.data$dag == day_name)

    if (nrow(day_schedule) == 0) return(NULL)

    day_schedule %>%
      mutate(
        bad = pool_name,
        date = as.character(slot_date),
        start = parse_pool_time(.data$start_time),
        end = parse_pool_time(.data$end_time),
        .before = "dag"
      )
  }) %>%
    select(all_of(c("bad", "dag", "date", "activity", "extra", "start", "end")))
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

# Function for Duranbad (Diemen)
# Parses plain HTML schedule from https://www.diemen.nl/zwembad/Openingstijden
# Also checks for schedule changes (roosterwijzigingen) during holidays
get_duranbad_timetable <- function() {
  library(rvest)

  base_url <- "https://www.diemen.nl/zwembad"
  main_url <- paste0(base_url, "/Openingstijden")
  changes_url <- paste0(base_url, "/Roosterwijzigingen")

  message("Fetching Duranbad (Diemen)...")

  # Fetch main page
  response <- GET(main_url, user_agent("Mozilla/5.0"))
  if (status_code(response) != 200) {
    message(paste("Could not fetch Duranbad schedule - Status:", status_code(response)))
    return(NULL)
  }

  page_content <- content(response, "text", encoding = "UTF-8")
  page_html <- read_html(page_content)

  # Check for active schedule changes notice
  # Look for date ranges like "Vanaf X december tot en met Y januari"
  has_active_changes <- FALSE
  changes_start <- NULL
  changes_end <- NULL

  # Extract notice about schedule changes
  notice_text <- page_html %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = " ")

  # Pattern: "Vanaf DD month YYYY tot en met DD month YYYY"
  date_pattern <- "Vanaf\\s+(\\d+)\\s+(\\w+)\\s+(\\d{4})\\s+tot en met\\s+(\\d+)\\s+(\\w+)\\s+(\\d{4})"
  match <- str_match(notice_text, date_pattern)

  if (!is.na(match[1, 1])) {
    dutch_months <- c("januari" = 1, "februari" = 2, "maart" = 3, "april" = 4,
                      "mei" = 5, "juni" = 6, "juli" = 7, "augustus" = 8,
                      "september" = 9, "oktober" = 10, "november" = 11, "december" = 12)

    start_day <- as.numeric(match[1, 2])
    start_month <- dutch_months[tolower(match[1, 3])]
    start_year <- as.numeric(match[1, 4])
    end_day <- as.numeric(match[1, 5])
    end_month <- dutch_months[tolower(match[1, 6])]
    end_year <- as.numeric(match[1, 7])

    if (!is.na(start_month) && !is.na(end_month)) {
      changes_start <- as.Date(paste(start_year, start_month, start_day, sep = "-"))
      changes_end <- as.Date(paste(end_year, end_month, end_day, sep = "-"))

      # Check if today falls within the change period
      today <- Sys.Date()
      if (today >= changes_start && today <= changes_end) {
        has_active_changes <- TRUE
        message(paste("Active schedule changes from", changes_start, "to", changes_end))
      }
    }
  }

  # Helper function to parse schedule text
  parse_duranbad_schedule <- function(html_content, is_changes = FALSE, change_dates = NULL) {
    # Get the schedule section
    schedule_text <- html_content %>%
      html_nodes("p") %>%
      html_text() %>%
      paste(collapse = "\n")

    # Dutch day names
    days <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")

    # Calculate dates for current week
    week_start <- floor_date(Sys.Date(), "week", week_start = 1)
    day_to_offset <- c("Maandag" = 0, "Dinsdag" = 1, "Woensdag" = 2, "Donderdag" = 3,
                       "Vrijdag" = 4, "Zaterdag" = 5, "Zondag" = 6)

    all_slots <- list()
    current_day <- NULL

    # Split by lines and parse
    lines <- str_split(schedule_text, "\n")[[1]]

    for (line in lines) {
      line <- str_trim(line)
      if (nchar(line) == 0) next

      # Check if this is a day header (with optional date for changes page)
      day_match <- str_match(line, paste0("^(", paste(days, collapse = "|"), ")"))

      # Also check for "maandag 22 en 29 december" format
      day_date_match <- str_match(line, paste0("^(", paste(tolower(days), collapse = "|"), ")\\s+(\\d+)"))

      if (!is.na(day_match[1, 1])) {
        current_day <- day_match[1, 2]
        next
      } else if (!is.na(day_date_match[1, 1])) {
        current_day <- str_to_title(day_date_match[1, 2])
        next
      }

      if (is.null(current_day)) next

      # Parse time slots: "HH:MM-HH:MM uur Activity (extra)"
      # Also handle variations like "HH:MM-HH:MM uur" with space issues
      time_pattern <- "(\\d{1,2})[:\\.](\\d{2})\\s*-\\s*(\\d{1,2})[:\\.](\\d{2})\\s*uur\\s*(.+)"
      time_match <- str_match(line, time_pattern)

      if (!is.na(time_match[1, 1])) {
        start_h <- as.numeric(time_match[1, 2])
        start_m <- as.numeric(time_match[1, 3])
        end_h <- as.numeric(time_match[1, 4])
        end_m <- as.numeric(time_match[1, 5])
        activity_text <- str_trim(time_match[1, 6])

        # Clean up activity text
        activity_text <- str_replace_all(activity_text, "\\s+", " ")

        # Extract main activity and extra info
        # Pattern: "Activity (extra info)" or "Activity extra info"
        activity <- activity_text
        extra <- ""

        # Check for parenthetical info
        paren_match <- str_match(activity_text, "^([^(]+)\\(([^)]+)\\)(.*)$")
        if (!is.na(paren_match[1, 1])) {
          activity <- str_trim(paren_match[1, 2])
          extra <- str_trim(paste0(paren_match[1, 3], " ", paren_match[1, 4]))
        }

        # Check for "Let op" notes
        if (str_detect(activity_text, "Let op")) {
          letop_match <- str_match(activity_text, "(.+?)\\s*Let op\\s*(.+)")
          if (!is.na(letop_match[1, 1])) {
            activity <- str_trim(letop_match[1, 2])
            extra <- paste0(extra, " Let op: ", str_trim(letop_match[1, 3]))
          }
        }

        # Calculate date
        day_offset <- day_to_offset[current_day]
        slot_date <- if (!is.na(day_offset)) as.character(week_start + days(day_offset)) else NA

        # Normalize activity names
        activity <- str_replace(activity, "Banen zwemmen", "Banenzwemmen")
        activity <- str_replace(activity, "Recreatie zwemmen", "Recreatief zwemmen")

        all_slots[[length(all_slots) + 1]] <- tibble(
          bad = "Duranbad (Diemen)",
          dag = current_day,
          date = slot_date,
          activity = activity,
          extra = str_trim(extra),
          start = start_h + start_m / 60,
          end = end_h + end_m / 60
        )
      }
    }

    if (length(all_slots) > 0) {
      return(bind_rows(all_slots))
    }
    return(NULL)
  }

  # Parse main schedule
  main_schedule <- parse_duranbad_schedule(page_html)

  # If there are active changes, also fetch and parse the changes page
  if (has_active_changes) {
    message("Fetching schedule changes (roosterwijzigingen)...")
    changes_response <- GET(changes_url, user_agent("Mozilla/5.0"))

    if (status_code(changes_response) == 200) {
      changes_content <- content(changes_response, "text", encoding = "UTF-8")
      changes_html <- read_html(changes_content)
      changes_schedule <- parse_duranbad_schedule(changes_html, is_changes = TRUE)

      if (!is.null(changes_schedule) && nrow(changes_schedule) > 0) {
        message(paste("Found", nrow(changes_schedule), "slots from roosterwijzigingen"))

        # For dates within the change period, use the changes schedule
        # Filter main_schedule to exclude dates within change period
        if (!is.null(main_schedule)) {
          main_schedule <- main_schedule %>%
            filter(is.na(date) | as.Date(date) < changes_start | as.Date(date) > changes_end)
        }

        # Combine: changes schedule takes precedence for affected dates
        main_schedule <- bind_rows(main_schedule, changes_schedule)
      }
    }
  }

  if (!is.null(main_schedule)) {
    message(paste("Found", nrow(main_schedule), "slots for Duranbad"))
  }

  return(main_schedule)
}

# Function for Zwembad De Meerkamp (Amstelveen)
# Parses the Modern Events Calendar timetable rendered on the official page.
get_meerkamp_timetable <- function() {
  library(rvest)

  main_url <- "https://amstelveensport.nl/zwembad-de-meerkamp/"
  ajax_url <- "https://amstelveensport.nl/wp-admin/admin-ajax.php"

  message("Fetching De Meerkamp (Amstelveen)...")

  response <- GET(main_url, user_agent("Mozilla/5.0"))
  if (status_code(response) != 200) {
    message(paste("Could not fetch De Meerkamp schedule - Status:", status_code(response)))
    return(NULL)
  }

  page_content <- content(response, "text", encoding = "UTF-8")
  page_html <- read_html(page_content)
  target_dates <- seq(Sys.Date(), Sys.Date() + 7, by = "1 day")

  parse_time_dec <- function(hours, minutes) {
    as.numeric(hours) + as.numeric(minutes) / 60
  }

  parse_meerkamp_events <- function(html_content) {
    articles <- html_content %>% html_elements("article.mec-timetable-event")
    if (length(articles) == 0) return(NULL)

    day_names <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")

    map_dfr(articles, function(article) {
      class_attr <- html_attr(article, "class") %||% ""
      date_match <- str_match(class_attr, "mec-timetable-day-\\d+-(\\d{8})")
      if (is.na(date_match[1, 2])) return(NULL)

      slot_date <- as.Date(date_match[1, 2], "%Y%m%d")
      time_text <- article %>%
        html_element(".mec-timetable-event-time span") %>%
        html_text2()

      time_match <- str_match(time_text, "(\\d{1,2}):(\\d{2})\\s*[-–]\\s*(\\d{1,2}):(\\d{2})")
      if (is.na(time_match[1, 1])) return(NULL)

      title_text <- article %>%
        html_element(".mec-timetable-event-title") %>%
        html_text2() %>%
        str_replace_all("\\u00a0", " ") %>%
        str_replace_all("\\r", "\n")

      title_lines <- str_split(title_text, "\n+")[[1]] %>%
        str_squish()
      title_lines <- title_lines[nzchar(title_lines)]
      if (length(title_lines) == 0) return(NULL)

      start <- parse_time_dec(time_match[1, 2], time_match[1, 3])
      end <- parse_time_dec(time_match[1, 4], time_match[1, 5])
      if (end < start) end <- end + 24

      tibble(
        bad = "De Meerkamp (Amstelveen)",
        dag = day_names[wday(slot_date, week_start = 1)],
        date = as.character(slot_date),
        activity = title_lines[1],
        extra = if (length(title_lines) > 1) paste(title_lines[-1], collapse = " ") else "",
        start = start,
        end = end
      )
    })
  }

  all_sessions <- parse_meerkamp_events(page_html)

  # The initial page contains the visible month. If the next 7 days cross into
  # another month, load that month through the same MEC endpoint used by the site.
  atts <- str_match(page_content, 'atts:\\s*"([^"]+)"')[1, 2]
  if (!is.na(atts) && !is.null(all_sessions)) {
    loaded_months <- unique(format(as.Date(all_sessions$date), "%Y-%m"))
    target_months <- unique(format(target_dates, "%Y-%m"))
    missing_months <- setdiff(target_months, loaded_months)

    for (month_id in missing_months) {
      year <- str_sub(month_id, 1, 4)
      month <- str_sub(month_id, 6, 7)
      body <- paste0(
        "action=mec_timetable_load_month",
        "&mec_year=", year,
        "&mec_month=", month,
        "&mec_week=1&",
        atts,
        "&apply_sf_date=0"
      )

      ajax_response <- tryCatch({
        POST(
          ajax_url,
          body = body,
          encode = "raw",
          content_type("application/x-www-form-urlencoded; charset=UTF-8"),
          user_agent("Mozilla/5.0"),
          add_headers(
            "Accept" = "application/json, text/javascript, */*; q=0.01",
            "Referer" = main_url,
            "X-Requested-With" = "XMLHttpRequest"
          )
        )
      }, error = function(e) {
        message(paste("Error fetching De Meerkamp month", month_id, ":", e$message))
        return(NULL)
      })

      if (!is.null(ajax_response) && status_code(ajax_response) == 200) {
        ajax_content <- content(ajax_response, "text", encoding = "UTF-8")
        ajax_data <- tryCatch(jsonlite::fromJSON(ajax_content), error = function(e) NULL)
        if (!is.null(ajax_data$month)) {
          month_html <- read_html(paste0("<div>", ajax_data$month, "</div>"))
          all_sessions <- bind_rows(all_sessions, parse_meerkamp_events(month_html))
        }
      }
    }
  }

  if (!is.null(all_sessions) && nrow(all_sessions) > 0) {
    all_sessions <- all_sessions %>%
      distinct(date, activity, start, end, .keep_all = TRUE) %>%
      filter(as.Date(date) %in% target_dates) %>%
      arrange(date, start)

    message(paste("Found", nrow(all_sessions), "slots for De Meerkamp"))
  }

  return(all_sessions)
}

# Function for De Sporthoeve (Badhoevedorp)
# Based on the official 2026 openingstijden PDF linked from Sportfondsen.
get_sporthoeve_timetable <- function() {
  message("Fetching De Sporthoeve (Badhoevedorp)...")

  target_dates <- seq(Sys.Date(), Sys.Date() + 7, by = "1 day")
  # Sporthoeve explicitly lists Eerste and Tweede Pinksterdag 2026 as closed.
  closed_dates <- as.Date(c("2026-05-24", "2026-05-25"))
  target_dates <- target_dates[!target_dates %in% closed_dates]

  schedule <- tribble(
    ~dag, ~activity, ~extra, ~start_time, ~end_time,
    "Maandag", "Banenzwemmen", "", "07:00", "08:50",
    "Maandag", "Banenzwemmen", "", "11:30", "13:25",
    "Maandag", "Aqua Power diep/ondiep", "", "09:00", "09:45",
    "Maandag", "Aqua Power diep/ondiep", "", "10:00", "10:45",
    "Maandag", "Aquavitaal", "", "11:00", "11:30",
    "Dinsdag", "Banenzwemmen", "", "12:00", "13:25",
    "Dinsdag", "Banenzwemmen", "", "20:45", "21:50",
    "Dinsdag", "Aqua Power diep/ondiep", "", "20:00", "20:45",
    "Woensdag", "Banenzwemmen", "", "07:00", "08:50",
    "Woensdag", "Banenzwemmen", "", "11:30", "13:25",
    "Woensdag", "Baby- peuterzwemmen", "", "09:00", "10:30",
    "Woensdag", "Aquavitaal", "Alleen op woensdag volgens rooster", "09:00", "09:30",
    "Donderdag", "Banenzwemmen", "", "12:00", "13:25",
    "Donderdag", "Banenzwemmen", "", "20:45", "21:50",
    "Donderdag", "Aqua Power diep/ondiep", "", "20:00", "20:45",
    "Vrijdag", "Banenzwemmen", "", "11:30", "13:25",
    "Vrijdag", "Baby- peuterzwemmen", "", "09:00", "10:30",
    "Vrijdag", "Aqua Power diep/ondiep", "", "09:00", "09:45",
    "Vrijdag", "Aqua Dance (ondiep)", "", "10:00", "10:45",
    "Vrijdag", "Aquavitaal", "", "11:00", "11:30",
    "Zaterdag", "Banenzwemmen", "", "08:00", "10:50",
    "Zondag", "Banenzwemmen", "", "09:00", "10:25",
    "Zondag", "Familiezwemmen", "", "10:30", "13:50",
    "Zondag", "Baby-Peuter vrijzwemmen", "", "09:15", "10:30",
    "Zondag", "Vrijzwemmen", "", "11:30", "13:50",
    "Zondag", "Jip's oefenuur", "", "10:30", "11:30"
  )

  sessions <- expand_weekly_pool_schedule("De Sporthoeve (Badhoevedorp)", schedule, target_dates)
  message(paste("Found", nrow(sessions), "slots for De Sporthoeve"))
  sessions
}

# Function for Zwembad De Waterlelie (Aalsmeer)
# Based on the official "Openingstijden per 2 september" PDF.
get_waterlelie_timetable <- function() {
  message("Fetching De Waterlelie (Aalsmeer)...")

  schedule <- tribble(
    ~dag, ~activity, ~extra, ~start_time, ~end_time,
    "Maandag", "Banenzwemmen", "Banenbad", "07:00", "14:00",
    "Maandag", "Banenzwemmen", "Recreatiebad", "11:00", "13:00",
    "Maandag", "Banenzwemmen", "Banenbad", "20:00", "21:30",
    "Maandag", "AquaJoggen", "Recreatiebad", "19:45", "20:30",
    "Maandag", "AquaVitaal", "Recreatiebad", "09:00", "11:00",
    "Maandag", "Zwemles", "Alle baden", "15:00", "18:00",
    "Maandag", "Baby & Peuterzwemmen", "Instructiebad", "09:00", "09:30",
    "Maandag", "Baby & Peuterzwemmen", "Instructiebad", "09:30", "10:15",
    "Maandag", "Baby & Peuterzwemmen", "Instructiebad", "10:15", "11:00",
    "Dinsdag", "Banenzwemmen", "Banenbad", "07:00", "14:00",
    "Dinsdag", "Banenzwemmen", "Recreatiebad", "11:00", "13:00",
    "Dinsdag", "Banenzwemmen", "Banenbad", "20:00", "21:30",
    "Dinsdag", "Borstcrawlles", "Recreatiebad", "19:00", "19:45",
    "Dinsdag", "Borstcrawlles", "Recreatiebad", "19:45", "20:30",
    "Dinsdag", "Borstcrawlles", "Recreatiebad", "20:30", "21:15",
    "Dinsdag", "Borstcrawlles", "Recreatiebad", "21:15", "22:00",
    "Dinsdag", "AquaVitaal", "Recreatiebad", "09:00", "11:00",
    "Dinsdag", "Zwemles", "Alle baden", "15:00", "18:00",
    "Dinsdag", "Baby & Peuterzwemmen", "Instructiebad", "09:00", "09:30",
    "Dinsdag", "Baby & Peuterzwemmen", "Instructiebad", "09:30", "10:15",
    "Dinsdag", "Baby & Peuterzwemmen", "Instructiebad", "10:15", "11:00",
    "Woensdag", "Banenzwemmen", "Banenbad", "07:00", "14:00",
    "Woensdag", "Banenzwemmen", "Banenbad", "20:00", "21:30",
    "Woensdag", "AquaEnergy", "Recreatiebad", "20:15", "21:00",
    "Woensdag", "Zwemles", "Alle baden", "15:00", "18:00",
    "Donderdag", "Banenzwemmen", "Banenbad", "07:00", "14:00",
    "Donderdag", "Banenzwemmen", "Recreatiebad", "11:00", "13:00",
    "Donderdag", "Banenzwemmen", "Banenbad", "20:00", "21:30",
    "Donderdag", "Borstcrawlles", "Recreatiebad", "20:45", "21:30",
    "Donderdag", "AquaJoggen", "Recreatiebad", "14:00", "14:45",
    "Donderdag", "AquaEnergy", "Recreatiebad", "19:45", "20:30",
    "Donderdag", "AquaVitaal", "Recreatiebad", "09:00", "11:00",
    "Donderdag", "Zwemles", "Alle baden", "15:00", "18:00",
    "Donderdag", "Baby & Peuterzwemmen", "Instructiebad", "09:00", "09:30",
    "Donderdag", "Baby & Peuterzwemmen", "Instructiebad", "09:30", "10:15",
    "Donderdag", "Baby & Peuterzwemmen", "Instructiebad", "10:15", "11:00",
    "Vrijdag", "Banenzwemmen", "Banenbad", "07:00", "14:00",
    "Vrijdag", "Banenzwemmen", "Recreatiebad", "11:00", "13:00",
    "Vrijdag", "AquaEnergy", "Recreatiebad", "09:00", "09:45",
    "Vrijdag", "Zwemles", "Alle baden", "14:00", "18:00",
    "Vrijdag", "Kleuterzwemmen", "Recreatiebad & instructiebad", "13:15", "14:00",
    "Vrijdag", "Baby & Peuterzwemmen", "Instructiebad", "09:00", "09:30",
    "Vrijdag", "Baby & Peuterzwemmen", "Instructiebad", "09:30", "10:15",
    "Vrijdag", "Baby & Peuterzwemmen", "Instructiebad", "10:15", "11:00",
    "Zaterdag", "Banenzwemmen", "Banenbad", "10:00", "12:30",
    "Zaterdag", "Zwemles", "Recreatiebad & instructiebad", "07:45", "12:30",
    "Zaterdag", "Recreatief zwemmen", "Recreatiebad & instructiebad", "12:30", "14:30",
    "Zondag", "Banenzwemmen", "Banenbad", "10:00", "13:00",
    "Zondag", "Oefenuurtje", "Recreatiebad", "09:00", "10:00",
    "Zondag", "Recreatief zwemmen", "Recreatiebad & instructiebad", "10:00", "13:00"
  )

  sessions <- expand_weekly_pool_schedule("De Waterlelie (Aalsmeer)", schedule)
  message(paste("Found", nrow(sessions), "slots for De Waterlelie"))
  sessions
}

# Function for De Slag (Zaandam)
# Parses the embedded schedule JSON from the official Sportbedrijf Zaanstad page.
get_de_slag_timetable <- function() {
  main_url <- "https://www.sportbedrijfzaanstad.nl/zwembaden/de-slag/"
  message("Fetching De Slag (Zaandam)...")

  response <- GET(main_url, user_agent("Mozilla/5.0"))
  if (status_code(response) != 200) {
    message(paste("Could not fetch De Slag schedule - Status:", status_code(response)))
    return(NULL)
  }

  page_content <- content(response, "text", encoding = "UTF-8")
  lessons_json <- str_match(page_content, regex("const\\s+lessen\\s*=\\s*(\\[.*?\\]);", dotall = TRUE))[1, 2]
  if (is.na(lessons_json)) {
    message("Could not find De Slag embedded lesson data")
    return(NULL)
  }

  lessons <- tryCatch(
    jsonlite::fromJSON(lessons_json, simplifyVector = FALSE),
    error = function(e) {
      message(paste("Could not parse De Slag lesson data:", e$message))
      NULL
    }
  )
  if (is.null(lessons) || length(lessons) == 0) return(NULL)

  target_dates <- seq(Sys.Date(), Sys.Date() + 7, by = "1 day")
  closed_dates <- as.Date(c("2026-05-24", "2026-05-25"))
  target_dates <- target_dates[!target_dates %in% closed_dates]

  lesson_value <- function(lesson, field, default = "") {
    value <- lesson[[field]]
    if (is.null(value) || identical(value, FALSE) || length(value) == 0) default else value
  }

  get_week_lessons <- function(week_number) {
    slots_by_day <- list()

    for (lesson in lessons) {
      day <- lesson_value(lesson, "dag")
      if (!nzchar(day)) next
      key <- paste0(lesson_value(lesson, "starttijd"), "-", lesson_value(lesson, "eindtijd"), lesson_value(lesson, "groep"))

      if (is.null(slots_by_day[[day]])) slots_by_day[[day]] <- list()

      frequency <- lesson_value(lesson, "frequentie")
      if (frequency == "eenmalig") {
        exception_week <- suppressWarnings(as.integer(lesson_value(lesson, "uitzondering", NA)))
        if (!is.na(exception_week) && exception_week == week_number) {
          slots_by_day[[day]][[key]] <- lesson
        }
      } else if (frequency == "wekelijks" && is.null(slots_by_day[[day]][[key]])) {
        slots_by_day[[day]][[key]] <- lesson
      }
    }

    slots_by_day
  }

  sessions <- map_dfr(target_dates, function(slot_date) {
    day_name <- get_dutch_day_name(slot_date)
    week_lessons <- get_week_lessons(isoweek(slot_date))
    day_lessons <- week_lessons[[day_name]]
    if (is.null(day_lessons) || length(day_lessons) == 0) return(NULL)

    map_dfr(day_lessons, function(lesson) {
      change <- lesson_value(lesson, "wijziging")
      if (change == "gaat niet door") return(NULL)

      extra <- c(lesson_value(lesson, "locatie"), lesson_value(lesson, "opmerking"))
      extra <- extra[nzchar(extra)]

      tibble(
        bad = "De Slag (Zaandam)",
        dag = day_name,
        date = as.character(slot_date),
        activity = lesson_value(lesson, "groep", "Onbekend"),
        extra = paste(extra, collapse = " - "),
        start = parse_pool_time(lesson_value(lesson, "starttijd")),
        end = parse_pool_time(lesson_value(lesson, "eindtijd"))
      )
    })
  }) %>%
    distinct(date, activity, extra, start, end, .keep_all = TRUE) %>%
    arrange(date, start)

  message(paste("Found", nrow(sessions), "slots for De Slag"))
  sessions
}

# Function for Zwembad De Breek (Landsmeer)
# Seasonal outdoor pool. Based on the official 2026 openingstijden poster.
# IMPORTANT FOR FUTURE AI/CODEX: De Breek publishes the actual schedule as a
# PNG poster, not as structured HTML or API data. Do not pretend these times
# are scraped dynamically. The check below only warns when the source poster on
# the official page appears to have changed by filename or response metadata;
# the timetable rows are intentionally encoded from the official 2026 poster to
# avoid brittle OCR.
get_de_breek_timetable <- function() {
  message("Fetching De Breek (Landsmeer)...")

  source_url <- "https://www.debreek.nl/openingstijden/"
  expected_poster <- "Poster-Entree-A1-2.png"
  expected_poster_length <- "187287"
  expected_poster_last_modified <- "Tue, 17 Mar 2026 09:06:50 GMT"
  target_dates <- seq(Sys.Date(), Sys.Date() + 7, by = "1 day")
  activity <- "Banenzwemmen / recreatief zwemmen"
  extra <- "Openluchtzwembad. Banenzwemmen mogelijk tijdens reguliere openingstijden."

  source_response <- tryCatch(
    GET(source_url, user_agent("Mozilla/5.0")),
    error = function(e) {
      message(paste("IMPORTANT: Could not check De Breek source page:", e$message))
      NULL
    }
  )

  if (!is.null(source_response)) {
    if (status_code(source_response) == 200) {
      source_content <- content(source_response, "text", encoding = "UTF-8")
      poster_urls <- str_extract_all(
        source_content,
        "https://www\\.debreek\\.nl/wp-content/uploads/[0-9]{4}/[0-9]{2}/[^\"'<> ]+\\.(png|jpg|jpeg|pdf)"
      )[[1]]
      matching_poster <- poster_urls[str_detect(poster_urls, fixed(expected_poster))]

      if (length(matching_poster) == 0) {
        message(
          paste(
            "IMPORTANT: De Breek source poster may have changed.",
            "Re-check the official openingstijden page before trusting the encoded timetable:",
            source_url
          )
        )
      } else {
        poster_head <- tryCatch(
          HEAD(matching_poster[[1]], user_agent("Mozilla/5.0")),
          error = function(e) {
            message(paste("IMPORTANT: Could not check De Breek poster metadata:", e$message))
            NULL
          }
        )

        if (!is.null(poster_head) && status_code(poster_head) == 200) {
          poster_headers <- headers(poster_head)
          poster_length <- poster_headers[["content-length"]]
          poster_last_modified <- poster_headers[["last-modified"]]

          if (
            is.null(poster_length) ||
              is.null(poster_last_modified) ||
              poster_length != expected_poster_length ||
              poster_last_modified != expected_poster_last_modified
          ) {
            message(
              paste(
                "IMPORTANT: De Breek source poster metadata changed.",
                "Re-check the official openingstijden page before trusting the encoded timetable:",
                source_url
              )
            )
          }
        } else if (!is.null(poster_head)) {
          message(paste("IMPORTANT: Could not check De Breek poster metadata - Status:", status_code(poster_head)))
        }
      }
    } else {
      message(paste("IMPORTANT: Could not check De Breek source page - Status:", status_code(source_response)))
    }
  }

  make_schedule <- function(...) {
    tribble(
      ~dag, ~activity, ~extra, ~start_time, ~end_time,
      ...
    )
  }

  roster_1 <- make_schedule(
    "Dinsdag", activity, extra, "07:00", "11:00",
    "Dinsdag", activity, extra, "14:00", "19:00",
    "Woensdag", activity, extra, "09:00", "11:00",
    "Woensdag", activity, extra, "14:00", "19:00",
    "Donderdag", activity, extra, "07:00", "11:00",
    "Donderdag", activity, extra, "14:00", "19:00",
    "Vrijdag", activity, extra, "09:00", "11:00",
    "Vrijdag", activity, extra, "14:00", "19:00",
    "Zaterdag", activity, extra, "11:00", "18:00",
    "Zondag", activity, extra, "11:00", "18:00"
  )

  roster_2 <- make_schedule(
    "Dinsdag", activity, extra, "07:00", "11:00",
    "Dinsdag", activity, extra, "14:00", "17:00",
    "Dinsdag", activity, extra, "19:00", "20:00",
    "Woensdag", activity, extra, "09:00", "11:00",
    "Woensdag", activity, extra, "14:00", "20:00",
    "Donderdag", activity, extra, "07:00", "11:00",
    "Donderdag", activity, extra, "14:00", "20:00",
    "Vrijdag", activity, extra, "09:00", "11:00",
    "Vrijdag", activity, extra, "14:00", "20:00",
    "Zaterdag", activity, extra, "11:00", "18:00",
    "Zondag", activity, extra, "11:00", "18:00"
  )

  roster_3 <- make_schedule(
    "Maandag", activity, extra, "10:00", "17:00",
    "Dinsdag", activity, extra, "07:00", "17:00",
    "Dinsdag", activity, extra, "19:00", "20:00",
    "Woensdag", activity, extra, "09:00", "20:00",
    "Donderdag", activity, extra, "07:00", "20:00",
    "Vrijdag", activity, extra, "09:00", "20:00",
    "Zaterdag", activity, extra, "11:00", "18:00",
    "Zondag", activity, extra, "11:00", "18:00"
  )

  roster_4 <- roster_2

  seasons <- list(
    list(start = as.Date("2026-05-02"), end = as.Date("2026-06-01"), schedule = roster_1),
    list(start = as.Date("2026-06-02"), end = as.Date("2026-07-05"), schedule = roster_2),
    list(start = as.Date("2026-07-06"), end = as.Date("2026-08-16"), schedule = roster_3),
    list(start = as.Date("2026-08-17"), end = as.Date("2026-08-30"), schedule = roster_4)
  )

  sessions <- map_dfr(seasons, function(season) {
    season_dates <- target_dates[target_dates >= season$start & target_dates <= season$end]
    expand_weekly_pool_schedule("De Breek (Landsmeer)", season$schedule, season_dates)
  }) %>%
    distinct(date, activity, extra, start, end, .keep_all = TRUE) %>%
    arrange(date, start)

  message(paste("Found", nrow(sessions), "slots for De Breek"))
  sessions
}

# Function for Het Amstelbad (Ouderkerk aan de Amstel)
# Seasonal outdoor pool. The 2026 season page lists daily opening hours.
get_amstelbad_timetable <- function() {
  message("Fetching Amstelbad (Ouderkerk aan de Amstel)...")

  target_dates <- seq(Sys.Date(), Sys.Date() + 7, by = "1 day")
  season_start <- as.Date("2026-04-25")
  season_end <- as.Date("2026-09-20")
  target_dates <- target_dates[target_dates >= season_start & target_dates <= season_end]

  if (length(target_dates) == 0) {
    message("Amstelbad is outside its 2026 outdoor season")
    return(NULL)
  }

  regular_schedule <- tribble(
    ~dag, ~activity, ~extra, ~start_time, ~end_time,
    "Maandag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "08:00", "20:00",
    "Dinsdag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "08:00", "20:00",
    "Woensdag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "08:00", "19:00",
    "Donderdag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "08:00", "20:00",
    "Vrijdag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "08:00", "19:00",
    "Zaterdag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "10:00", "18:00",
    "Zondag", "Recreatief zwemmen", "Openluchtzwembad. Tenminste een baan open voor banenzwemmen.", "10:00", "18:00"
  )

  extended_season_schedule <- tibble(
    dag = zwemsterdam_day_names,
    activity = "Recreatief zwemmen",
    extra = "Verlengde seizoensweek. Openluchtzwembad.",
    start_time = "10:00",
    end_time = "18:00"
  )

  sessions <- bind_rows(
    expand_weekly_pool_schedule(
      "Amstelbad (Ouderkerk aan de Amstel)",
      regular_schedule,
      target_dates[target_dates < as.Date("2026-09-14")]
    ),
    expand_weekly_pool_schedule(
      "Amstelbad (Ouderkerk aan de Amstel)",
      extended_season_schedule,
      target_dates[target_dates >= as.Date("2026-09-14")]
    )
  )

  message(paste("Found", nrow(sessions), "slots for Amstelbad"))
  sessions
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
