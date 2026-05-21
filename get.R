
library(tidyverse)
library(jsonlite)
source("utils.R")

# Create directories if they don't exist
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("frontend/public")) dir.create("frontend/public", recursive = TRUE)

previous_data <- if (file.exists("frontend/public/data.json")) {
  tryCatch(
    jsonlite::fromJSON("frontend/public/data.json"),
    error = function(e) {
      message(paste("Could not load previous data cache:", e$message))
      NULL
    }
  )
} else {
  NULL
}

empty_swimming_data <- function() {
  tibble(
    bad = character(),
    dag = character(),
    date = character(),
    activity = character(),
    extra = character(),
    start = numeric(),
    end = numeric()
  )
}

append_cached_pool_rows <- function(current_data, required_pools, source_name) {
  found_pools <- if (!is.null(current_data) && nrow(current_data) > 0) {
    unique(current_data$bad)
  } else {
    character()
  }

  missing_pools <- setdiff(required_pools, found_pools)

  if (length(missing_pools) > 0) {
    target_dates <- as.character(seq(Sys.Date(), Sys.Date() + 7, by = "1 day"))
    cached_pool_data <- if (!is.null(previous_data) && nrow(previous_data) > 0) {
      previous_data %>%
        filter(
          bad %in% missing_pools,
          date %in% target_dates
        ) %>%
        select(any_of(c("bad", "dag", "date", "activity", "extra", "start", "end")))
    } else {
      empty_swimming_data()
    }

    if (nrow(cached_pool_data) > 0) {
      message(
        paste(
          source_name,
          "source unavailable or incomplete for:",
          paste(missing_pools, collapse = ", "),
          "- using rows from the last committed data cache."
        )
      )

      cache_note <- paste(
        "Laatste succesvolle",
        source_name,
        "update; officiële site tijdelijk niet bereikbaar"
      )

      cached_pool_data <- cached_pool_data %>%
        mutate(
          extra = case_when(
            is.na(extra) | extra == "" ~ cache_note,
            str_detect(extra, fixed(cache_note)) ~ extra,
            TRUE ~ paste(extra, cache_note, sep = " - ")
          )
        )

      current_data <- bind_rows(current_data, cached_pool_data)
    } else {
      message(
        paste(
          source_name,
          "source unavailable or incomplete for:",
          paste(missing_pools, collapse = ", "),
          "- no cached rows available for the current date window."
        )
      )
    }
  }

  if (!is.null(current_data) && nrow(current_data) > 0) {
    current_data %>%
      distinct(bad, date, activity, extra, start, end, .keep_all = TRUE)
  } else {
    empty_swimming_data()
  }
}

# Pool website URLs for linking
pool_websites <- list(
  "Zuiderbad" = "https://www.amsterdam.nl/zuiderbad/zwembadrooster-zuiderbad/",
  "Noorderparkbad" = "https://www.amsterdam.nl/noorderparkbad/zwembadrooster-noorderparkbad/",
  "De Mirandabad" = "https://www.amsterdam.nl/demirandabad/rooster/",
  "Flevoparkbad" = "https://www.amsterdam.nl/flevoparkbad/zwembadrooster-flevoparkbad/",
  "Brediusbad" = "https://www.amsterdam.nl/brediusbad/zwembadrooster-brediusbad/",
  "Het Marnix" = "https://hetmarnix.nl/zwemmen/",
  "Sportfondsenbad Oost" = "https://amsterdamoost.sportfondsen.nl/tijden-tarieven/",
  "Sportplaza Mercator" = "https://mercator.sportfondsen.nl/tijden-tarieven-van-mercator/",
  "Bijlmer Sportcentrum" = "https://www.optisport.nl/zwembad-bijlmer-amsterdam-zuidoost",
  "Sloterparkbad" = "https://www.optisport.nl/zwembad-het-sloterparkbad-amsterdam",
  "Duranbad (Diemen)" = "https://www.diemen.nl/zwembad/Openingstijden",
  "De Meerkamp (Amstelveen)" = "https://amstelveensport.nl/zwembad-de-meerkamp/",
  "De Sporthoeve (Badhoevedorp)" = "https://sporthoeve.sportfondsen.nl/tijden-en-tarieven/",
  "De Waterlelie (Aalsmeer)" = "https://sportinaalsmeer.nl/zwembad-de-waterlelie",
  "De Slag (Zaandam)" = "https://www.sportbedrijfzaanstad.nl/zwembaden/de-slag/",
  "Amstelbad (Ouderkerk aan de Amstel)" = "https://www.amstelbad.nl/praktische-info/openingstijden/"
)

# 1. Municipal Pools (Amsterdam.nl API)
# Only these 5 pools are available in the Amsterdam zwembaden API
municipal_pools <- c("zuiderbad", "noorderparkbad", "de-mirandabad", "flevoparkbad", "brediusbad")

print("Fetching municipal pools...")
muni_data <- municipal_pools %>%
  map_dfr(get_amsterdam_timetables)

# 2. Het Marnix
print("Fetching Het Marnix...")
marnix_data <- get_marnix_timetable()

# 3. Sportfondsen Pools
# NOTE: The Sportfondsen websites have migrated to new URL structure!
# Old: sportfondsenbadamsterdamoost.nl -> New: amsterdamoost.sportfondsen.nl
# Old: sportplazamercator.nl -> New: mercator.sportfondsen.nl
# Each pool has different URL paths for their schedule pages
print("Fetching Sportfondsen pools...")
sportfondsen_pools <- list(
  list(url = "https://amsterdamoost.sportfondsen.nl", name = "Sportfondsenbad Oost", path = "/tijden-tarieven/"),
  list(url = "https://mercator.sportfondsen.nl", name = "Sportplaza Mercator", path = "/tijden-tarieven-van-mercator/")
)

sportfondsen_data <- sportfondsen_pools %>%
  map_dfr(~get_sportfondsen_timetable(.x$url, .x$name, .x$path))

required_sportfondsen_pools <- c("Sportfondsenbad Oost", "Sportplaza Mercator")
sportfondsen_data <- append_cached_pool_rows(
  sportfondsen_data,
  required_sportfondsen_pools,
  "Sportfondsen"
)

# 4. Optisport Pools (Bijlmer, Sloterparkbad)
# These require Playwright to bypass Cloudflare - data fetched separately
print("Loading Optisport pools (requires 'node explore_optisport.js' to be run first)...")
optisport_data <- get_optisport_data()

# 5. Duranbad (Diemen)
# Plain HTML parsing from diemen.nl - also checks for roosterwijzigingen
print("Fetching Duranbad (Diemen)...")
duranbad_data <- get_duranbad_timetable()

# 6. De Meerkamp (Amstelveen)
# Modern Events Calendar timetable from amstelveensport.nl
print("Fetching De Meerkamp (Amstelveen)...")
meerkamp_data <- get_meerkamp_timetable()
meerkamp_data <- append_cached_pool_rows(
  meerkamp_data,
  "De Meerkamp (Amstelveen)",
  "De Meerkamp"
)

# 7. Nearby pools around Amsterdam
print("Fetching nearby regional pools...")
sporthoeve_data <- get_sporthoeve_timetable()
waterlelie_data <- get_waterlelie_timetable()
de_slag_data <- get_de_slag_timetable()
amstelbad_data <- get_amstelbad_timetable()

# Combine all data
all_swimming_data <- bind_rows(
  muni_data,
  marnix_data,
  sportfondsen_data,
  optisport_data,
  duranbad_data,
  meerkamp_data,
  sporthoeve_data,
  waterlelie_data,
  de_slag_data,
  amstelbad_data
) %>%
  mutate(
    # Ensure consistency in day names
    dag = str_to_title(dag),
    # Add a unique ID for the frontend
    id = row_number(),
    # Add pool website URL
    website = unlist(pool_websites[bad])
  ) %>%
  filter(!is.na(start), !is.na(end))

# Export as RDS for legacy and JSON for the new frontend
saveRDS(all_swimming_data, "data/fin.rds")
write_json(all_swimming_data, "frontend/public/data.json", pretty = TRUE)

# Create metadata with last update time and data sources
metadata <- list(
  lastUpdated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  lastUpdatedLocal = format(Sys.time(), "%d-%m-%Y %H:%M"),
  totalSessions = nrow(all_swimming_data),
  pools = unique(all_swimming_data$bad),
  dataSources = list(
    list(
      name = "Gemeente Amsterdam",
      description = "Gemeentelijke zwembaden van Amsterdam",
      url = "https://www.amsterdam.nl/sport/zwembaden/",
      pools = c("Zuiderbad", "Noorderparkbad", "De Mirandabad", "Flevoparkbad", "Brediusbad")
    ),
    list(
      name = "Het Marnix",
      description = "Zwembad in Amsterdam West",
      url = "https://hetmarnix.nl/",
      pools = c("Het Marnix")
    ),
    list(
      name = "Sportfondsen Amsterdam",
      description = "Sportfondsen zwembaden",
      url = "https://www.sportfondsen.nl/",
      pools = c("Sportfondsenbad Oost", "Sportplaza Mercator")
    ),
    list(
      name = "Optisport",
      description = "Optisport zwembaden",
      url = "https://www.optisport.nl/",
      pools = c("Bijlmer Sportcentrum", "Sloterparkbad")
    ),
    list(
      name = "Duran Sportcentrum",
      description = "Zwembad in Diemen (bij Amsterdam)",
      url = "https://www.diemen.nl/zwembad/",
      pools = c("Duranbad (Diemen)")
    ),
    list(
      name = "AmstelveenSport",
      description = "Zwembad in Amstelveen (bij Amsterdam)",
      url = "https://amstelveensport.nl/zwembad-de-meerkamp/",
      pools = c("De Meerkamp (Amstelveen)")
    ),
    list(
      name = "Sportfondsen De Sporthoeve",
      description = "Zwembad in Badhoevedorp (bij Amsterdam)",
      url = "https://sporthoeve.sportfondsen.nl/tijden-en-tarieven/",
      pools = c("De Sporthoeve (Badhoevedorp)")
    ),
    list(
      name = "Sport in Aalsmeer",
      description = "Zwembad in Aalsmeer (bij Amsterdam)",
      url = "https://sportinaalsmeer.nl/zwembad-de-waterlelie",
      pools = c("De Waterlelie (Aalsmeer)")
    ),
    list(
      name = "Sportbedrijf Zaanstad",
      description = "Zwembad in Zaandam (bij Amsterdam)",
      url = "https://www.sportbedrijfzaanstad.nl/zwembaden/de-slag/",
      pools = c("De Slag (Zaandam)")
    ),
    list(
      name = "Het Amstelbad",
      description = "Seizoensgebonden openluchtzwembad in Ouderkerk aan de Amstel",
      url = "https://www.amstelbad.nl/praktische-info/openingstijden/",
      pools = c("Amstelbad (Ouderkerk aan de Amstel)")
    )
  )
)

write_json(metadata, "frontend/public/metadata.json", pretty = TRUE, auto_unbox = TRUE)

print(paste("Data collection complete. Found", nrow(all_swimming_data), "sessions."))
print("Exported to data/fin.rds, frontend/public/data.json, and frontend/public/metadata.json")
