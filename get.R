
library(tidyverse)
library(jsonlite)
source("utils.R")

# Create directories if they don't exist
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("frontend/public")) dir.create("frontend/public", recursive = TRUE)

# Pool website URLs for linking
pool_websites <- list(
  "Zuiderbad" = "https://www.amsterdam.nl/zuiderbad/zwembadrooster-zuiderbad/",
  "Noorderparkbad" = "https://www.amsterdam.nl/noorderparkbad/zwembadrooster-noorderparkbad/",
  "De Mirandabad" = "https://www.amsterdam.nl/de-mirandabad/zwembadrooster-de-mirandabad/",
  "Flevoparkbad" = "https://www.amsterdam.nl/flevoparkbad/zwembadrooster-flevoparkbad/",
  "Brediusbad" = "https://www.amsterdam.nl/brediusbad/zwembadrooster-brediusbad/",
  "Het Marnix" = "https://hetmarnix.nl/zwemmen/",
  "Sportfondsenbad Oost" = "https://amsterdamoost.sportfondsen.nl/tijden-tarieven/",
  "Sportplaza Mercator" = "https://mercator.sportfondsen.nl/tijden-tarieven/",
  "Bijlmer Sportcentrum" = "https://www.optisport.nl/zwembad-bijlmer-amsterdam-zuidoost",
  "Sloterparkbad" = "https://www.optisport.nl/sloterparkbad-amsterdam"
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

# 4. Optisport Pools (Bijlmer, Sloterparkbad)
# These require Playwright to bypass Cloudflare - data fetched separately
print("Loading Optisport pools (requires 'node explore_optisport.js' to be run first)...")
optisport_data <- get_optisport_data()

# Combine all data
all_swimming_data <- bind_rows(muni_data, marnix_data, sportfondsen_data, optisport_data) %>%
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
    )
  )
)

write_json(metadata, "frontend/public/metadata.json", pretty = TRUE, auto_unbox = TRUE)

print(paste("Data collection complete. Found", nrow(all_swimming_data), "sessions."))
print("Exported to data/fin.rds, frontend/public/data.json, and frontend/public/metadata.json")
