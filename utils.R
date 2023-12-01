
library(lubridate)
library(dplyr)

aggregate_timeframes <- function(df) {
  # Sort by start time
  df_sorted <- df %>% arrange(start)
  
  # Initialize an empty data frame for results
  new_rows <- df_sorted[0,]
  
  # Iterate through each row
  for (i in 1:nrow(df_sorted)) {
    current_row <- df_sorted[i, ]
    
    # Check for overlaps with the last row in new_rows
    if (nrow(new_rows) == 0 || current_row$start >= tail(new_rows, 1)$end) {
      new_rows <- rbind(new_rows, current_row)
    } else {
      # Adjust the end time of the last row and add a new row
      last_row <- tail(new_rows, 1)
      last_row$end <- min(last_row$end, current_row$start)
      
      # Update the last row in new_rows
      new_rows[nrow(new_rows), ] <- last_row
      
      # Add the current row with adjusted start time
      if (current_row$end > last_row$end) {
        current_row$start <- last_row$end
        new_rows <- rbind(new_rows, current_row)
      }
    }
  }
  
  return(new_rows)
}

get_timetables <- function(bad) {
  
  
  # URL and endpoint
  url <- paste0("https://zwembaden.api-amsterdam.nl/nl/api/", bad, "/activity/")
  
  # Headers
  headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:120.0) Gecko/20100101 Firefox/120.0",
    `Accept` = "application/json, text/plain, */*",
    `Accept-Language` = "en-US,en;q=0.5",
    # `Accept-Encoding` = "gzip, deflate, br",
    `Origin` = "https://www.amsterdam.nl",
    `Connection` = "keep-alive",
    `Referer` = "https://www.amsterdam.nl/",
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "cors",
    `Sec-Fetch-Site` = "cross-site",
    `TE` = "trailers"
  )
  
  # Make the GET request
  response <- GET(url, add_headers(.headers=headers))
  
  # Check the status code
  status_code(response)
  
  # View the response content
  yo <- content(response, "parsed")
  
  
  # yo$days[[1]]$ -> as
  # as  %>% map_dfr(as_tibble)
  # bind_rows()
  
  zwemdat <- yo$days %>% 
    map_dfr(~{
      .x$schedule  %>% map_dfr(as_tibble) %>% 
        mutate(dag = .x$name)
    }) %>% 
    mutate(start = parse_number(start),
           end = parse_number(end)) %>% 
    mutate(bad = str_to_title(str_replace(bad, "-", " ")))
  
  return(zwemdat)
}



get_thistype <- function(url, baad) {
  # print(url)
  # baad <- names(url)
  # print(baad)
  # Setting headers
  headers <- c(
    "authority" = "www.sportfondsenbadamsterdamoost.nl",
    "accept" = "*/*",
    "accept-language" = "en-US,en;q=0.9,de-DE;q=0.8,de;q=0.7,nl;q=0.6",
    "cookie" = "next-auth.csrf-token=143cf089b12624ceaba5b31e71958c1e5cd09cb1ccff4279ecfadeb816c8cab3%7C55437bce0b0910ee94a7d04db05f827988af8747d826fb488ee99083c3bd4712; next-auth.callback-url=http%3A%2F%2Flocalhost%3A3000; _ga_0T3204D8PT=GS1.1.1701421564.1.0.1701421564.60.0.0; _ga=GA1.2.1694794977.1701421564; _gid=GA1.2.454577609.1701421565; sfnAcceptEssential=true; sfnAcceptAnalytics=true; sfnAcceptThirdParty=true; sqzl_consent=analytics,marketing; sqzllocal=sqzl6569a21a0000043f8712; sqzl_session_id=6569a1fc0000043f8711|1701421594.415; _gat_UA-40360714-15=1",
    "referer" = "https://www.sportfondsenbadamsterdamoost.nl/",
    "sec-ch-ua" = "\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"",
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = "\"Windows\"",
    "sec-fetch-dest" = "empty",
    "sec-fetch-mode" = "cors",
    "sec-fetch-site" = "same-origin",
    "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
  )
  
  # Making the GET request
  response <- GET(url, add_headers(.headers=headers))
  
  # Checking the response
  # status_code(response)
  ress <- content(response, "parsed")
  
  
  yo <- ress[["pageProps"]][["extraPageProps"]][["scheduleData"]][["timeSlots"]]
  kidinschool <- yo %>% 
    map_dfr(~{
      # print(.x)
      bind_cols(.x %>% 
                  .[c("day", "startTime", "endTime", "occupationDisplay")] %>% 
                  purrr::discard(is_empty) %>% 
                  as_tibble(),
                
                .x$activitySchedule$activity %>% flatten()  %>% 
                  .[c("title", "url", "id", "name")] %>% 
                  purrr::discard(is_empty) %>% 
                  as_tibble()
      )
    }) %>% 
    rename(dag = day, start = startTime, end = endTime, occupation = occupationDisplay, activity = title)  %>% 
    mutate(bad = "oost") %>% 
    filter(str_detect(activity, "Banenzwemmen")) %>% 
    group_split(dag, bad) %>% 
    map_dfr(aggregate_timeframes) %>% 
    ungroup() %>% 
    mutate(start = parse_number(str_remove(start, ":"))/100)%>%
    mutate(end = parse_number(str_remove(end, ":"))/100) %>% 
    mutate(bad = baad)
  
  return(kidinschool)
}


library(httr)

# URL for the request
url <- "https://hetmarnix.nl/wp-admin/admin-ajax.php"



# Define the start date
start_date <- ymd_hms("2023-11-26T23:00:00.000Z")

# Create a sequence of dates, 7 days apart
date_sequence <- seq(from = start_date, by = "7 days", length.out = 10)

# Create a dataset with start and end dates for each week
dates_dataset <- tibble(
  start = format(date_sequence, "%Y-%m-%dT%H:%M:%OSZ"),
  end = format(date_sequence + days(7), "%Y-%m-%dT%H:%M:%OSZ")
)

# Print the dataset
# print(dates_dataset)

# Find the current week
today <- Sys.Date()
current_week <- dates_dataset %>% 
  filter(as.Date(start) <= today & as.Date(end) >= today)

# Print the current week
# print(current_week)

# Parameters for the request
params <- list(
  action = "getlessons",
  start = current_week$start,
  end = current_week$end#,
  # sectionId = "8281996b-b3d0-4178-83a7-5586566b24ac"
)

# Headers
headers <- c(
  "authority" = "hetmarnix.nl",
  "accept" = "*/*",
  "accept-language" = "en-US,en;q=0.9,de-DE;q=0.8,de;q=0.7,nl;q=0.6",
  "cookie" = "PHPSESSID=316f4f595b2f8c3335b012027dd3337d; _gid=GA1.2.315117722.1701423553; euCookieConsent.accepted=1; euCookieConsent.clicked=1; _ga_574TD1Y4X2=GS1.1.1701423553.2.1.1701424055.58.0.0; _ga=GA1.2.1341570072.1700223087; _gat_gtag_UA_112315885_1=1; _gat_UA-198709723-66=1; _ga_FMXVSD1516=GS1.2.1701423569.1.1.1701424055.0.0.0",
  "referer" = "https://hetmarnix.nl/schedule/tijden/",
  "sec-ch-ua" = "\"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"Windows\"",
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-origin",
  "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36",
  "x-requested-with" = "XMLHttpRequest"
)

# Making the GET request
response <- GET(url, query = params, add_headers(.headers = headers))

# Checking the response
# status_code(response)
marnixres <- content(response, "parsed")


gviolo <- marnixres %>% 
  map_dfr(~{
    .x %>% flatten() %>% 
      as_tibble() %>% 
      mutate(dag = lubridate::wday(start, label = T, abbr = F))
  }) 

# Function to translate English weekday names to Dutch
translate_weekday_to_dutch <- function(weekday_en) {
  # Mapping of English to Dutch weekday names
  weekdays_map <- c("Monday" = "Maandag", 
                    "Tuesday" = "Dinsdag", 
                    "Wednesday" = "Woensdag", 
                    "Thursday" = "Donderdag", 
                    "Friday" = "Vrijdag", 
                    "Saturday" = "Zaterdag", 
                    "Sunday" = "Zondag")
  
  # Translate
  return(weekdays_map[weekday_en])
}

# Example usage
# translate_weekday_to_dutch("Monday") # Should return "maandag"

urls <- c("https://www.sportfondsenbadamsterdamoost.nl/_next/data/9gp0AHBSfAZXqjxvXI1rJ/tijden-tarieven.json?slug=tijden-tarieven",
          "https://www.sportplazamercator.nl/_next/data/9gp0AHBSfAZXqjxvXI1rJ/tijden-tarieven-van-mercator.json?slug=tijden-tarieven-van-mercator") %>% 
  set_names("Sportfondsenbad Oost",
            "Sportplaza Mercator")




kidinschool <- urls %>% 
  imap_dfr(~get_thistype(.x, .y))

hetmarnix <- gviolo  %>%# View()
  # mutate(start = lubridate::ymd_hms(start)) %>% 
  # mutate(start = paste0(as.numeric(lubridate::hour(start)), as.numeric(lubridate::minute(start))) %>% as.numeric) %>% 
  rowwise() %>% 
  mutate(start = str_split(start, "T") %>% unlist %>%  .[2]) %>% 
  mutate(end = str_split(end, "T") %>% unlist %>%  .[2]) %>% 
  ungroup() %>% #View()
  mutate(start = as.numeric(str_remove_all(start, ":"))/10000) %>% 
  mutate(end = as.numeric(str_remove_all(end, ":"))/10000) %>%
  rename(activity = title) %>% 
  mutate(dag = translate_weekday_to_dutch(dag)) %>% 
  mutate(bad = "Het Marnix")  %>% #View() 
  distinct(dag, bad, start, end, activity) %>% 
  filter(str_detect(activity, "Banenzwemmen"))
# filter(dag == "Dinsdag") %>% View()
# group_split(dag, bad) %>% 
# map_dfr(aggregate_timeframes) %>% 
# ungroup() %>% View()

