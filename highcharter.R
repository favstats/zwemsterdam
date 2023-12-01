library(lubridate)
library(highcharter)
library(dplyr)
N <- 7

set.seed(1234)

df <- tibble(
  start = Sys.Date() + months(sample(10:20, size = N)),
  end = start + months(sample(1:3, size = N, replace = TRUE)),
  cat = rep(1:5, length.out = N) - 1,
  progress = round(stats::runif(N), 1)
) |> 
  mutate_if(is.Date, datetime_to_timestamp)

hchart(
  df,
  "xrange",
  hcaes(x = start, x2 = end, y = cat)
)


opening <- readr::read_csv("data/opening.csv") %>% 
  tidyr::separate_rows(times, sep = "en") %>% 
  tidyr::separate(times, sep = "–|-", into = c("from", "to")) %>% 
  tidyr::drop_na() %>% 
  mutate_at(vars(from, to), ~stringr::str_remove_all(.x, ":") %>% as.numeric()) %>% 
  select(start = from, end = to, cat = dag, bad) %>%# filter(bad == "Mirandabad") %>% 
  mutate(cat = factor(cat, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") %>% rev)) %>% 
  mutate(cat = as.numeric(cat)) 

readr::read_csv("data/opening.csv") %>% 
  tidyr::separate_rows(times, sep = "en") %>% 
  tidyr::separate(times, sep = "–|-", into = c("from", "to")) %>% 
  tidyr::drop_na() %>% 
  mutate_at(vars(from, to), ~stringr::str_remove_all(.x, ":") %>% as.numeric())  %>% count(dag)

hchart(
  opening %>% filter(cat == "2"),# %>% filter(bad %in% c("Mirandabad", "Zuiderbad")),
  "xrange",
  hcaes(x = start, x2 = end, y = cat, color = bad, group = bad, fill = bad)
)  %>% 
  hc_yAxis(
    title = FALSE,
    categories = c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "") %>% rev
  )  # %>%
  # hc_xAxis(
    # title = FALSE, dateTimeLabelFormats = list(day = '%h:%m'), type = "datetime")
