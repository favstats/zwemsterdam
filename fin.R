library(httr)
library(tidyverse)
library(lubridate)

source("utils.R")

bader <- c("zuiderbad", "noorderparkbad", "de-mirandabad", "flevoparkbad", "brediusbad")


zwemdat <- bader %>% 
  map_dfr(get_timetables)  %>% filter(activity == "Banenzwemmen") %>% 
  mutate( cat = as.factor(bad) %>% as.numeric() %>% magrittr::subtract(1)) %>%
  mutate(start = as.numeric(start),  # Ensure start is numeric
         end = as.numeric(end))     # Ensure end is numeric



# zwemdat%>% 
# #   filter(bad == "zuiderbad") %>% 
# # filter(dag == "dinsdag") %>% 
#   group_split(dag, bad) %>% 
#   map_dfr(aggregate_timeframes) %>% View()
  
fin <- zwemdat %>% 
  filter(str_detect(activity, "Banenzwemmen")) %>% 
  filter(str_detect(extra, "Naak|chronisch|senioren", negate = T)) %>% 
  mutate(dag = str_to_title(dag)) %>% 
  group_split(dag, bad) %>% 
  map_dfr(aggregate_timeframes) %>% 
  bind_rows(kidinschool) %>% 
  bind_rows(hetmarnix) %>% 
  mutate(dag = fct_relevel(dag, c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")))


saveRDS(fin, file = "data/fin.rds")
