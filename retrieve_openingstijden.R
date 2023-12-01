1# Install the 'RSelenium' package if it is not already installed
# install.packages("RSelenium")

# Load the 'RSelenium' package
library(RSelenium)
library(dplyr)
library(rvest)
library(xml2)

# Start a local Selenium server

rd <- rsDriver(port = sample(4000L:4999L, size = 1), browser = "firefox",
               chromever = NULL) # alt: chromever = NULL

# Open a new session on the Selenium server
remDr <- rd$client

# Navigate to the page containing the button
remDr$navigate("https://www.amsterdam.nl/demirandabad/rooster/")

# Use the 'findElements' function to find all buttons with the specified attributes
buttons <- remDr$findElements(using = "css selector", value = "a[role='tab'][aria-selected='false'][aria-setsize='3'][aria-posinset='3'][href='#'][target='_self'][class='nav-link']")
buttons
# Iterate through the buttons and simulate clicking on each one
for (button in buttons) {
  button$clickElement()
}


# Use the 'findElement' function to find the date picker button
button <- remDr$findElement(using = "id", value = "primary-data-picker")

# Use the 'clickElement' function to simulate clicking on the button
button$clickElement()

# Use the 'findElement' function to find the date picker dialog
dialog <- remDr$findElement(using = "id", value = "__BVID__26__calendar-grid_")
dialog$sendKeysToElement(list("\uE007"))





# Use the 'findElement' function to find the date picker dialog
dialog <- remDr$findElement(using = "id", value = "__BVID__26__calendar-grid_")
dialog$sendKeysToElement(list("\uE014"))
dialog$sendKeysToElement(list("\uE007"))

retrieve_table <- function(x) {
  
}

rawhtml <- remDr$getPageSource() %>% 
  .[[1]]


pg <- rawhtml %>% 
  read_html() 

xml_find_all(pg, ".//br") %>% xml_add_sibling("p", "\n")

xml_find_all(pg, ".//br") %>% xml_remove()

aciviteiten_list <- pg %>% html_table() %>% 
  .[[3]] %>% 
  janitor::clean_names() %>% 
  mutate(date = Sys.Date()+1) %>% 
  tidyr::separate(tijdsslot, "-", into = c("from", "to")) %>% 
  tidyr::separate(activiteit, "\n", into = c("activity", "details")) %>% 
  mutate_at(vars(activity, details, from, to), stringr::str_trim)
  



