library(tidyverse)
library(rvest)
library(janitor)
library(xml2)
library(polite)
library(jsonlite)
library(httr2)
library(matter)
library(data.table)


mbappe_url <- "https://www.futek.io/85139014"
form_url <- "https://www.futek.io/"

values <- jsondata$nationality

scrape_data_for_value <- function(value) {
  # Read the initial form page
  session <- session(form_url)
  form <- html_form(session)[[1]]
  
  # Fill the form with the current value
  filled_form <- html_form_set(form, `nationality-search1` = value)
  
  # Submit the form
  result_page <- html_form_submit(filled_form)
  
  # Scrape data from the resulting page
  scraped_data <- read_html(result_page) %>% 
    html_elements(".fc-attributes-table") %>% 
    html_elements("a") %>% 
    html_attr("href")     
  
  return(scraped_data)
}

player_ids <- lapply(values, scrape_data_for_value)

player_ids <- rbind(player_ids)

player_url_list <- unlist(as.list(player_ids))

chunk_list1 <- lapply(chunk_test$`1`, get_player_data)

chunk_test <- split(player_url_list, ceiling(seq_along(player_url_list)/500))

for (vector_index in seq_along(chunk_test)) {
  player_vector <- chunk_test[[vector_index]]
  
  # Iterate through each player in the current vector
  for (player_index in seq_along(player_vector)) {
    player <- player_vector[player_index]
    player_data <- get_player_data(player)
    
    # Define the filename, incorporating the vector index and player index
    filename <- paste0("player_data_", vector_index, "_", player_index, ".csv")
    
    # Save the data as a CSV file
    write.csv(player_data, filename, row.names = FALSE)
  }
}

file_list <- list.files(path = "~/Desktop/R/Football Analysis/FMRows")
  
df <- lapply(file_list, read_csv)

df_bind <- rbindlist(df)

df_bind <- as_tibble(df_bind)

df_bind <- df_bind %>%
  select(player_id, everything())

write.csv(df_bind, file = "fm_player_data.csv")

get_player_data <- function(player_url) {
  
  url <- get_player_url(player_url)
  html_content <- read_html(url)

  player_id <- str_replace_all(player_url, "/", "")
  player_info <- get_player_info(html_content)
  attributes <- get_player_attributes(html_content)

  data <- get_player_row(player_info, attributes)
  data$player_id <- as.character(player_id)
  return(data)
}

get_player_url <- function(player_url) {
  
  main_url <- "https://www.futek.io"
  player_url <- paste0(main_url, player_url)
  
  return(player_url)
}
  
  
  
get_player_attributes <- function(html_content) {
  attributes <- html_content %>% 
    html_elements(".player-card-column") %>% 
    html_table(convert = FALSE) %>% 
    bind_rows()
  
  return(attributes)
}

get_player_info <- function(html_content) {
  player_info <- html_content %>% 
    html_elements(".player-info p") %>% 
    html_text2() %>% 
    enframe(name = NULL, value = "value") %>% 
    separate(value, into = c("X1", "X2"), sep = ": ", extra = "merge")
  
  return(player_info)
}

get_player_row <- function(player_info, attributes) {
  bind_rows(player_info, attributes) %>% 
    pivot_wider(names_from = X1, values_from = X2)
}

