library(purrr)
library(jsonlite)
library(httr)
library(rvest)
library(RSQLite)
library(dplyr)
library(magrittr)

token <- Sys.getenv("SLACK_BOT_TOKEN")
db <- dbConnect(SQLite(), "data/exercises.sql")

payload <- commandArgs(trailingOnly = T) %>% fromJSON()

d <- stringr::str_split(payload$actions$block_id, "\\|")

challenge_title <- GET(d[[1]][2]) %>% content %>% html_node(glue::glue("#{d[[1]][3]}")) %>% html_text()

if(payload$actions$action_id == "exercise_need_help") {
  
  #find_helpers
  helpers <- readRDS("data/instructors.rds")
  
  call_helpers <- list(users = stringr::str_flatten(helpers, collapse = ","))
  
  #open message with helpers
  resp <- POST("https://slack.com/api/conversations.open",add_headers("Authorization" = glue::glue("Bearer {token}")), body = call_helpers, encode = "json")
  
  helper_pm <- content(resp)
  
  #print(helper_pm)
  
  #write user needing help to helpers
  if(helper_pm$ok) {
    helper_message <- list(
      channel = helper_pm$channel$id,
      response_type = "in_channel",
      as_user = T,
      text = glue::glue("<@{payload$user$id}> needs some help with {challenge_title}.")
    )
    
  resp <-  POST("https://slack.com/api/chat.postMessage",add_headers("Authorization" = glue::glue("Bearer {token}")), body = helper_message, encode = "json")
  #print(content(resp))
  
  #Log the help request
  log_data <- tibble::tibble(id = digest::digest(glue::glue("{payload$message$ts}:{payload$actions$block_id}")), timestamp = as.character(lubridate::now()), episode = d[[1]][2], challenge = d[[1]][3], user = payload$user$id, channel = payload$channel$name, action = "help")
  dbWriteTable(db, 'exercises', log_data, append = T)
  }
  
  POST("https://slack.com/api/chat.postEphemeral",add_headers("Authorization" = glue::glue("Bearer {token}")), body = list(channel = payload$channel$id, user = payload$user$id, text = "I've let the instructors know. Someone will help you out when they are free.", as_user = T), encode = "json")
}

if(payload$actions$action_id == "exercise_finished") {
  
  log_data <- tibble::tibble(id = digest::digest(glue::glue("{payload$message$ts}:{payload$actions$block_id}")), timestamp = as.character(lubridate::now()), episode = d[[1]][2], challenge = d[[1]][3], user = payload$user$id, channel = payload$channel$name, action = "finished")
  dbWriteTable(db, 'exercises', log_data, append = T)
  
  #Haven't seen user before
  if (!payload$user$id %in% (tbl(db, from = "users") %>% collect() %>% .$user)) {
    res <- POST("https://slack.com/api/users.info",add_headers("Authorization" = glue::glue("Bearer {token}")), body = list(user = payload$user$id)) %>% content()
    if(res$ok) {
      
      name <- ifelse(res$user$profile$display_name == "", res$user$profile$real_name, res$user$profile$display_name)
      
      dbWriteTable(db, "users", tibble::tibble(user = res$user$id, name = name, avatar = res$user$profile$image_24), append = T)
      
    }
  }
  POST("https://slack.com/api/chat.postEphemeral",add_headers("Authorization" = glue::glue("Bearer {token}")), body = list(channel = payload$channel$id, user = payload$user$id, text = praise::praise("${Adjective}! We'll move on to the next topic soon."), as_user = T), encode = "json")
  
}