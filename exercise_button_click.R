library(purrr)
library(jsonlite)
library(httr)
library(rvest)

token <- Sys.getenv("SLACK_BOT_TOKEN")

payload <- commandArgs(trailingOnly = T) %>% fromJSON()

if(payload$actions$action_id == "exercise_need_help") {
  
  #find_helpers
  helpers <- c("UHAGQKENT")
  
  call_helpers <- list(users = stringr::str_flatten(helpers, collapse = ","))
  
  #open message with helpers
  resp <- POST("https://slack.com/api/conversations.open",add_headers("Authorization" = glue::glue("Bearer {token}")), body = call_helpers, encode = "json")
  
  helper_pm <- content(resp)
  
  print(helper_pm)
  
  #write user needing help to helpers
  if(helper_pm$ok) {
    helper_message <- list(
      channel = helper_pm$channel$id,
      response_type = "in_channel",
      text = glue::glue("<@{payload$user$id}> needs some help with the challenge.")
    )
    
  resp <-  POST("https://slack.com/api/chat.postMessage",add_headers("Authorization" = glue::glue("Bearer {token}")), body = helper_message, encode = "json")
  print(content(resp))
  
  }
}