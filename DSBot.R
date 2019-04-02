library(httr)
library(rvest)
library(purrr)
library(jsonlite)
library(magrittr)
library(lubridate)
library(RSQLite)
library(dbplyr)


db <- dbConnect(SQLite(), "data/exercises.sql")

token <- Sys.getenv("SLACK_BOT_TOKEN")


userify <- function(user_id) {
  return(glue::glue("<@{user_id}>"))
}

#* @post /exercise
#* @serializer unboxedJSON

function(req, res, channel_id, text, trigger_id) {
  
  #saveRDS(req, file = "exercise_request.rds")
  #saveRDS(res, file = "exercise_response.rds")
  
  episode = trimws(text) %>% stringr::str_remove_all("^<|>$")
  
  if(!stringr::str_detect(episode, "https?://csiro-data-school.github.io")) {
    return(list(text = "Please try again with the link to an episode."))
  }
  
  system(glue::glue("Rscript create_exercise_dialog.R {episode} {trigger_id}"), wait = F, ignore.stdout = T, ignore.stderr = T)
  
  res$status <- 200
  return(res)
}

#* @post /actions
#* @serializer unboxedJSON
function(req, res, payload) {
  
  payload <- jsonlite::fromJSON(payload)
  saveRDS(payload, "payload.rds")
  
  #Response is from 'Exercise' dialog box; pass off to exercise manager
  if (payload$type == "dialog_submission") {
    if(payload$callback_id == "exercise_callback"){
      system(paste0("Rscript manage_exercise.R"," '", toJSON(payload, auto_unbox = T), "'"), wait = F, ignore.stdout = T, ignore.stderr = T)
    }
  }
  
  #Clicked button at end of exercise
  if (payload$type == "block_actions") {
    if (stringr::str_detect(payload$actions$block_id, "exercise_buttons")) {
      system(paste0("Rscript exercise_button_click.R"," '", toJSON(payload, auto_unbox = T), "'"), wait = F, ignore.stdout = T, ignore.stderr = T)
    }
  }
  
  res$status <- 200
  return(res)
}
#* @post /slack
#* @json
function(req, res, event, authed_users, challenge) {
  
  #Someone uses @DSBot in channel
  if(event$type == "app_mention") {
    
    #Setting instructors for the day
    if(stringr::str_detect(event$text, "(?i)instruct")) {
      users <- stringr::str_extract_all(event$text, "<@.*?>")[[1]] %>% 
        stringr::str_remove_all("[<@>]")
      
      non_bots <- users[!users %in% authed_users]
      
      saveRDS(non_bots, "data/instructors.rds")
      
      
      POST("https://slack.com/api/chat.postEphemeral",add_headers("Authorization" = glue::glue("Bearer {token}")), body = list(channel = event$channel, user = event$user, text = glue::glue("OK, I\'ll let {glue::glue_collapse(userify(non_bots), sep = ', ', last = ' & ')} know if anyone needs help."), as_user = T), encode = "json")
      
      #return(list(text=glue::glue("OK, I\'ll let {glue::glue_collapse(userify(non_bots), sep = ', ', last = ' & ')} know if anyone needs help.")))
    }
    
  }
  
  
  res$status <- 200
  return(res)
  #Return challenge for url authorisation
  #list(challenge=challenge)
}
