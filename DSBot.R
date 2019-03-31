library(httr)
library(rvest)
library(purrr)
library(jsonlite)
library(future)
library(magrittr)
library(lubridate)
library(RSQLite)
library(dbplyr)

future::plan(multiprocess)

source("create_exercise_dialog.R")
source("manage_exercise.R")

db <- dbConnect(SQLite(), "data/exercises.sql")

token <- Sys.getenv("SLACK_BOT_TOKEN")


#* @post /exercise
#* @serializer unboxedJSON

function(req, res, channel_id, text, trigger_id) {
  
  #saveRDS(req, file = "exercise_request.rds")
  #saveRDS(res, file = "exercise_response.rds")
  
  episode = trimws(text) %>% stringr::str_remove_all("^<|>$")
  
  if(!stringr::str_detect(episode, "https?://csiro-data-school.github.io")) {
    return(list(text = "Please try again with the link to an episode."))
  }
  
  resp <- create_exercise_dialog(token, episode, trigger_id)
  
  resp <- content(resp)
  
  if(!resp$ok) return(list(text = glue::glue_data(resp, "**OK**: {ok}, **Warning**: {warning}, **Error:** {error}")))
  
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
      f %<-% manage_exercise(token, db, payload)
    }
  }
  
  #Clicked button at end of exercise
  if (payload$type == "block_actions") {
    if (stringr::str_detect(payload$actions$block_id, "exercise_buttons")) {
     f %<-% process_exercise_button_click(token, db, payload)
    }
  }
  
  res$status <- 200
  return(res)
}
#* @post /slack
#* @json
function(challenge) {
  list(challenge=challenge)
}
