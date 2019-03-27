library(httr)
library(rvest)
library(purrr)
library(jsonlite)

#* @post /exercise
#* @serializer unboxedJSON

function(req, res, channel_id, text, trigger_id) {
  episode = trimws(text) %>% stringr::str_remove_all("^<|>$")
  
  if(!stringr::str_detect(episode, "https?://csiro-data-school.github.io")) {
    return(list(text = "Please try again with the link to an episode."))
  }
  
  challenges <- GET(episode) %>% 
    content %>% 
    html_nodes("blockquote") %>% 
    keep(~html_attrs(.) == "challenge")

  challenge_ids <- html_node(challenges, "h2") %>% html_attr("id")
  challenge_names <- html_node(challenges, "h2") %>% html_text()
  
  map2(challenge_names, challenge_ids, function(nm, id) {
    list(label = nm, value = id)
  })
   
  body <- list(
    trigger_id = trigger_id,
    dialog = list(
      title = "Start an exercise",
      callback_id = "exercise_callback",
      state = episode,
      elements = list(
        list(
          type = "select",
          name = "challenge",
          label = "Which challenge?",
          options = map2(challenge_names, challenge_ids, function(nm, id) {
            list(label = nm, value = id)
          })
        ),
        list(
          type = "text",
          name = "time",
          label = "How long to run (in minutes)?"
        )
      )
    )
  )  
  
  r = POST("https://slack.com/api/dialog.open",add_headers("Authorization" = "Bearer xoxp-587828095701-588568660775-588633919799-bf4ed98866dcf2ab7bfda054c430f827"), body = body, encode = "json")
  
  res$status <- 200
  return(res)
}

#* @post /actions
#* @serializer unboxedJSON
function(req, res, payload) {
  res$status <- 200
  
  payload <- jsonlite::fromJSON(payload)
  payload
  
  saveRDS(object = payload, file = "payload.rds")
  
  # body <- list(
  #   response_type = "ephemeral",
  #   text = "fallback text",
  #   blocks = list(
  #     list(
  #       type = "section",
  #       text = list(
  #         type = "mrkdwn",
  #         text = "You responded:"
  #       )
  #     ),
  #     list(
  #       type = "divider"
  #     ),
  #     list(
  #       type = "section",
  #       text = list(
  #         type = "mrkdwn",
  #         text = glue::glue_data(payload$submission, "{challenge} and {time}")
  #       )
  #     )
  #   )
  # )
  # 
  # POST(payload$response_url, body = body, encode = "json")
  
  system(paste0("Rscript manage_exercise.R"," '", toJSON(payload, auto_unbox = T), "'"), wait = F, ignore.stdout = T, ignore.stderr = T)
  
  return(res)
}
#* @post /slack
#* @json
function(challenge) {
  list(challenge=challenge)
}
