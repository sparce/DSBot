library(purrr)
library(jsonlite)
library(httr)
library(rvest)
library(lubridate)
library(RSQLite)
library(dplyr)
library(magrittr)

token <- Sys.getenv("SLACK_BOT_TOKEN")
db <- dbConnect(SQLite(), "data/exercises.sql")

payload <- commandArgs(trailingOnly = T) %>% fromJSON()

end_time <- now() + minutes(payload$submission$time)

ep <- GET(payload$state) %>% content()

challenges <- ep %>% 
  html_nodes("blockquote") %>% 
  keep(~html_attr(., "class") == "challenge")

running_challenge <- challenges[(html_node(challenges, "h2") %>% html_attr("id")) == payload$submission$challenge]

challenge_text <- running_challenge %>% 
  html_nodes(xpath = "p|div|pre|h1|h2|h3|h4") %>% 
  discard(~html_attr(., "id") %in% payload$submission$challenge) %>% 
  #stringr::str_replace_all(c("</?pre.*?>" = "``","</?code.*?>" = "`", "</?.*?>" = "", "\\n" = " ")) %>% 
  #format if not a code block
  map_if(~!html_name(.) %in% c("div", "pre"), ~stringr::str_replace_all(., c("</?code.*?>" = "`", "</?h\\d.*?>" = "*", "</?.*?>" = "", "\\n" = " ", "  +?" = " ")) %>% stringr::str_wrap(width = 90)) %>%
  #if code block
  map_if(~class(.) == "xml_node", ~stringr::str_replace_all(., c("</?pre.*?>" = "```", "</?.*?>" = ""))) %>%
  trimws() %>%
  stringr::str_flatten("\n")

time_left <- function() {
  secs_left <- difftime(end_time, now(), units = "sec") %>% as.integer()
  
  glue::glue("{floor(secs_left / 60)}:{sprintf('%0.2d', secs_left%%60)}")
}

challenge_title <- glue::glue("<{payload$state}#{payload$submission$challenge}|*{running_challenge %>% html_node('h2') %>% html_text()}*>")

message <- list(
  response_type = "in_channel",
  channel = payload$channel$id,
  text = glue::glue("{challenge_title}\n{challenge_text}"),
  blocks = list(
    list(
      type = "section",
      text = list(
        type = "mrkdwn",
        text = glue::glue("{challenge_title}\n{challenge_text}")
      )
    ),
    list(
      type = "context",
      elements = list(
        list(
          type = "mrkdwn",
          text = glue::glue("*Time left:* {time_left()}")
        )
      )
    ),
    list(
      type = "actions",
      block_id = glue::glue("exercise_buttons|{payload$state}|{payload$submission$challenge}"),
      elements = list(
        list(
          type = "button",
          action_id = "exercise_finished",
          text = list(
            text = ":thumbsup: Completed",
            type = "plain_text",
            emoji = T
          ),
          value = glue::glue("finished_{payload$submission$challenge}")
        ),
        list(
          type = "button",
          action_id = "exercise_need_help",
          text = list(
            text = ":sos: I need help",
            type = "plain_text",
            emoji = T
          ),
          value = glue::glue("help_{payload$submission$challenge}")
        )
      )
    )
  )
)

resp <- POST("https://slack.com/api/chat.postMessage",add_headers("Authorization" = glue::glue("Bearer {token}")), body = message, encode = "json")

saveRDS(resp, "resp.rds")

# Log the exercise start

exercise_message <- content(resp)
log_data <- tibble::tibble(id = digest::digest(glue::glue("{exercise_message$message$ts}:{glue::glue('exercise_buttons|{payload$state}|{payload$submission$challenge}')}")), timestamp = as.character(lubridate::now()), episode = payload$state, challenge = payload$submission$challenge, user = payload$user$id, action = "created")
dbWriteTable(db, 'exercises', log_data, append = T)

#Update message with time remaining for challenge
while(end_time > now()) {
  print("Start loop")
  old_message <- content(resp)
  
  #Update time left
  new_blocks <- old_message$message$blocks
  new_blocks[[2]]$elements[[1]]$text <- glue::glue("*Time left:* {time_left()}")
  
  #Update completed users
  challenge_id <- digest::digest(glue::glue("{old_message$ts}:{old_message$message$blocks[[3]]$block_id}"))
  completed_users <- tbl(db, "exercises") %>% filter(id == challenge_id, action == "finished") %>% left_join(tbl(db, "users")) %>% select(name, avatar) %>% collect() %>% group_by(name, avatar) %>% summarise()  %$% map2(name, avatar, ~list(type = "image", alt_text = .x, image_url = .y))
  
  print(completed_users)
  
  completed_block <- list(
    type = "context",
    elements = c(list(list(type= "mrkdwn", text = glue::glue("{length(completed_users)} completed:"))) , completed_users)
  )
  
  if(length(completed_users)>0) {
    new_blocks[[4]] <- completed_block
  }
  
  updated_message <- list(
    channel = old_message$channel,
    text = old_message$message$text,
    ts = old_message$ts,
    blocks = new_blocks
  )
  
  resp <- POST("https://slack.com/api/chat.update",add_headers("Authorization" = glue::glue("Bearer {token}")), body = updated_message, encode = "json")
  
  Sys.sleep(0.5)

}


#post final message
old_message <- content(resp)

#Update time left
new_blocks <- old_message$message$blocks
new_blocks[[2]]$elements[[1]]$text <- "*Exercise complete*"

updated_message <- list(
  channel = old_message$channel,
  text = old_message$message$text,
  ts = old_message$ts,
  blocks = new_blocks
)

resp <- POST("https://slack.com/api/chat.update",add_headers("Authorization" = glue::glue("Bearer {token}")), body = updated_message, encode = "json")

