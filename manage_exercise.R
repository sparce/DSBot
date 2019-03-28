library(purrr)
library(jsonlite)
library(httr)
library(rvest)
library(lubridate)

token <- Sys.getenv("SLACK_BOT_TOKEN")

payload <- commandArgs(trailingOnly = T) %>% fromJSON()

end_time <- now() + minutes(payload$submission$time)

ep <- GET(payload$state) %>% content()

challenges <- ep %>% 
  html_nodes("blockquote") %>% 
  keep(~html_attr(., "class") == "challenge")

running_challenge <- challenges[(html_node(challenges, "h2") %>% html_attr("id")) == payload$submission$challenge]

challenge_text <- running_challenge %>% 
  html_nodes(xpath = "p|div") %>% 
  stringr::str_replace_all(c("</?pre.*?>" = "``","</?code.*?>" = "`", "</?.*?>" = "", "\\n" = " ")) %>% 
  glue::glue_collapse("\n")


time_left <- function() {
  secs_left <- difftime(end_time, now(), units = "sec") %>% as.integer()
  
  glue::glue("{floor(secs_left / 60)}:{sprintf('%0.2d', secs_left%%60)}")
}

message <- list(
  response_type = "in_channel",
  channel = payload$channel$id,
  text = challenge_text,
  blocks = list(
    list(
      type = "section",
      text = list(
        type = "mrkdwn",
        text = challenge_text
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
      elements = list(
        list(
          type = "button",
          text = list(
            text = ":thumbsup: Completed",
            type = "plain_text",
            emoji = T
          ),
          value = "finished"
        ),
        list(
          type = "button",
          text = list(
            text = ":sos: I need help",
            type = "plain_text",
            emoji = T
          ),
          value = "help"
        )
      )
    )
  )
)

resp <- POST("https://slack.com/api/chat.postMessage",add_headers("Authorization" = glue::glue("Bearer {token}")), body = message, encode = "json")

saveRDS(resp, "resp.rds")

#Update message with time remaining for challenge
while(end_time > now()) {
  
  old_message <- content(resp)
  
  #Update time left
  new_blocks <- old_message$message$blocks
  new_blocks[[2]]$elements[[1]]$text <- glue::glue("*Time left:* {time_left()}")
  
  updated_message <- list(
    channel = old_message$channel,
    text = old_message$message$text,
    ts = old_message$ts,
    blocks = new_blocks
  )
  
  resp <- POST("https://slack.com/api/chat.update",add_headers("Authorization" = glue::glue("Bearer {token}")), body = updated_message, encode = "json")
  
  Sys.sleep(5)

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

