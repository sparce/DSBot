library(purrr)
library(jsonlite)
library(httr)
library(rvest)
payload <- commandArgs(trailingOnly = T) %>% fromJSON()

browser()

print(payload)

ep <- GET(payload$state) %>% content()

challenges <- ep %>% 
  html_nodes("blockquote") %>% 
  keep(~html_attrs(.) == "challenge")

running_challenge <- challenges[(html_node(challenges, "h2") %>% html_attr("id")) == payload$submission$challenge]

challenge_text <- running_challenge %>% 
  html_nodes(xpath = "p|div") %>% 
  stringr::str_replace_all(c("</?pre.*?>" = "``","</?code.*?>" = "`", "</?.*?>" = "", "\\n" = " ")) %>% 
  glue::glue_collapse("\n")


message <- list(
  response_type = "in_channel",
  text = "fallback_text",
  blocks = list(
    list(
      type = "section",
      text = list(
        type = "mrkdwn",
        text = challenge_text
      )
    )
  )
)

POST(payload$response_url, body = message, encode = "json")