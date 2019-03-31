create_exercise_dialog <- function(token, episode, trigger_id) {

  challenges <- GET(episode) %>% 
    content %>% 
    html_nodes("blockquote") %>% 
    keep(~html_attr(., "class") == "challenge")
  
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
          subtype = "number",
          name = "time",
          label = "How long to run (in minutes)?"
        )
      )
    )
  )  
  
  r = POST("https://slack.com/api/dialog.open",add_headers("Authorization" = glue::glue("Bearer {token}")), body = body, encode = "json")
  
  r
}

