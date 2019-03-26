library(httr)
library(rvest)
library(purrr)

#* @post /exercise
#* @json

function(text, response_url, trigger_id) {
  
  
  
  list(text, response_url, trigger_id)
  
}

#* @post /slack
#* @json
function(challenge) {
  list(challenge=challenge)
}
