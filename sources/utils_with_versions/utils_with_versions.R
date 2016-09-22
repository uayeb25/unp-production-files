send.email.model.completed <- function(university,
                                       model,
                                       high_finance,
                                       medium_finance,
                                       high_academic,
                                       medium_academic,
                                       risk,
                                       safe){
  send_email <- paste0("https://mbi-laureate.appspot.com/prediction/send/",
                       university,"/",
                       model,"/",
                       high_finance,"/",
                       medium_finance,"/",
                       high_academic,"/",
                       medium_academic,"/",
                       risk,"/",
                       safe)
  
  print(send_email)
  
  load <- fromJSON(send_email)
  
  print(load)
  
}

send.slack.model.completed <- function(university,
                                       model,
                                       high_finance,
                                       medium_finance,
                                       high_academic,
                                       medium_academic,
                                       risk,
                                       safe){
  send_email <- paste0("https://mbi-laureate.appspot.com/slack/send/",
                       university,"/",
                       model,"/",
                       high_finance,"/",
                       medium_finance,"/",
                       high_academic,"/",
                       medium_academic,"/",
                       risk,"/",
                       safe)
  
  print(send_email)
  
  load <- fromJSON(send_email)
  
  print(load)
  
}

add.email.for.model.completed <- function(email){
  url <- paste0("https://mbi-laureate.appspot.com/email/",email,"/action/create/proposal/model-completed")
  load <- fromJSON(url)
  print(load)
}

delete.email.for.model.completed <- function(email){
  url <- paste0("https://mbi-laureate.appspot.com/email/",email,"/action/delete/proposal/model-completed")
  load <- fromJSON(url)
  print(load)
}

add.email.for.errors <- function(email){
  url <- paste0("https://mbi-laureate.appspot.com/email/",email,"/action/create/proposal/errors")
  load <- fromJSON(url)
  print(load)
}

delete.email.for.errors <- function(email){
  url <- paste0("https://mbi-laureate.appspot.com/email/",email,"/action/delete/proposal/errors")
  load <- fromJSON(url)
  print(load)
}

send.email.message <- function(message,university){
  message <- str_replace_all(message," ","%20")
  url <- paste0("https://mbi-laureate.appspot.com/send/message/",message,"/to/",university)
  load <- fromJSON(url)
  print(load)
}