
packages <- c("plyr","jsonlite","curl","stringr","caret")

check.install.load.Package<-function(package_name){
  if(!package_name%in%installed.packages()){install.packages(package_name)}
  library(package_name,character.only = TRUE)} 

for(package in packages){check.install.load.Package(package)}


send.email.model.completed <- function(model,
                                       number,
                                       red,
                                       orange,
                                       yellow,
                                       green){
  send_email <- paste0("https://mbi-laureate.appspot.com/prediction/send/",
                       model,"/",
                       number,"/",
                       red,"/",
                       orange,"/",
                       yellow,"/",
                       green)
  
  print(send_email)
  
  load <- fromJSON(send_email)
  
  print(load)
  
}

send.slack.model.completed <- function(model,
                                       number,
                                       red,
                                       orange,
                                       yellow,
                                       green){
  send_email <- paste0("https://mbi-laureate.appspot.com/slack/send/",
                       model,"/",
                       number,"/",
                       red,"/",
                       orange,"/",
                       yellow,"/",
                       green)
  
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

list.of.packages <- c("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


