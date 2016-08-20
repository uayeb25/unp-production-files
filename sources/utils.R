library(jsonlite)

update_model <- function(model,need_update){
  load <- fromJSON(paste0("https://mbi-laureate.appspot.com/model/",model,"/need_update/",need_update))
  print(load)
}

need_model_update <- function(model){
  load <- fromJSON(paste0("https://mbi-laureate.appspot.com/model/",model))
  as.logical(load$need_update)
}