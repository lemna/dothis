#' Generate a "To Do" List in the markers pane
#'
#' This function should never be called directly.
#'
#' @return returns invisibly
#' @import stringr
#' @export
generate_todolistAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  print(context$path)
  todo_list_keywords <- "(?is)(#+'* *todo:?)|(#+'* *fixme:?)"
  todolist <- str_subset(string = context$contents,
                         pattern = todo_list_keywords) %>%
    str_replace(pattern = todo_list_keywords,
                replacement = "") %>%
    str_trim(side = "both")
  locations <- which(str_detect(string = context$contents,
                                pattern = todo_list_keywords))
  markers <- lapply(seq_along(todolist), function(x){
    marker <- list()
    marker$type <- "usage"
    marker$file <- context$path
    marker$line <- locations[x]
    marker$column <- 1
    marker$message <- todolist[x]
    return(marker)
  })

  rstudioapi::callFun("sourceMarkers",
                      name = "To Do List",
                      markers = markers)
  return(invisible())
}

