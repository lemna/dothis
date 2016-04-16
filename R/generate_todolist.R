#' Generate a "To Do" List in the Rstudio markers pane
#'
#' This function looks for comments in a file starting with the
#' words 'todo' or 'fixme' and gathers them in the markers pane. This function
#' should never be called directly - it is added as an item in the Addins menu.
#'
#' @param filename optional filename
#'
#' @return returns invisibly
#' @import rstudioapi stringr
#' @export
generate_todolistAddin <- function(){
  generate_todolist()
}

#' @rdname generate_todolistAddin
generate_todolist_projectAddin <- function(){
  rstudioapi::verifyAvailable()
  project <- rstudioapi::getActiveProject()
  filenames <- list.files(path = project,
                          pattern = "(.R$)|(.Rmd$)",
                          include.dirs = TRUE,
                          recursive = TRUE)
  lapply(filenames, generate_todolist)
  return(invisible())
}

#' @rdname generate_todolistAddin
generate_todolist <- function(filename = NULL){
  rstudioapi::verifyAvailable()
  if (is.null(filename)){
    context <- rstudioapi::getActiveDocumentContext()
    if (context$id == "#console") return(invisible())
    filename <- context$path
    if (filename == "" ) stop(call. = FALSE, "Save file first")
    contents <- context$contents
  } else {
    contents <- readLines(filename)
  }

  todo_list_keywords <- c("(?is)(#+'* *todo:?)", "(?is)(#+'* *fixme:?)")
  names(todo_list_keywords) <- c("To Do List", "Fix Me List")

  basePath <- rstudioapi::getActiveProject()

  get_todo_lists <- function(string, pattern){
    messages <- str_subset(string = string, pattern = pattern) %>%
      str_replace(pattern = pattern, replacement = "") %>%
      str_trim(side = "both")
    locations <- which(str_detect(string = string, pattern = pattern))
    result <- lapply(seq_along(messages), function(x){
      marker <- list()
      marker$type <- "usage"
      marker$file <- filename
      marker$line <- locations[x]
      marker$column <- 1
      marker$message <- messages[x]
      return(marker)
    })
    return(result)
  }

  todolist <- lapply(todo_list_keywords, function(pattern)
    get_todo_lists(string = contents, pattern = pattern))
  lapply(seq_along(todolist),
         function(x) rstudioapi::callFun("sourceMarkers",
                                         name = names(todolist)[x],
                                         markers = todolist[[x]],
                                         basePath = basePath))
  return(invisible())
}
