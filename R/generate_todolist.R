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
  if(is.null(filename)){
    context <- rstudioapi::getActiveDocumentContext()
    filename <- context$path
    if(filename == '' ) stop(call. = FALSE, 'Save file first')
    contents <- context$contents
  } else {
    contents <- readLines(filename)
  }

  todo_list_keywords <- "(?is)(#+'* *todo:?)|(#+'* *fixme:?)"

  basePath <- rstudioapi::getActiveProject()

  todolist <- str_subset(string = contents,
                         pattern = todo_list_keywords) %>%
    str_replace(pattern = todo_list_keywords,
                replacement = "") %>%
    str_trim(side = "both")
  locations <- which(str_detect(string = contents,
                                pattern = todo_list_keywords))
  markers <- lapply(seq_along(todolist), function(x){
    marker <- list()
    marker$type <- "usage"
    marker$file <- filename
    marker$line <- locations[x]
    marker$column <- 1
    marker$message <- todolist[x]
    return(marker)
  })

  rstudioapi::callFun("sourceMarkers",
                      name = paste("To Do List for", filename),
                      markers = markers,
                      basePath = basePath)
  return(invisible())
}

