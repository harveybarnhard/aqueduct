#' Run an aqueduct Workflow
#' 
#' This function runs the workflow provided in the arguments, ignoring 
#' components of the workflow (nodes) whose parent nodes have not changed
#' 
#' @return A dataframe of all of the workflow nodes and if they have been
#' activated
#' 
#' 
aqueduct = function(...){
  arguments = list(...)
  print("hello")
}