#' Run an aqueduct Workflow
#' 
#' This function runs the workflow provided in the arguments, ignoring 
#' components of the workflow (nodes) whose parent nodes have not changed
#' 
#' @param outdir(outdata)~nodename(dir1(indata1)+dir2(indata2)+...) 
#'    A formula instructing which datasets (e.g. \code{indata1})
#'    a workflow node (\code{nodename}) accepts as input
#'    
#'     
#' @return A dataframe of all of the workflow nodes and if they have been
#' activated
#' 
#' 
aqueduct_setup = function(...){
  arguments = list(...)
  print(arguments)
}
