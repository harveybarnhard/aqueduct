#' Set up aqueduct workflow
#' 
#' This function establishes filepaths that are to be used in an
#' aqueduct workflow by defining the filepaths and their names. Shorter names
#' make more readable workflows. These filepaths are stored in a hidden
#' aqueduct environment to avoid clutter (an environment that is established
#' in this folder)
#' @param main
#'     Directory in which you want the aqueduct files to be located, provided
#'     as an exact formula (see entry below). If \code{update=TRUE} then this
#'     parameter need not be provided.
#' @param ...
#'     Filepath formulas, separated by commas. There are two kinds of formulas.
#'     An exact formula should be given as a string. These work best for 
#'     establishing the basepaths from the root of your system from which
#'     all other paths are derived. For example,
#'     \deqn{basepath = "C:/Users/example_user"}
#'     The formulas may also be given as relative paths, where the first level
#'     is derived from previously defined filepaths. For example,
#'     \deqn{datapath ~ basepath/data/raw}
#'     which would correspond to "C:/Users/example_user/data/raw"
#' @param update 
#'     \code{TRUE} by default. If \code{FALSE}, then a new environment is
#'     created and any previous filepaths are erased. If \code{TRUE} then any
#'     filepaths given will be added to the list of previous filepaths
#' @examples 
#' 
#' 
aqueduct_setup = function(main="", ..., update=TRUE){
  # Create a hidden environment for aqueduct to work in without cluttering
  # up the local environment
  if(update==FALSE){
    .aqueduct_env = new.env()
  }
  if(main!=""){
    assign("main", main, envir=.aqueduct_env)
    dir.create(file.path(main, ".aqueduct"), showWarnings=FALSE)
  }
  paths = list(...)
  # Loop over each input
  for(i in 1:length(paths)){
    # Parse the formula entries
    if(class(paths[[i]])=="character"){
      assign(names(paths)[i],
             paths[[i]][1],
             envir=aqueduct_env)
    }else if(class(paths[[i]])=="formula"){
      filepath = unlist(str_split(format(paths[[2]][[3]]), "/"))
      assign(as.character(paths[[2]][[2]]),
             paste0(get(filepath[1], envir=.aqueduct_env), "/",
                    paste0(filepath[2:length(filepath)], collapse="/")),
             envir=aqueduct_env)
    }else{
      warning(paste0(
        "Provided input (", format(paths[[2]]), ") is neither a parent path ",
        "character entry nor a child path formula entry."
      ))
    }
  }
}

# TODO: Make sure the code never destroys the .aqueduct folder and never
#       overwrites any of the aqueduct files
# TODO: Decide the format of the aqueduct files, then create them and read them
#       in during the setup phase
# TODO: create a aqueduct_destroy() function
