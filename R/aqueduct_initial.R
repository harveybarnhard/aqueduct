#' Initial setup of an aqueduct workflow
#' 
#' Helper function for aqueduct_setup()
#' @param basepath
#'     Character string; directory in which you want the aqueduct files to be
#'     located, provided as an exact formula (see entry below).
#'     If \code{update=TRUE} then this parameter need not be provided.
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
#' @param projname
#'     Character string; \code{"aqueduct"} by default.
#'     The informative name you want to give to the aqueduct workflow
#'     project. The choice of this parameter has little practical implications
#'     other than to make statements printed from aqueduct() more informative
#' 
#' 
aqueduct_initial = function(basepath, ..., projname){
  # Create folder if it does not already exist
  dir.create(file.path(basepath, ".aqueduct"), showWarnings=FALSE)
  
  # If aqueduct cache files exist then load them. If not, then create them.
  # TODO error handle when only some of the files exist
  filename = file.path(basepath, ".aqueduct", "aqueduct_env.rds")
  if(!file.exists(filename)){
    # Create new aqueduct environment
    .aqueduct_env <<- new.env()
    assign("basepath", basepath, envir=.aqueduct_env)
    assign("aqueduct", file.path(basepath, ".aqueduct"), envir=.aqueduct_env)
    saveRDS(.aqueduct_env, filename)
    # Create new aqueduct timesheet
    timesheet = data.frame(
      node = NA,
      last_timestamp
    )
  }else{
    .aqueduct_env <<- readRDS(filename)
  }
  
  paths = list(...)
  if(length(paths)>0){
    # Loop over each input formula
    for(i in 1:length(paths)){
      # Parse the formula entries
      if(class(paths[[i]])=="character"){
        assign(names(paths)[i],
               paths[[i]][1],
               envir=.aqueduct_env)
      }else if(class(paths[[i]])=="formula"){
        filepath = unlist(str_split(format(paths[[i]][[3]]), "/"))
        assign(as.character(paths[[i]][[2]]),
               paste0(get(filepath[1], envir=.aqueduct_env), "/",
                      paste0(filepath[2:length(filepath)], collapse="/")),
               envir=.aqueduct_env)
      }else{
        warning(paste0(
          "Provided input (", format(paths[[2]]), ") is neither an exact ",
          "path given as a character string nor a relative path given as a ",
          "formula of the form w ~ x/y/z."
        ))
      }
    }
  }
}
