#' Set up aqueduct workflow
#' 
#' This function establishes filepaths that are to be used in an
#' aqueduct workflow by defining the filepaths and their names. Shorter names
#' make more readable workflows. These filepaths are stored in a hidden
#' aqueduct environment to avoid clutter (an environment that is established
#' in this folder)
#' 
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
#' @param update 
#'     logical; \code{FALSE} by default. If \code{FALSE}, then a new environment
#'     is created and any previous filepaths are erased. If \code{TRUE} then any
#'     filepaths given will be added to the list of previous filepaths
#' @param archive
#'     logical; \code{FALSE} by default. If \code{FALSE} then it is assumed that
#'     aqueduct_setup() is updating filepaths. If TRUE and a basepath is given,
#'     then any previous aqueduct caches are archived
     
aqueduct_setup = function(basepath="",
                          ...,
                          projname="aqueduct",
                          update=FALSE,
                          archive=FALSE){
  # Error Handing 
  paths = list(...)
  if(length(paths)==0 & basepath==""){
    stop("Paths must be provided.")
  }
  if(update==FALSE & basepath==""){
    stop("update=FALSE but basepath not provided.")
  }
  if(update==TRUE & archive==TRUE){
    stop("update and archive cannot be set to TRUE at the same time.")
  }
  if(basepath=="" & !exists(".aqueduct_env")){
    stop("No basepath provided, aqueduct_setup() does not know where to look ",
         "to load workflow.")
  }
  
  # Calling appropriate helper functions 
  if(update==FALSE & archive==FALSE & basepath!=""){
    aqueduct_initial(basepath=basepath, ..., projname=projname)
  }else if(update==FALSE & archive==TRUE & exists(".aqueduct_env")){
    # aqueduct_archive(...)
    print("done")
  }else if(update==TRUE & archive==FALSE & exists(".aqueduct_env")){
    # aqueduct_update(...)
    print("done")
  }
}

#TODO create aqueduct_archive()
#TODO create aqueduct_update()
