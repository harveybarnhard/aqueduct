#' Set up aqueduct workflow
#' 
#' This function establishes filepaths that are to be used in an
#' aqueduct workflow by defining the filepaths and their names. Shorter names
#' make more readable workflows. These filepaths are stored in a hidden
#' aqueduct environment to avoid clutter (an environment that is established
#' in this folder)
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
#' @examples 
#' 
#' 
aqueduct_destroy = function(basepath="",
                          ...,
                          projname="aqueduct",
                          update=FALSE,
                          archive=FALSE){
  # Create a hidden environment for aqueduct to work in without cluttering
  # up the local environment. Create the aqueduct files if they do not already
  # exist
  if(update==FALSE){
    # Catch any input errors
    if(basepath!=""){
      dir.create(file.path(basepath, ".aqueduct"), showWarnings=FALSE)
      if(file.exists(file.path(basepath, "/.aqueduct/aqueduct_cache.rds"))){
        if(archive==TRUE){
          # aqueduct_archive()
        }else if(archive==FALSE){
          userinput = readline(paste0("update=FALSE and basepath provided, ",
                                      "but there already exists an aqueduct ",
                                      "cache in ", basepath,".", 
                                      "Do you wish to archive the previous ",
                                      "cache and start a new aqueduct cache? ",
                                      "[yes/no]"))
          if(userinput=="yes"){
            stop("Try again with archive=TRUE")
          }else if(userinput=="no"){
            userinput = readline(paste0("Do you wish to update the previous ",
                                        "cache instead of creating a new cache? ",
                                        "[yes/no]"))
            if(userinput=="yes"){
              stop("Try again using update=TRUE.")
            }else if(userinput=="no"){
              stop("If you wish to erase the previous cache, use ",
                   "aqueduct_destroy() instead. Then try using aqueduct_setup() ",
                   "again.")
            }else if(!userinput%in%c(c("yes", "no"))){
              stop("Response other than 'yes' or 'no' provided. Try again.")
            }
          }else if(!userinput%in%c(c("yes", "no"))){
            stop("Response other than 'yes' or 'no' provided. Try again.")
          }
        }
      }else if(!file.exists(file.path(basepath, "/.aqueduct/aqueduct_cache.rds"))){
        # aqueduct_initial()
      }
    }else if(basepath==""){
      stop("update=FALSE but no basepath provided")
    }
    # .aqueduct_env = new.env()
    #   assign("basepath", basepath, envir=.aqueduct_env)
    #   assign("aqueduct", file.path(basepath, ".aqueduct"))
  }else if(update==TRUE){
    if(!exists(".aqueduct_env")){
      stop(paste0("update=TRUE but an initial aqueduct_setup() ",
                  "step has not been made."))
    }
  }
  
  
  paths = list(...)
  if(length(paths)==0){
    if(basepath!=""){
      print("Aqueduct setup successful!")
    }else if(basepath==""){
      print()
    }
  }else if(length(paths)>0){
    # Loop over each input formula
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
}

# TODO: Make sure the code never destroys the .aqueduct folder and never
#       overwrites any of the aqueduct files
# TODO: Decide the format of the aqueduct files, then create them and read them
#       in during the setup phase
# TODO: create an aqueduct_archive() function
# TODO: create an aqueduct_destroy() function
# TODO: create an aqueduct_update() function
# TODO: create aqueduct_initial() function
