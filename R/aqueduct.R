#' Run an aqueduct Workflow
#' 
#' This function runs the workflow provided in the arguments, ignoring 
#' components of the workflow (nodes) whose parent nodes have not changed
#' 
#' @param ...
#'    Formulas, separated by commas, that dictate the workflow of the project.
#'    Each formula is written as
#'    \deqn{outdir(out) ~ node(indir1(in1) + indir2(in2) + ..., options)}
#'    where the use of the components of the formula are described below.
#' @param verbose
#'    \code{FALSE} by default. Returns all output of code while running.
#' @return
#'    \item{workflow}{A dataframe of all the workflow nodes with the previous
#'                    timestamp before running, and the timestamp after running}
#'    \item{plot}{A Plot displaying a directed acyclic graph (DAG) of the 
#'                workflow}
#' @details
#' ## Formula Arguments
#' An individual formula is made up of the following components
#' * \code{node} The name of the .R file as it exists in the code folder as
#'   defined in \code{\link{aqueduct_setup}()}. Do not include the .R file
#'   extension.
#' * \code{indir1, indir2} The names of the directories for the first and
#'   second input file where the names are defined in \code{\link{aqueduct_setup}()}
#' * \code{in1, in2} The names of the first and second input files, without
#'   extensions, that are located in \code{indir1} and \code{indir2},
#'   respectively. Most of the time, the files will be in .csv format, but
#'   \code{aqueduct()} will also read in .xlsx and .dta file formats without a
#'   need to specify the extension. If the input files are not found in the
#'   the input directories, then the files are looked for in the sub directories
#'   of the input directories.
#' * \code{outdir} The name of the directory for the output file.
#' * \code{out} The name of the output file created by \code{node}.
#'   \code{aqueduct()} is smart in that it determines the file format of the
#'   output and saves it as the appropriate file format. For the most part, the
#'   file output will be a single .csv file. When the main function in 
#'   \code{node} outputs a list of dataframe style objects, a .csv file is saved
#'   in \code{outdir} and the files are saved with the file name 
#'   \[name_in_list\].csv where name_in_list is the name given for that object
#'   in the list. When this is the case, specifying \code{outdir()}
#'   is sufficient.
#'   If the file is not a dataframe object, matrix, or vector, then
#'   the object will be saved as a .RData object.
#' ## Formula Options
#' The options section of the formula contains additional arguments to pass
#' on to the \code{node} file.
#' 
#' @examples
#' # Let's say we are working for a government agency that provides affordable
#' # housing to low-income individuals. We want to determine if there are
#' # subgroups of the population that disproportionately drop out of the
#' # program despite being eligible. The workflow is: 
#' #    1. Clean and merge characteristic files of individuals eligible 
#' #       for program using a file called load_chars.R
#' #    2. Merge characteristic files onto the main database file that contains
#' #       information on whether or not eligible individuals participated in
#' #       the program, and if so, how long they participated.
#' #    3. Run a SVM to classify groups that are disproportionately likely to
#' #       drop out of the program
#' #    4. Produce plots based on these results and the underlying
#' #       characteristics of the population
#' #    5. Produce a knitted document displaying these results
#' # And the filepath is as follows, starting from the basepath:
#' #    /finding_groups
#' #    ----/code
#' #    --------/clean
#' #    ------------/load_chars.R
#' #    ------------/clean_chars.R
#' #    --------/build
#' #    ------------/add_chars.R
#' #    --------/analyze
#' #    ------------/svm_classify.R
#' #    ------------/create_plots.R
#' #    ------------/produce_report.Rmd
#' #    ----/data
#' #    --------/raw
#' #    ------------/main_db.csv
#' #    ------------/chars
#' #    ----------------/location_file.csv
#' #    ----------------/race_file.csv
#' #    ----------------/age_file.csv
#' #    ----------------/education_file.csv
#' #    --------/derived
#' #    --------/current
#' #    ----/output
#' #    --------/plots
#' 
#' # First, set paths using aqueduct_setup()
#' aqueduct_setup(
#'   basepath = "C:/Users/Harvey/GitHub/aqueduct/examples/example1",
#'   raw      ~ basepath/data/raw,
#'   derived  ~ basepath/data/derived,
#'   current  ~ basepath/data/current,
#'   plots    ~ basepath/output/plots
#' )
#' # Then run the aqueduct workflow!
#' aqueduct(
#'   raw() ~ create_data(,seed=1996)
#'   derived(chars) ~ load_chars(raw(location_file) +
#'                               raw(race_file) +
#'                               raw(age_file) +
#'                               raw(education_file)),
#'   derived(clean_chars) ~ clean_chars(derived(chars)),
#'   derived(db_w_chars) ~ add_chars(raw(main_db) + derived(clean_chars)),
#'   current(classified_groups) ~ svm_classify(derived(db_w_chars)),
#'   plots(classify_plots) ~ create_plots(current(classified_groups) +
#'                                        derived(clean_chars)),
#'   output(final_report)  ~ produce_report() 
#' )

aqueduct = function(..., verbose=FALSE){
  formulae = list(derived(y)~run_func(raw(create_x)+raw(create_z)))
  for(i in 1:length(formulae)){
    # Separate the left and right hand side of the formula
    lhs = format(formulae[[i]][[2]])
    rhs = format(formulae[[i]][[3]])
    # Parse the left hand side
    outdir  = get(sub("^(.*?)\\(.+", "\\1", lhs),
                  envir=.aqueduct_env)
    outfile = regmatches(lhs, gregexpr("(?<=\\().*?(?=\\))", lhs, perl=T))[[1]]
      
    # Parse the right hand side
    codefile = sub("^(.*?)\\(.+", "\\1", rhs)
    infiles  = sub(paste0("^",codefile,"\\((.*)\\)"), "\\1", rhs)
    infiles  = strsplit(infiles, split="\\+")[[1]]
    infiles  = sub("\\s", "", infiles)
    inpaths  = unlist(mget(gsub("^(.*?)\\(.+", "\\1", infiles),
                           envir=.aqueduct_env))
    infiles  = unlist(regmatches(infiles, gregexpr("(?<=\\().*?(?=\\))",
                                                   infiles,
                                                   perl=T)))
    # Make sure files exist, then pull their timestamps
    if(length(infiles)!=length(inpaths)){
      stop("Right hand side of formula ", i, " contains an error.")
    }
    timesheet = local(timesheet, envir=.aqueduct_env)
    for(j in 1:length(infiles)){
      path = file.path(inpaths[j], paste0(infiles[j], ".csv"))
      if(!file.exists(path)){
        stop(paste0("File ", infiles[j], " in formula ", i, " not found in ",
                    "directory ", inpaths[j]))
      }
      if(path%in%timesheet$path){
        timesheet[timesheet$path==path,]$curr_timestamp = file.mtime(path)
      }else{
        new_row = nrow(timesheet)+1
        timesheet[new_row,]$node_type = "data"
        timesheet[new_row,]$parent_dir = names(inpaths)[j]
        timesheet[new_row,]$name  = infiles[j]
        timesheet[new_row,]$last_timestamp = as.POSIXct(NA)
        timesheet[new_row,]$curr_timestamp = as.POSIXct(file.mtime(path))
        timesheet[new_row,]$path = path
        new_entry = c(path,
                      "data",
                      infiles[j],
                      as.POSIXct(NA),
                      as.POSIXct(file.mtime(path)))
        names(new_entry) = colnames(timesheet)
        timesheet = rbind(timesheet, new_entry)
      }
    }
    # Search for code file in basepath
    # Run the code
    if(any(flag)==FALSE){
      objects_in_memory=ls()
      # source(codepath, verbose=verbose)
      rm(list=setdiff(ls(), objects_in_memory))
      gc()
    }
  }
  return(arguments)
}

#TODO Finish making timesheet and see how it works
#TODO Create example files to make sure they work
#TODO Strava metro?
