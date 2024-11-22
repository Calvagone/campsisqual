
#' Check the destination engine.
#' 
#' @param dest destination engine, character
#' 
checkDest <- function(dest) {
  if (!(dest %>% length() == 1 && dest %in% c("RxODE", "rxode2", "mrgsolve"))) {
    stop("Dest must be RxODE or mrgsolve")
  }
}

#' Qualify function.
#' 
#' @param x campsistrans object, if NULL, NONMEM files must exist
#' @param model CAMPSIS model
#' @param dataset CAMPSIS dataset
#' @param variables variables to compare
#' @param tolerance relative tolerance accepted when comparing 2 values
#' @param outputFolder output folder for qualification
#' @param reexecuteNONMEM force re-execute NONMEM if results already exist
#' @param compartments compartment indexes to output, numeric vector
#' @param dest destination engine to use
#' @param seed simulation/table export seed, default value is 1
#' @param settings simulation settings, please note NOCB is enabled by default
#' @return qualification summary
#' @importFrom campsismod export
#' @export
qualify <- function(x, model, dataset, variables, tolerance=1e-2, outputFolder,
                    reexecuteNONMEM=T, compartments=NULL, dest="rxode2", seed=1, settings=Settings(NOCB(TRUE))) {
  # Check destination engine
  checkDest(dest)
  
  # Dataset to table
  if (is(dataset, "dataset")) {
    settingsNM <- settings
    settingsNM@nocb@enable <- TRUE # NOCB always TRUE for NONMEM
    nmTable <- dataset %>% campsismod::export(dest="mrgsolve", model=model, seed=seed, settings=settingsNM)
  } else {
    nmTable <- dataset
  }

  return(qualify_delegate(x=x, model=model, engineTable=dataset, nmTable=nmTable,
                 variables=variables, tolerance=tolerance, compartments=compartments, outputFolder=outputFolder,
                 reexecuteNONMEM=reexecuteNONMEM, dest=dest, seed=seed, settings=settings))
}

#' Fix very annoying bug in current version of RxODE (> v1.1.0) or rxode2 (2.0.7).
#' Description of the bug:
#' If the model has a lag time and if the dataset does have an observation at time 0,
#' RxODE still outputs the time 0.
#' 
#' @param campsis CAMPSIS output
#' @param model CAMPSIS model
#' @param dataset engine table OR CAMPSIS dataset
#' @param dest destination engine
#' @return the corrected output if the bug was not present if RxODE
#' @importFrom dplyr filter
fixRxODEBug <- function(campsis, model, dataset, dest) {
  if (dest %in% c("RxODE", "rxode2")) {
    if (is(dataset, "dataset")) {
      times <- dataset %>% getTimes()
      
      # If LAG is found in model & time 0 does not exists in observations
      # We remove time 0 from the output
      properties <- model@compartments@properties
      if (!is.null(properties %>% find(LagTime(1))) && !(0 %in% times)) {
        campsis <- campsis %>% dplyr::filter(TIME!=0)
      }
    }
  }
  return(campsis)
}

#' Append original ID to simulation output if it exists in the dataset.
#' 
#' @param x CAMPSIS output
#' @param dataset CAMPSIS dataset or data frame
#' @param dataset engine table OR CAMPSIS dataset
#' @return updated output
#' @importFrom dplyr distinct left_join relocate select
appendOriginalId <- function(x, dataset) {
  if (is(dataset, "data.frame")) {
    if ("ORIGINAL_ID" %in% colnames(dataset)) {
      idPairs <- dataset %>% dplyr::select(ID, ORIGINAL_ID) %>% dplyr::distinct()
      x <- x %>% dplyr::left_join(idPairs, by="ID") %>% dplyr::relocate(ID, ORIGINAL_ID)
    }
  }
  return(x)
}

#' Qualify function.
#' 
#' @param x campsistrans object, if NULL, NONMEM files must exist
#' @param model CAMPSIS model
#' @param engineTable engine table OR CAMPSIS dataset
#' @param nmTable NONMEM dataset, data frame form
#' @param variables variables to compare
#' @param tolerance relative tolerance accepted when comparing 2 values
#' @param outputFolder output folder for qualification
#' @param reexecuteNONMEM force re-execute NONMEM if results already exist
#' @param compartments compartment indexes to output, numeric vector
#' @param dest destination engine to use
#' @param seed simulation/table export seed, default value is 1
#' @param settings simulation settings
#' @return qualification summary
#' @importFrom tibble add_column as_tibble
#' @importFrom campsis simulate
#' @export
qualify_delegate <- function(x, model, engineTable, nmTable, variables, tolerance, outputFolder,
                             reexecuteNONMEM=T, compartments=NULL, dest, seed, settings) {
  # Check destination engine
  checkDest(dest)
  
  # Create output folder if not created yet
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  # Prepare NONMEM qualification files if CAMPSIS trans object is provided
  # Otherwise, NONMEM files are supposed to be there
  if (!is.null(x)) {
    if (find.package("campsistrans", quiet=TRUE) %>% length() > 0) {
      eval(expr=parse(text="x %>% campsistrans::prepareNONMEMFiles(dataset=nmTable, variables=variables, compartments=compartments, outputFolder=outputFolder)"))
    } else {
      stop("campsistrans package is required to prepare NONMEM files")
    }
  }
  
  # Simulate with RxODE or mrgsolve
  campsis <- campsis::simulate(model, dataset=engineTable, dest=dest, seed=seed, settings=settings, outvars=variables)
  
  # Append ORIGINAL_ID
  campsis <- appendOriginalId(campsis, engineTable)
  
  # Fix RxODE bug
  campsis <- fixRxODEBug(campsis=campsis, model=model, dataset=engineTable, dest=dest)
  
  # Retrieve NONMEM results
  nonmem <- executeNONMEM(outputFolder=outputFolder, reexecuteNONMEM=reexecuteNONMEM)
  
  # Compare results
  summary <- compare(nonmem, campsis, variables=variables, tolerance=tolerance, dest=dest)
  
  return(summary)
}

#' Execute NONMEM. PsN will be called automatically by R. 
#' Prepared control stream 'model.mod' is executed automatically and NONMEM results
#' are returned in the form of a data frame.
#' 
#' @param outputFolder output folder of prepared NONMEM files
#' @param reexecuteNONMEM force re-execute NONMEM if results already exist
#' @export
executeNONMEM <- function(outputFolder, reexecuteNONMEM=T) {
  tabFile <- paste0(outputFolder, "/", "output.tab")
  if (!file.exists(tabFile) || reexecuteNONMEM) {
    system("cmd.exe", input=paste0("cd ","\"", outputFolder, "\"", " & ", "execute ", "model.mod"))
    unlink(paste0(outputFolder, "/", "modelfit_dir1"), recursive=TRUE)
  }
  nonmem <- read.nonmem(tabFile)[[1]] %>% as.data.frame()
  return(nonmem)
}

