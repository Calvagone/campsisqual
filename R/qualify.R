
#' Check the destination engine.
#' 
#' @param dest destination engine, character
#' 
checkDest <- function(dest) {
  if (!(dest %>% length() == 1 && dest %in% c("rxode2", "mrgsolve"))) {
    stop("Dest must be rxode2 or mrgsolve")
  }
}

#' Qualify function.
#' 
#' @param model Campsis model to be qualified
#' @param dataset Campsis dataset or data frame to be qualified
#' @param ipred individual predictions to be compared with (=reference results)
#' @param variables variables to compare
#' @param tolerance relative tolerance accepted when comparing 2 values
#' @param dest destination engine to use
#' @param seed simulation/table export seed, default value is 1
#' @param settings simulation settings, please note NOCB is enabled by default
#' @param idref ID reference, "dataset" or "ipred" (other IDs will be ignored)
#' @return qualification summary
#' @importFrom dplyr arrange filter
#' @importFrom campsismod export
#' @export
qualify <- function(model, dataset, ipred, variables, tolerance=1e-2,
                    dest="rxode2", seed=1, settings=Settings(NOCB(TRUE)), idref="ipred") {
  # Check destination engine
  checkDest(dest)
  
  # Dataset to table
  if (is(dataset, "dataset")) {
    settingsNM <- settings
    settingsNM@nocb@enable <- TRUE # NOCB always TRUE for NONMEM
    table <- dataset %>% campsismod::export(dest="mrgsolve", model=model, seed=seed, settings=settingsNM)
  } else {
    table <- dataset
  }
  
  # Ignore IDs mechanism
  datasetIds <- table$ID %>% unique()
  ipredIds <- ipred$ID %>% unique()
  if (idref == "dataset") {
    refIds <- datasetIds
  } else if (idref == "ipred") {
    refIds <- ipredIds
  } else {
    stop("idref must be 'dataset' or 'ipred'")
  }
  table <- table %>%
    dplyr::filter(ID %in% refIds) %>%
    dplyr::arrange(ID, TIME) %>%
    addSimulationIDColumn()
  ipred <- ipred %>%
    dplyr::filter(ID %in% refIds) %>%
    dplyr::arrange(ID, TIME) %>%
    addSimulationIDColumn()

  # Check destination engine
  checkDest(dest)
  
  # Simulate with rxode2 or mrgsolve
  campsis <- campsis::simulate(model, dataset=dataset, dest=dest, seed=seed, settings=settings, outvars=variables)
  
  # Append ORIGINAL_ID
  campsis <- appendOriginalId(campsis, table)
  
  # Fix rxode2 bug
  campsis <- fixRxODEBug(campsis=campsis, model=model, dataset=dataset, dest=dest)
  
  # Compare results
  summary <- compare(ipred, campsis, variables=variables, tolerance=tolerance, dest=dest)
  
  # Show if qualification passed or failed
  cat(ifelse(summary %>% passed(), "QUALIFICATION SUCCESSFUL", "QUALIFICATION FAILED"))
  
  return(summary)
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

#' Add simulation ID column (id's starting at 1 and consecutive). Original column ID
#' will be replaced by the new simulation ID column, after being renamed into
#' ORIGINAL_ID column.
#'
#' @param dataset NONMEM dataset
#' @param id current identifier column, default is 'ID'
#' @return updated data frame
#' @importFrom dplyr arrange group_by group_indices rename_at select
addSimulationIDColumn <- function(dataset, id="ID") {
  if ("ID" %in% colnames(dataset) && id != "ID") {
    dataset <- dataset %>% dplyr::select(-ID)
  }
  # Current ID is renamed into ORIGINAL_ID
  if (!("ORIGINAL_ID" %in% colnames(dataset))) {
    dataset <- dataset %>%
      dplyr::rename_at(.vars=id, .funs=function(x){"ORIGINAL_ID"})
  }
  # Arrange rows by ORIGINAL_ID
  dataset <- dataset %>%
    dplyr::arrange(ORIGINAL_ID)
  # Add simulation ID column
  dataset <- dataset %>%
    tibble::add_column(ID=dataset %>% dplyr::group_by(ORIGINAL_ID) %>%
                         dplyr::group_indices(), .before="ORIGINAL_ID")
  # Arrange rows by ID
  dataset <- dataset %>% dplyr::arrange(ID) 
  return(dataset)
}
