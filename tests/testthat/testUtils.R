TEST_ENGINES <- c("mrgsolve", "rxode2")
REEXECUTE_NONMEM <- FALSE
ENABLE_SUITE <- TRUE

#'
#' Qualify model.
#' 
#' @param ctlPath path to the NONMEM control file
#' @param modelName name of the model
#' @param dataset dataset
#' @param modelfun function to apply on the model
#' @param dest destination engine
#' @param variables variables to qualify
#' @param seed seed
#' @param settings Campsis settings
#' @param updateDataset update default observation compartment in Campsis dataset
#' @param tolerance tolerance to apply on the qualification against NONMEM
#' @param name description
#' @param skipNM skip NONMEM model preparation
#' @return qualification object
#'
qualify_model <- function(ctlPath, modelName, dataset, modelfun=NULL, dest, variables,
                         seed=1, settings=Settings(NOCB(TRUE)), updateDataset=FALSE, tolerance=1e-2,
                         skipNM=TRUE) {
  if (!is_qualification_suite_provided()) {
    stop("No qualification suite provided")
  }
  option <- get_campsisqual_option()
  qualSuite <- option$QUALIFICATION_SUITE
  
  modelSuiteFolder <- file.path(qualSuite, "model_suite")
  modelFolder <- file.path(modelSuiteFolder, modelName)
  campsisModelFolder <- file.path(modelFolder, "campsis")
  nonmemModelFolder <- file.path(modelFolder, "nonmem")
  
  resultsFolder <- file.path(qualSuite, "qualification_results")
  nocb <- settings@nocb@enable
  
  # dir.create(file.path(modelFolder), showWarnings=FALSE)
  # dir.create(file.path(resultsFolder), showWarnings=FALSE)
  
  if (skipNM) {
    # CAMPSIS trans object is NULL
    object <- NULL
    
    # We load the persisted model
    model <- read.campsis(campsisModelFolder) 
    
  } else {
    # NONMEM import
    object <- campsistrans::importNONMEM(ctlPath)
    
    # Conversion to CAMPSIS
    model <- object %>% export(dest="campsis")
    
    # Apply function on model
    if (!is.null(modelfun)) {
      model <- model %>% modelfun() 
    }
    
    # Export CAMPSIS model
    model %>% write(campsisModelFolder)
  }
  
  # Update dataset (compartment index of observations is adapted automatically)
  if (updateDataset) {
    index <- model %>% get_compartment_index("CENTRAL")
    dataset <- dataset %>% set_default_obs_cmt(index)
  }
  
  # Read individual predictions
  ipred <- read.nonmem(file.path(nonmemModelFolder, "output.tab"))[[1]]
  
  # Start qualification
  qual <- qualify(
    model=model,
    dataset=dataset,
    ipred=ipred,
    variables=variables,
    tolerance=tolerance,
    dest=dest,
    seed=seed,
    settings=settings)
  
  # Export to PDF (only when NOCB is true to not duplicate PDF's)
  # if (nocb) {
  #   qual %>% write(file=paste0(resultsFolder, "qualification_", modelName, "_", dest, ".pdf"), summary=FALSE, log=FALSE)
  # }
  
  return(qual)
}

get_test_name <- function(name) {
  return(paste0(name, " (", paste0(TEST_ENGINES, collapse="/"), ")"))
}

is_qualification_suite_provided <- function() {
  option <- get_campsisqual_option()
  if (is.null(option)) {
    return(FALSE)
  } else {
    path <- option$QUALIFICATION_SUITE
    return(dir.exists(path))
  }
}

get_campsisqual_option <- function() {
  return(getOption("campsisqual.options"))
} 

activate_suite <- function(enable) {
  if (enable) {
    basePath <- "<PATH_TO_SUITE>"
    qualOptions <- list()
    qualOptions$QUALIFICATION_SUITE=file.path(basePath, "qualification_suite")
    qualOptions$QUALIFICATION_SUITE_N_MODELS=97
    options(campsisqual.options=qualOptions)
  }
}
