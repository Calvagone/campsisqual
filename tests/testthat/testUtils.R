
# setwd("C:/prj/campsisqual/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsisqual/tests/")
# testFolder <<- "C:/prj/campsisqual/tests/testthat/"
# reticulate::use_python("C:/PsN-5.0.0/python/python-3.7.7.amd64/python.exe", required=TRUE)
# reticulate::py_config()
# version <- pharmpy["__version__"]

testFolder <- ""
testEngines <- c("mrgsolve", "rxode2")
reexecuteNONMEM <- FALSE

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
qualifyModel <- function(ctlPath, modelName, dataset, modelfun=NULL, dest, variables,
                         seed=1, settings=Settings(NOCB(TRUE)), updateDataset=FALSE, tolerance=1e-2,
                         skipNM=TRUE) {
  if (!isQualificationSuiteProvided()) {
    stop("No qualification suite provided")
  }
  option <- getCampsisqualOption()
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
    index <- model %>% getCompartmentIndex("CENTRAL")
    dataset <- dataset %>% setDefaultObsCmt(index)
  }
  
  # Start qualification
  qual <- qualify(
    x = object,
    model=model,
    dataset=dataset,
    variables=variables,
    tolerance=tolerance,
    outputFolder=nonmemModelFolder,
    reexecuteNONMEM=reexecuteNONMEM,
    dest=dest,
    seed=seed,
    settings=settings)
  
  # Export to PDF (only when NOCB is true to not duplicate PDF's)
  # if (nocb) {
  #   qual %>% write(file=paste0(resultsFolder, "qualification_", modelName, "_", dest, ".pdf"), summary=FALSE, log=FALSE)
  # }
  
  return(qual)
}

getTestName <- function(name) {
  return(paste0(name, " (", paste0(testEngines, collapse="/"), ")"))
}

isQualificationSuiteProvided <- function() {
  option <- getCampsisqualOption()
  if (is.null(option)) {
    return(FALSE)
  } else {
    path <- option$QUALIFICATION_SUITE
    return(dir.exists(path))
  }
}

getCampsisqualOption <- function() {
  return(getOption("campsisqual.options"))
} 
