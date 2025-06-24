library(testthat)
library(ggplot2)

context("Qualification of the Campsis model suite against NONMEM")

testFolder <-  file.path(getwd(), test_path())

# Load utilities
source(file.path(testFolder, "testUtils.R"))

# Activate suite
activateSuite(enableSuite)

# Load the model suite generation script
source(file.path(testFolder, "generate_model_suite.R"))

#'
#' Use this method to regenerate the list of models to test (calvamod required).
#' 
#' @return character vector with all model names
#' 
getModelSuiteNames <- function() {
  pks <- generateModelSuite() %>%
    discard3CptZoModels()
  
  names <- pks@list %>% purrr::map(.f=function(pk) {
    return(pk %>% getShortName())
  }) %>% purrr::flatten_chr()
  
  all <- paste0("\"", names, "\"",  collapse=", ")
  cat(all)
  return(all)
}

# These models cannot be exported to NONMEM
# See https://github.com/Calvagone/calvamod/issues/84
discard3CptZoModels <- function(pks) {
  pks@list <- pks@list %>% purrr::discard(~grepl(pattern="3cpt_zo", x=.x %>% getShortName()))
  return(pks)
}

#'
#' Generate the Campsis dataset based on the model short name (e.g. '1cpt_fo').
#' Before: PK models from calvamod were generated on the fly in campsisqual and
#' the dataset was exported automatically (based on the input dataset configured
#' in 'generate_model_suite.R')
#' Now: the dataset is generated based on the model short name.
#' 
#' @param shortName model short name
#' @return Campsis dataset
#' 
generateDatasetBasedOnModelName <- function(shortName) {
  isTmdd <- grepl(pattern="tmdd", x=shortName)
  isInfusion <- grepl(pattern="_zo", x=shortName)
  
  if (isTmdd) {
    dataset <- getTmddReferenceDataset(infusion=isInfusion)
  } else {
    dataset <- getPKReferenceDataset(infusion=isInfusion)
  }
  dataset <- dataset %>%
    add(DatasetConfig(exportTDOS=TRUE)) # For Weibull models to work
  return(dataset)
}

getAllModelNames <- function() {
  pks <- c("1cpt_fo", "1cpt_fo_lag", "1cpt_fo_transit", "1cpt_weibull_original", "1cpt_weibull", "1cpt_weibull_double", "1cpt_mmabs", "1cpt_zo", "1cpt_fo_mm", "1cpt_fo_lag_mm", "1cpt_fo_transit_mm", "1cpt_weibull_original_mm", "1cpt_weibull_mm", "1cpt_weibull_double_mm", "1cpt_mmabs_mm", "1cpt_zo_mm", "1cpt_fo_mixed", "1cpt_fo_lag_mixed", "1cpt_fo_transit_mixed", "1cpt_weibull_original_mixed", "1cpt_weibull_mixed", "1cpt_weibull_double_mixed", "1cpt_mmabs_mixed", "1cpt_zo_mixed", "2cpt_fo", "2cpt_fo_lag", "2cpt_fo_transit", "2cpt_weibull_original", "2cpt_weibull", "2cpt_weibull_double", "2cpt_mmabs", "2cpt_zo", "2cpt_fo_mm", "2cpt_fo_lag_mm", "2cpt_fo_transit_mm", "2cpt_weibull_original_mm", "2cpt_weibull_mm", "2cpt_weibull_double_mm", "2cpt_mmabs_mm", "2cpt_zo_mm", "2cpt_fo_mixed", "2cpt_fo_lag_mixed", "2cpt_fo_transit_mixed", "2cpt_weibull_original_mixed", "2cpt_weibull_mixed", "2cpt_weibull_double_mixed", "2cpt_mmabs_mixed", "2cpt_zo_mixed", "3cpt_fo", "3cpt_fo_lag", "3cpt_fo_transit", "3cpt_weibull_original", "3cpt_weibull", "3cpt_weibull_double", "3cpt_mmabs", "3cpt_fo_mm", "3cpt_fo_lag_mm", "3cpt_fo_transit_mm", "3cpt_weibull_original_mm", "3cpt_weibull_mm", "3cpt_weibull_double_mm", "3cpt_mmabs_mm", "3cpt_fo_mixed", "3cpt_fo_lag_mixed", "3cpt_fo_transit_mixed", "3cpt_weibull_original_mixed", "3cpt_weibull_mixed", "3cpt_weibull_double_mixed", "3cpt_mmabs_mixed", "1cpt_fo_tmdd_full", "1cpt_zo_tmdd_full", "2cpt_fo_tmdd_full", "2cpt_zo_tmdd_full", "1cpt_fo_tmdd_full_cst_rtot", "1cpt_zo_tmdd_full_cst_rtot", "2cpt_fo_tmdd_full_cst_rtot", "2cpt_zo_tmdd_full_cst_rtot", "1cpt_fo_tmdd_full_ib", "1cpt_zo_tmdd_full_ib", "2cpt_fo_tmdd_full_ib", "2cpt_zo_tmdd_full_ib", "1cpt_fo_tmdd_full_cst_rtot_ib", "1cpt_zo_tmdd_full_cst_rtot_ib", "2cpt_fo_tmdd_full_cst_rtot_ib", "2cpt_zo_tmdd_full_cst_rtot_ib", "1cpt_fo_tmdd_qe", "1cpt_zo_tmdd_qe", "2cpt_fo_tmdd_qe", "2cpt_zo_tmdd_qe", "1cpt_fo_tmdd_qss", "1cpt_zo_tmdd_qss", "2cpt_fo_tmdd_qss", "2cpt_zo_tmdd_qss", "1cpt_fo_tmdd_wagner", "1cpt_zo_tmdd_wagner", "2cpt_fo_tmdd_wagner", "2cpt_zo_tmdd_wagner")
  return(pks)
}

generateModelIndexes <- function(pks) {
  option <- getCampsisqualOption()
  nModels <- option$QUALIFICATION_SUITE_N_MODELS
  randomPickUp <- TRUE
  
  if (nModels > length(pks)) {
    nModels <- length(pks)
  }
  
  nTotal <- length(pks)
  set.seed(1)
  
  if (randomPickUp) {
    modelIndexes <- base::sample(seq_len(nTotal), size=nModels, replace=FALSE)
  } else {
    modelIndexes <- seq_len(nTotal)
  }
  return(modelIndexes)
}

if (!isQualificationSuiteProvided()) {
  return(TRUE)
}

pks <- getAllModelNames()
modelIndexes <- generateModelIndexes(pks)

qualifyModelSuiteModel <- function(shortName) {
  dataset <- generateDatasetBasedOnModelName(shortName)
  for (engine in testEngines) {
    qual <- qualifyModel(ctlPath=NULL, modelName=shortName, dataset=dataset,
                         variables="CONC", dest=engine, skipNM=TRUE)
    expect_true(qual %>% passed())
  }
}

for (index in modelIndexes) {
  shortName <- pks[index]
  test_that(getTestName(sprintf("Qualification of model `%s' against NONMEM is successful", shortName)), {qualifyModelSuiteModel(shortName=shortName)})
}
