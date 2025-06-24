library(testthat)
library(ggplot2)

context("Qualification of the NONMEM subroutines, implemented in Campsis, against NONMEM")

testFolder <-  file.path(getwd(), test_path())
skipNONMEMPreparation <- TRUE

# Load utilities
source(file.path(testFolder, "testUtils.R"))

# Activate suite
activateSuite(enableSuite)

advanFilename <- function(advan, trans, ext=".mod") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

createDataset <- function() {
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1, ii=12, addl=2))
  obsTimes <- seq(1, 36, by=1)
  obsTimes <- obsTimes[!(obsTimes %in% c(0,12,24))] # Remove obs at administrations (for RxODE...)
  dataset <- dataset %>% add(Observations(times=obsTimes, compartment=1)) # Compartment number will be adapted
  return(dataset)
}

getCtlPath <- function(advan, trans, skipNM) {
  if (skipNM) {
    return(NULL)
  } else {
    return(campsistrans::getNONMEMModelTemplate(advan, trans))
  }
}

qualifySubroutine <- function(advan, trans) {
  if (!isQualificationSuiteProvided()) {
    return(TRUE)
  }
  option <- getCampsisqualOption()
  qualSuite <- option$QUALIFICATION_SUITE
  modelName <- advanFilename(advan, trans, ext="")
  for (engine in testEngines) {
    qual <- qualifyModel(ctlPath=getCtlPath(advan, trans, skipNM=skipNONMEMPreparation), modelName=modelName,
                         dataset=createDataset(), dest=engine, variables="CP", updateDataset=TRUE,
                         skipNM=skipNONMEMPreparation)
    expect_true(qual %>% passed())
  }
}

getTestDescription <- function(routine) {
  if (isQualificationSuiteProvided()) {
    return(sprintf("%s model works as expected", routine))
  } else {
    return(sprintf("%s model works as expected - NOT RUN", routine))
  }
}

test_that(getTestName(getTestDescription("ADVAN1 TRANS1")), {
  advan <- 1
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN1 TRANS2")), {
  advan <- 1
  trans <- 2
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN2 TRANS1")), {
  advan <- 2
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN2 TRANS2")), {
  advan <- 2
  trans <- 2
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN3 TRANS1")), {
  advan <- 3
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN3 TRANS3")), {
  advan <- 3
  trans <- 3
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN3 TRANS4")), {
  advan <- 3
  trans <- 4
  return(qualifySubroutine(advan, trans))
})

# Disabled because TRANS5 subroutine models are not properly translated by Pharmpy v0.46
# test_that(getTestName(getTestDescription("ADVAN3 TRANS5")), {
#   # advan <- 3
#   # trans <- 5
# })

test_that(getTestName(getTestDescription("ADVAN4 TRANS1")), {
  advan <- 4
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN4 TRANS3")), {
  advan <- 4
  trans <- 3
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN4 TRANS4")), {
  advan <- 4
  trans <- 4
  return(qualifySubroutine(advan, trans))
})

# Disabled because TRANS5 subroutine models are not properly translated by Pharmpy v0.46
# test_that(getTestName(getTestDescription("ADVAN4 TRANS5")), {
#   # advan <- 4
#   # trans <- 5
# })

test_that(getTestName(getTestDescription("ADVAN11 TRANS4")), {
  advan <- 11
  trans <- 4
  return(qualifySubroutine(advan, trans))
})

test_that(getTestName(getTestDescription("ADVAN12 TRANS4")), {
  advan <- 12
  trans <- 4
  return(qualifySubroutine(advan, trans))
})


