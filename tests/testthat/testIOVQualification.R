library(testthat)
library(ggplot2)

context("Qualification of models with inter-occasion variability (IOV) against NONMEM.")

testFolder <-  file.path(getwd(), test_path())
skipNONMEMPreparation <- TRUE

# Load utilities
source(file.path(testFolder, "testUtils.R"))

# Activate suite
activateSuite(enableSuite)

getTestDescription <- function(x) {
  if (isQualificationSuiteProvided()) {
    return(x)
  } else {
    return(sprintf("%s - NOT RUN", x))
  }
}

test_that(getTestName(getTestDescription("ADVAN4 TRANS4 model with IOV on ALAG1 works as expected")), {
  ctlPath <- file.path(testFolder, "resources", "advan4_trans4_iovalag1" , "model.mod")
  modelName <- "advan4_trans4_iovalag1"

  if (!isQualificationSuiteProvided()) {
    return(TRUE)
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Bolus(time=48, amount=1000, compartment=1)) %>%
    add(Bolus(time=72, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(72, 96, by=1), compartment=2)) %>%
    add(IOV(colname="IOV_ALAG1", NormalDistribution(0, 1)))

  # BUG in RxODE/rxode2 with alag when no observation 0: see method 'fixRxODEBug'
  for (engine in testEngines) {
    qualNocb <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", settings=Settings(NOCB(TRUE)), skipNM=skipNONMEMPreparation)
    qualLocf <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", settings=Settings(NOCB(FALSE)), skipNM=skipNONMEMPreparation)
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})

test_that(getTestName(getTestDescription("ADVAN4 TRANS4 model with IOV on F1 works as expected")), {
  ctlPath <- file.path(testFolder, "resources", "advan4_trans4_iovf1", "model.mod")
  modelName <- "advan4_trans4_iovf1"

  if (!isQualificationSuiteProvided()) {
    return(TRUE)
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Bolus(time=48, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(48, 72, by=1), compartment=2)) %>%
    add(IOV(colname="IOV_F1", NormalDistribution(0, 1)))

  for (engine in testEngines) {
    qualNocb <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", settings=Settings(NOCB(TRUE)), skipNM=skipNONMEMPreparation)
    qualLocf <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", settings=Settings(NOCB(FALSE)), skipNM=skipNONMEMPreparation)
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})

test_that(getTestName(getTestDescription("ADVAN4 TRANS4 model with IOV on KA works as expected")), {
  ctlPath <- file.path(testFolder, "resources", "advan4_trans4_iovka", "model.mod")
  modelName <- "advan4_trans4_iovka"

  if (!isQualificationSuiteProvided()) {
    return(TRUE)
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Bolus(time=48, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(48, 72, by=1), compartment=2)) %>%
    add(IOV(colname="IOV_KA", NormalDistribution(0, 1)))

  for (engine in testEngines) {
    qualNocb <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", settings=Settings(NOCB(TRUE, "IOV_KA")), skipNM=skipNONMEMPreparation)
    qualLocf <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", settings=Settings(NOCB(FALSE, "IOV_KA")), skipNM=skipNONMEMPreparation)
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})

test_that(getTestName(getTestDescription("ADVAN4 TRANS4 model with IOV on CL works as expected")), {
  ctlPath <- file.path(testFolder, "resources", "advan4_trans4_iovcl", "model.mod")
  modelName <- "advan4_trans4_iovcl"

  if (!isQualificationSuiteProvided()) {
    return(TRUE)
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Bolus(time=48, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(48, 72, by=1), compartment=2)) %>%
    add(IOV(colname="IOV_CL", NormalDistribution(0, 1)))

  for (engine in testEngines) {
    qualNocb <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", seed=2, settings=Settings(NOCB(TRUE, "IOV_CL")), skipNM=skipNONMEMPreparation)
    qualLocf <- qualifyModel(ctlPath=ctlPath, modelName=modelName, dataset=dataset, dest=engine,
                             variables="CP", seed=2, settings=Settings(NOCB(FALSE, "IOV_CL")), skipNM=skipNONMEMPreparation)
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})
