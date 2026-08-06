library(testthat)
library(ggplot2)
library(campsis)

context("Qualification of models with inter-occasion variability (IOV) against NONMEM.")

SKIP_NONMEM_PREPARATION <- TRUE

# Load utilities
source(file.path(getwd(), test_path(), "testUtils.R"))

# Activate suite
activate_suite(ENABLE_SUITE)

test_that("ADVAN4 TRANS4 model with IOV on ALAG1 works as expected", {
  ctlPath <- file.path(getwd(), test_path(), "resources", "advan4_trans4_iovalag1", "model.mod")
  modelName <- "advan4_trans4_iovalag1"

  if (!is_qualification_suite_provided()) {
    testthat::skip("Qualification suite not provided")
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 24, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 48, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 72, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(72, 96, by = 1), compartment = 2)) %>%
    add(IOV(colname = "IOV_ALAG1", NormalDistribution(0, 1)))

  # BUG in RxODE/rxode2 with alag when no observation 0: see method 'fix_rxode_bug'
  for (engine in TEST_ENGINES) {
    qualNocb <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      settings = Settings(NOCB(TRUE)),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    qualLocf <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      settings = Settings(NOCB(FALSE)),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})

test_that("ADVAN4 TRANS4 model with IOV on F1 works as expected", {
  ctlPath <- file.path(getwd(), test_path(), "resources", "advan4_trans4_iovf1", "model.mod")
  modelName <- "advan4_trans4_iovf1"

  if (!is_qualification_suite_provided()) {
    return(TRUE)
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 24, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 48, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(48, 72, by = 1), compartment = 2)) %>%
    add(IOV(colname = "IOV_F1", NormalDistribution(0, 1)))

  for (engine in TEST_ENGINES) {
    qualNocb <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      settings = Settings(NOCB(TRUE)),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    qualLocf <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      settings = Settings(NOCB(FALSE)),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})

test_that("ADVAN4 TRANS4 model with IOV on KA works as expected", {
  ctlPath <- file.path(getwd(), test_path(), "resources", "advan4_trans4_iovka", "model.mod")
  modelName <- "advan4_trans4_iovka"

  if (!is_qualification_suite_provided()) {
    testthat::skip("Qualification suite not provided")
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 24, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 48, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(48, 72, by = 1), compartment = 2)) %>%
    add(IOV(colname = "IOV_KA", NormalDistribution(0, 1)))

  for (engine in TEST_ENGINES) {
    qualNocb <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      settings = Settings(NOCB(TRUE, "IOV_KA")),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    qualLocf <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      settings = Settings(NOCB(FALSE, "IOV_KA")),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})

test_that("ADVAN4 TRANS4 model with IOV on CL works as expected", {
  ctlPath <- file.path(getwd(), test_path(), "resources", "advan4_trans4_iovcl", "model.mod")
  modelName <- "advan4_trans4_iovcl"

  if (!is_qualification_suite_provided()) {
    testthat::skip("Qualification suite not provided")
  }

  dataset <- Dataset(3) %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 24, amount = 1000, compartment = 1)) %>%
    add(Bolus(time = 48, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(48, 72, by = 1), compartment = 2)) %>%
    add(IOV(colname = "IOV_CL", NormalDistribution(0, 1)))

  for (engine in TEST_ENGINES) {
    qualNocb <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      seed = 2,
      settings = Settings(NOCB(TRUE, "IOV_CL")),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    qualLocf <- qualify_model(
      ctlPath = ctlPath,
      modelName = modelName,
      dataset = dataset,
      dest = engine,
      variables = "CP",
      seed = 2,
      settings = Settings(NOCB(FALSE, "IOV_CL")),
      skipNM = SKIP_NONMEM_PREPARATION
    )
    expect_true(qualNocb %>% passed())
    expect_true(qualLocf %>% passed())
  }
})
