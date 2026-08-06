library(testthat)
library(ggplot2)
library(campsis)

context("Qualification of the NONMEM subroutines, implemented in Campsis, against NONMEM")

test_folder <- file.path(getwd(), test_path())
SKIP_NONMEM_PREPARATION <- TRUE

# Load utilities
source(file.path(test_folder, "testUtils.R"))

# Activate suite
activate_suite(ENABLE_SUITE)

advan_filename <- function(advan, trans, ext = ".mod") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

create_dataset <- function() {
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time = 0, amount = 1000, compartment = 1, ii = 12, addl = 2))
  obs_times <- seq(1, 36, by = 1)
  obs_times <- obs_times[!(obs_times %in% c(0, 12, 24))] # Remove obs at administrations (for RxODE...)
  dataset <- dataset %>%
    add(Observations(times = obs_times, compartment = 1)) # Compartment number will be adapted
  return(dataset)
}

get_ctl_path <- function(advan, trans, skipNM) {
  if (skipNM) {
    return(NULL)
  } else {
    return(campsistrans::getNONMEMModelTemplate(advan, trans))
  }
}

qualify_subroutine <- function(advan, trans) {
  if (!is_qualification_suite_provided()) {
    testthat::skip("Qualification suite not provided")
  }
  results <- NULL
  option <- get_campsisqual_option()
  qualSuite <- option$QUALIFICATION_SUITE
  modelName <- advan_filename(advan, trans, ext = "")
  for (engine in TEST_ENGINES) {
    qual <- qualify_model(
      ctlPath = get_ctl_path(advan, trans, skipNM = SKIP_NONMEM_PREPARATION),
      modelName = modelName,
      dataset = create_dataset(),
      dest = engine,
      variables = "CP",
      updateDataset = TRUE,
      skipNM = SKIP_NONMEM_PREPARATION
    )
    results <- results %>%
      append(qual %>% passed())
  }
  return(results)
}

test_that("ADVAN1 TRANS1 model works as expected", {
  advan <- 1
  trans <- 1
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN1 TRANS2 model works as expected", {
  advan <- 1
  trans <- 2
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN2 TRANS1 model works as expected", {
  advan <- 2
  trans <- 1
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN2 TRANS2 model works as expected", {
  advan <- 2
  trans <- 2
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN3 TRANS1 model works as expected", {
  advan <- 3
  trans <- 1
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN3 TRANS3 model works as expected", {
  advan <- 3
  trans <- 3
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN3 TRANS4 model works as expected", {
  advan <- 3
  trans <- 4
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

# Disabled because TRANS5 subroutine models are not properly translated by Pharmpy v0.46
# test_that("ADVAN3 TRANS5")), {
#   # advan <- 3
#   # trans <- 5
# })

test_that("ADVAN4 TRANS1 model works as expected", {
  advan <- 4
  trans <- 1
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN4 TRANS3 model works as expected", {
  advan <- 4
  trans <- 3
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN4 TRANS4 model works as expected", {
  advan <- 4
  trans <- 4
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

# Disabled because TRANS5 subroutine models are not properly translated by Pharmpy v0.46
# test_that("ADVAN4 TRANS5")), {
#   # advan <- 4
#   # trans <- 5
# })

test_that("ADVAN11 TRANS4 model works as expected", {
  advan <- 11
  trans <- 4
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})

test_that("ADVAN12 TRANS4 model works as expected", {
  advan <- 12
  trans <- 4
  results <- qualify_subroutine(advan, trans)
  expect_true(length(results) > 0)
  expect_all_true(results)
})
