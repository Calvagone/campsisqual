library(testthat)
library(ggplot2)

context("Qualification of the NONMEM subroutines, implemented in Campsis, against NONMEM")

test_folder <-  file.path(getwd(), test_path())
SKIP_NONMEM_PREPARATION <- TRUE

# Load utilities
source(file.path(test_folder, "testUtils.R"))

# Activate suite
activate_suite(ENABLE_SUITE)

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

get_ctl_path <- function(advan, trans, skipNM) {
  if (skipNM) {
    return(NULL)
  } else {
    return(campsistrans::getNONMEMModelTemplate(advan, trans))
  }
}

qualifySubroutine <- function(advan, trans) {
  if (!is_qualification_suite_provided()) {
    return(TRUE)
  }
  option <- get_campsisqual_option()
  qualSuite <- option$QUALIFICATION_SUITE
  modelName <- advanFilename(advan, trans, ext="")
  for (engine in TEST_ENGINES) {
    qual <- qualify_model(ctlPath=get_ctl_path(advan, trans, skipNM=SKIP_NONMEM_PREPARATION), modelName=modelName,
                         dataset=createDataset(), dest=engine, variables="CP", updateDataset=TRUE,
                         skipNM=SKIP_NONMEM_PREPARATION)
    expect_true(qual %>% passed())
  }
}

get_test_description <- function(routine) {
  if (is_qualification_suite_provided()) {
    return(sprintf("%s model works as expected", routine))
  } else {
    return(sprintf("%s model works as expected - NOT RUN", routine))
  }
}

test_that(get_test_name(get_test_description("ADVAN1 TRANS1")), {
  advan <- 1
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN1 TRANS2")), {
  advan <- 1
  trans <- 2
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN2 TRANS1")), {
  advan <- 2
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN2 TRANS2")), {
  advan <- 2
  trans <- 2
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN3 TRANS1")), {
  advan <- 3
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN3 TRANS3")), {
  advan <- 3
  trans <- 3
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN3 TRANS4")), {
  advan <- 3
  trans <- 4
  return(qualifySubroutine(advan, trans))
})

# Disabled because TRANS5 subroutine models are not properly translated by Pharmpy v0.46
# test_that(get_test_name(get_test_description("ADVAN3 TRANS5")), {
#   # advan <- 3
#   # trans <- 5
# })

test_that(get_test_name(get_test_description("ADVAN4 TRANS1")), {
  advan <- 4
  trans <- 1
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN4 TRANS3")), {
  advan <- 4
  trans <- 3
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN4 TRANS4")), {
  advan <- 4
  trans <- 4
  return(qualifySubroutine(advan, trans))
})

# Disabled because TRANS5 subroutine models are not properly translated by Pharmpy v0.46
# test_that(get_test_name(get_test_description("ADVAN4 TRANS5")), {
#   # advan <- 4
#   # trans <- 5
# })

test_that(get_test_name(get_test_description("ADVAN11 TRANS4")), {
  advan <- 11
  trans <- 4
  return(qualifySubroutine(advan, trans))
})

test_that(get_test_name(get_test_description("ADVAN12 TRANS4")), {
  advan <- 12
  trans <- 4
  return(qualifySubroutine(advan, trans))
})


