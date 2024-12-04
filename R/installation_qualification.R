
#'
#' Clean package results.
#'
#' @param raw raw results
#' @return cleaned results
#' @importFrom dplyr as_tibble group_by rename summarise
cleanPackageResults <- function(raw) {
  retValue <- raw %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Test = test) %>%
    dplyr::group_by(file, context, Test) %>%
    dplyr::summarise(
      #NumTests = first(nb),
      Passed   = sum(passed),
      Failed   = sum(failed),
      Warnings = sum(warning),
      Errors   = sum(as.numeric(error)),
      Skipped  = sum(as.numeric(skipped)),
      .groups = "drop"
    )
  return(retValue)
}

#'
#' Summarise results.
#'
#' @param results results
#' @return summarised results
#' @importFrom dplyr summarise_if
summariseResults <- function(results) {
  return(results %>% dplyr::summarise_if(is.numeric, sum))
}

#'
#' Print summary kable.
#'
#' @param x data frame
#' @return printed kable
#' @importFrom kableExtra kbl kable_styling column_spec
printSummaryKable <- function(x) {
  return(x %>% kableExtra::kbl(booktabs=T) %>%
           kableExtra::kable_styling(full_width=T, latex_options=c("striped", "hold_position")) %>%
           kableExtra::column_spec(1, width="2.6cm") %>%
           kableExtra::column_spec(2, width="1.5cm") %>% print())
}

#'
#' Print kable.
#'
#' @param x data frame
#' @return printed kable
#' @importFrom kableExtra kbl kable_styling column_spec
printKable <- function(x) {
  return(x %>% kableExtra::kbl(booktabs=T) %>%
           kableExtra::kable_styling(full_width=T, latex_options=c("striped", "hold_position")) %>%
           kableExtra::column_spec(1, width = "8cm") %>% print())
}

#'
#' Passed method.
#'
#' @param results results
#' @return TRUE or FALSE
#' @importFrom dplyr summarise_if
qualificationPassed <- function(results) {
  row <- results %>% dplyr::summarise_if(is.numeric, sum)
  retValue <- TRUE
  if (row$Failed > 0) {
    retValue <- FALSE
  }
  # Too strict, so disabled
  # if (row$Warnings > 0) {
  #   retValue <- FALSE
  # }
  if (row$Errors > 0) {
    retValue <- FALSE
  }
  return(retValue)
}

#'
#' Write results.
#'
#' @param results results
#' @return nothing
#' @importFrom dplyr nest_by
#' @importFrom purrr pwalk
writeResults <- function(results) {
  nestedResults <- results %>%
    dplyr::nest_by(file, context, .key = "Results") %>%
    dplyr::nest_by(file, .key = "Categories")
  return(purrr::pwalk(list(nestedResults$file, nestedResults$Categories), writeFileResults))
}

#'
#' Write file results.
#'
#' @param file file
#' @param categories categories 
#' @param ... extra arguments
#' @return nothing
#' @importFrom purrr pwalk
writeFileResults <- function(file, categories, ...) {
  #cat("###", file, "\n")
  return(purrr::pwalk(list(categories$context, categories$Results, file), writeCategoryResults))
}

#'
#' Write category results.
#'
#' @param category category
#' @param tests tests 
#' @param file file
#' @param ... extra arguments
#' @return nothing
#' @importFrom kableExtra kbl kable_styling column_spec
writeCategoryResults <- function(category, tests, file, ...) {
  cat(paste0("**", category, "**\n\n"))
  cat("*Script: ", file, "*\n")
  return(tests %>% kableExtra::kbl(booktabs=T, longtable=T) %>%
           kableExtra::kable_styling(full_width=F, latex_options=c("hold_position", "striped")) %>%
           kableExtra::column_spec(1, width = "8cm") %>% print())
}

bindPackageAndVersion <- function(x, package) {
  return(cbind("Package"=package, "Version"=getNamespaceVersion(package) %>% as.character(),
               x %>% summariseResults()))
}

getOSName <- function(short=FALSE) {
  os <- paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
  if (short) {
    os <- gsub(" ", "_", x=os)
    os <- gsub("Windows", "Win", x=os)
    os <- gsub("Win_10", "Win10", x=os)
  }
  return(os)
}

#'
#' Collect package warnings
#' 
#' @param x package test results
#' @return a tibble with 2 columns: 'warning', 'occurrences'
#' @importFrom dplyr group_by summarise
#' @importFrom tibble tibble
#' @importFrom purrr map_chr discard
collectPackageWarnings <- function(x) {
  assertthat::assert_that(is(x, "testthat_results"), msg="x must be of type 'testthat_results'")
  warningMessages <- NULL
  for (i in 1:length(x)) {
    testResult <- x[[i]]
    warnings <- testResult$results %>% purrr::map_chr(.f=function(y) {
      if (is(y, "expectation_warning")) {
        return(y$message)
      } else {
        return(as.character(NA))
      }
    }) %>% purrr::discard(~is.na(.x))
    warningMessages <- warningMessages %>%
      append(warnings)
  }
  
  retValue <- tibble::tibble(warning=warningMessages) %>%
    dplyr::group_by(warning) %>%
    dplyr::summarise(occurrences=dplyr::n())
  
  return(retValue)
}

#'
#' Run qualification.
#'
#' @param packages packages to qualify
#' @param fullname fullname of the person running the qualification
#' @param initials username initials to be used in the filename. If not provided, initials are deduced from the fullname.
#' @param output_dir output directory
#' @param qualification_suite qualification suite object, type ?QualificationSuite for more information
#' @param cpu number of workers to be used, default is 6 workers
#' @param skip_vdiffr skip Vdiffr tests, default is TRUE
#' @return TRUE if the qualification passes, FALSE otherwise
#' @importFrom purrr map_df
#' @importFrom testthat test_package
#' @importFrom rmarkdown find_pandoc
#' @importFrom tictoc tic toc
#' @importFrom PKI PKI.load.cert PKI.verifyCA
#' @export
runQualification <- function(packages, fullname, initials=NULL, output_dir=getwd(), qualification_suite=NULL, cpu=6L, skip_vdiffr=TRUE) {
  if (!all(packages %in% c("campsismod", "campsis", "campsisnca", "campsismisc", "campsisqual", "campsistrans", "ecampsis"))) {
    stop("Invalid packages. Only packages from the Campsis suite can be qualified.")
  }

  if (length(find.package("ncappc", quiet=TRUE))==0) stop("ncappc not installed")
  if (length(find.package("tinytex", quiet=TRUE))==0) stop("tinytex not installed")
  if (length(find.package("tictoc", quiet=TRUE))==0) stop("tictoc not installed")
  if (length(find.package("mrgsolve", quiet=TRUE))==0) stop("mrgsolve not installed")
  if (length(find.package("rxode2", quiet=TRUE))==0) stop("rxode2 not installed")
  
  # Check tinyTEX installation
  if (!checkTinyTEXInstallation()) {
    stop("TinyTeX is not properly installed. Please install it using tinytex::install_tinytex()")
  }
  
  isWindows <- tolower(getOSName()) %>% startsWith("win")
  if (isWindows) {
    defaultPath <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
    if (file.exists(defaultPath)) {
      Sys.setenv("RSTUDIO_PANDOC"=defaultPath)
    }
  }
  
  tictoc::tic()
  results <- runQualificationCore(packages=packages, qualification_suite=qualification_suite, cpu=cpu, skip_vdiffr=skip_vdiffr)
  if (!is.null(qualification_suite)) {
    report <- renderReport(results=results, packages=packages, fullname=fullname, initials=initials,
                           output_dir=output_dir, qualification_suite=qualification_suite)
  }
  tictoc::toc()
  
  # Check if qualification passed
  qualOK <- results$summarised %>% qualificationPassed()
  print(ifelse(qualOK, "QUALIFICATION SUCCESSFUL", "QUALIFICATION FAILED"))
  
  return(qualOK)
}

#'
#' Run qualification core.
#'
#' @param packages packages to qualify
#' @param qualification_suite qualification suite object
#' @param cpu number of workers to be used
#' @param skip_vdiffr skip Vdiffr tests, default is TRUE
#' @return summarised results
#' @importFrom purrr map_df
#' @importFrom testthat test_package
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom pbmcapply progressBar
#' @importFrom zip unzip
#' @export
runQualificationCore <- function(packages, qualification_suite=NULL, cpu=6L, skip_vdiffr=TRUE) {
  packagesNo <- length(packages)
  if (packagesNo == 0) {
    stop("No packages to qualify")
  }
  qualSuite <- NULL
  if (is.null(qualification_suite)) {
    cat("No qualification suite provided. Only the base tests will be performed and no qualification report will be produced.\n")
  } else {
    credentials <- qualification_suite@credentials
    zipPath <- qualification_suite@path
    tmpDir <- tempdir()
    zip::unzip(zipfile=zipPath, exdir=tmpDir)
    baseDir <- basename(zipPath) |>
      gsub(pattern=".zip", replacement="")
    suiteDir <- "qualification_suite"
    qualSuite <- file.path(tmpDir, suiteDir)
    with_dir(
      tmpDir,
      decryptFolder(from=baseDir, to=suiteDir, private_key=credentials@private_key_path,
                    passphrase=credentials@passphrase)
    )
  }
  
  # Check package existence and check that it was installed with tests
  for (package in packages) {
    # Check package exists (error is raised if it does not exist)
    pkgPath <- find.package(package)
    
    # Check that testthat repo exists
    testRepo <- file.path(pkgPath, "tests/testthat")
    if (!dir.exists(testRepo)) {
      stop(paste0("Package ", package, " does not have a testthat repository"))
    }
  }
  
  # 1 CPU per package, don't create workers if not needed
  if (packagesNo <= cpu) {
    cpu <- packagesNo
  }
  
  # Start cluster configuration
  if (cpu > 1) {
    cl <-  parallel::makeCluster(cpu)
    doSNOW::registerDoSNOW(cl)
  }
  `%dopar%` <- foreach::`%dopar%`
  
  # Progress bar
  pb <- pbmcapply::progressBar(max=packagesNo, style="ETA")
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  # Preparing options
  qualOptions <- list()
  if (is.null(qualification_suite)) {
    qualOptions$QUALIFICATION_SUITE=""
  } else {
    qualOptions$QUALIFICATION_SUITE=qualSuite
    qualOptions$QUALIFICATION_SUITE_N_MODELS=qualification_suite@nmodels
  }
  
  testResults <- foreach::foreach(i=seq_along(packages), .combine=append, .options.snow=opts) %dopar% {
    package <- packages[i]
    Sys.setenv("NOT_CRAN"=TRUE)
    options(campsisqual.options=qualOptions)
    options(campsis.options=list(SKIP_LONG_TESTS=FALSE, SKIP_VDIFFR_TESTS=skip_vdiffr))
    options(ecampsis.options=list(SKIP_LONG_TESTS=FALSE, SKIP_VDIFFR_TESTS=skip_vdiffr, SKIP_NM_IMPORT_TESTS=FALSE))
    retValue <- list()
    retValue[[package]] <- testthat::test_package(package, reporter=c("list"), stop_on_failure=FALSE, stop_on_warning=FALSE)
    return(retValue)
  }
  
  # Stop cluster
  if (cpu > 1) {
    parallel::stopCluster(cl)
  }
  
  # Clean test results
  testResultsCleaned <- testResults %>%
    purrr::map(~cleanPackageResults(.x))
  
  # Collect warnings
  warnings <- testResults %>%
    purrr::map(~collectPackageWarnings(.x))
  
  summarisedResults <- packages %>% purrr::map_df(.f=function(package) {
    return(testResultsCleaned[[package]] %>% bindPackageAndVersion(package))
  })
  
  return(list(summarised=summarisedResults, all=testResultsCleaned, warnings=warnings, qualSuite=qualSuite))
}

#'
#' Render report with rmarkdown.
#'
#' @param results results
#' @param packages packages 
#' @param fullname fullname
#' @param initials username initials to be used in the filename. If not provided, initials are deduced from the fullname.
#' @param output_dir output directory
#' @param qualification_suite qualification suite object
#' @return nothing
#' @importFrom rmarkdown render
renderReport <- function(results, packages, fullname, initials=NULL, output_dir, qualification_suite) {
  credentials <- qualification_suite@credentials
  eCampsisQual <- "ecampsis" %in% packages

  # Report filename
  reportFilename <- paste0(sprintf("IQ_OQ_%s-", ifelse(eCampsisQual, "e-Campsis", "Campsis_Suite")),
                           ifelse(eCampsisQual, getNamespaceVersion("ecampsis"), getNamespaceVersion("campsis")),
                           "_", getOSName(short=TRUE), "_", initials,
                           "_", gsub("-", "", x=Sys.Date() %>% as.character()))
  
  # Initials deduced from fullname (if not provided)
  parts <- strsplit(fullname, " ")[[1]]
  if (is.null(initials)) {
    initials <- substring(parts, 0, 1) |>
      paste0(collapse="")
  }
  
  rmdFilename <- ifelse(eCampsisQual,
                        "qualification_ecampsis_template.Rmd",
                        "qualification_campsis_template.Rmd")
  
  # Rendering report
  report <- tryCatch(
    rmarkdown::render(
      input=file.path(results$qualSuite, "reporting", rmdFilename),
      params=list(results=results$all,
                  summarised_results=results$summarised,
                  warnings=results$warnings,
                  packages=packages,
                  fullname=fullname,
                  credentials=credentials),
      output_file=reportFilename,
      output_dir=output_dir
    ),
    error=function(cond) {
      print(cond)
    },
    finally={
      unlink(results$qualSuite, recursive=TRUE)
    }
  )
  
  return(report)
}
