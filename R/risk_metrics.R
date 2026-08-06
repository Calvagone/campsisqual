summarise_riskmetric <- function(x, package) {
  UseMethod("summarise_riskmetric")
}

risk_metric_to_string <- function(x) {
  if (is.character(x)) {
    return(paste0(as.character(x), collapse = ", "))
  } else {
    return("")
  }
}

risk_metric_result <- function(x) {
  ok <- FALSE
  if (is.logical(x)) {
    if (as.logical(x) %>% length() >= 1) {
      ok <- all(as.logical(x))
    }
  } else if (is.character(x)) {
    if (as.character(x) %>% length() >= 1) {
      ok <- TRUE
    }
  } else if (is.integer(x)) {
    ok <- ifelse(x >= 1, TRUE, FALSE)
  } else {
    print(sprintf("Issue with class %s", class(x)[1]))
  }
  return(ifelse(ok, "Yes", "No"))
}

#' @export
summarise_riskmetric.default <- function(x, package) {
  stop("No summarise method for object of class ", class(x))
}

#' @export
summarise_riskmetric.pkg_metric_has_vignettes <- function(x, package) {
  # By default, vignettes are not installed
  # However we have vignettes in campsismod and campsis
  # Let's simply override the default behavior
  result <- risk_metric_result(x)
  info <- risk_metric_to_string(x)

  if (package %in% c("campsismod", "campsis")) {
    result <- "Yes"
    info <- sprintf("https://calvagone.github.io/%s.doc/articles", package)
  }

  return(tibble::tibble(Criteria = "Has vignettes?", Result = result, `More info` = info, Category = "Documentation"))
}

#' @export
summarise_riskmetric.pkg_metric_has_website <- function(x, package) {
  return(tibble::tibble(
    Criteria = "Has website?",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Documentation"
  ))
}

get_package_branch <- function(package) {
  if (package %in% c("campsismod", "campsis", "campsisnca", "campsismisc", "campsistrans", "ecampsis")) {
    return("main")
  } else {
    return("master")
  }
}

#' @export
summarise_riskmetric.pkg_metric_has_news <- function(x, package) {
  # Add a link to the online file
  result <- risk_metric_result(x)
  if (result == "Yes") {
    info <- sprintf("https://github.com/Calvagone/%s/blob/%s/NEWS.md", package, get_package_branch(package))
  } else {
    info <- ""
  }
  return(tibble::tibble(Criteria = "Has NEWS file?", Result = result, `More info` = info, Category = "Documentation"))
}

#' @export
summarise_riskmetric.pkg_metric_has_maintainer <- function(x, package) {
  return(tibble::tibble(
    Criteria = "Has maintainer?",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Maintenance"
  ))
}

#' @export
summarise_riskmetric.pkg_metric_news_current <- function(x, package) {
  return(tibble::tibble(
    Criteria = "Package version in NEWS?",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Maintenance"
  ))
}

#' @export
summarise_riskmetric.pkg_metric_license <- function(x, package) {
  return(tibble::tibble(
    Criteria = "License",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Maintenance"
  ))
}

#' @export
summarise_riskmetric.pkg_metric_has_source_control <- function(x, package) {
  return(tibble::tibble(
    Criteria = "Has source control?",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Transparency"
  ))
}

#' @export
summarise_riskmetric.pkg_metric_has_bug_reports_url <- function(x, package) {
  return(tibble::tibble(
    Criteria = "Has bug reports URL?",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Transparency"
  ))
}

#' @export
summarise_riskmetric.pkg_metric_successful_tests <- function(x, package) {
  return(tibble::tibble(
    Criteria = "Were tests successful?",
    Result = risk_metric_result(x),
    `More info` = risk_metric_to_string(x),
    Category = "Testing"
  ))
}

#' @export
summarise_riskmetric.pkg_metric_online_code_coverage <- function(x, package) {
  if (package %in% c("campsismod", "campsis", "campsisnca", "campsismisc")) {
    result <- "Yes"
    info <- sprintf("https://app.codecov.io/gh/Calvagone/%s", package)
  } else {
    result <- risk_metric_result(x)
    info <- risk_metric_to_string(x)
  }
  return(tibble::tibble(
    Criteria = "Online code coverage URL?",
    Result = result,
    `More info` = info,
    Category = "Testing"
  ))
}

#'
#' Access package using the riskmetric package.
#'
#' @param package package name
#' @param successful_tests were tests successful?
#' @return human readable table
#' @export
#' @importFrom riskmetric pkg_assess pkg_ref assess_has_bug_reports_url assess_has_maintainer assess_has_news assess_has_source_control assess_has_vignettes assess_has_website assess_license assess_news_current
#' @importFrom purrr map list_rbind
#' @importFrom dplyr arrange
assess_package <- function(package, successful_tests) {
  path <- find.package(package)
  if (length(path) == 0) {
    stop(sprintf("Package '%s' not found", package))
  }
  if (length(path) > 1) {
    stop(sprintf("Multiple packages '%s' found", package))
  }

  assessment <- riskmetric::pkg_assess(
    riskmetric::pkg_ref(path),
    assessments = list(
      riskmetric::assess_has_bug_reports_url,
      riskmetric::assess_has_maintainer,
      riskmetric::assess_has_news,
      riskmetric::assess_has_source_control,
      riskmetric::assess_has_vignettes,
      riskmetric::assess_has_website,
      riskmetric::assess_license,
      riskmetric::assess_news_current
    )
  )

  # Compute a global score
  score <- riskmetric::pkg_score(assessment)
  score$has_vignettes <- NULL # Removed because depends on installations of vignettes
  score$license <- NULL # Removed because value is NA

  # TODO: decide what we do and how we weight

  # This will remove the class on the list object
  assessment <- as.list(assessment)

  # Add metrics for successful tests
  class(successful_tests) <- c(class(successful_tests), "pkg_metric_successful_tests")

  # Add metrics for online code coverage
  onlineCodeCoverage <- FALSE # By default
  class(onlineCodeCoverage) <- c(class(onlineCodeCoverage), "pkg_metric_online_code_coverage")

  assessment[[length(assessment) + 1]] <- successful_tests
  assessment[[length(assessment) + 1]] <- onlineCodeCoverage

  retValue <- assessment %>%
    purrr::map(~ summarise_riskmetric(.x, package = package)) %>%
    purrr::list_rbind() %>%
    dplyr::arrange(Category)

  return(retValue)
}
