#_______________________________________________________________________________
#----                     qualification_suite class                       ----
#_______________________________________________________________________________

#'
#' Qualification suite class.
#' 
#' @slot path path to qualification suite (zip file)
#' @slot credentials qualification suite credentials
#' @slot nmodels number of models to qualify
#' @export
setClass(
  "qualification_suite",
  representation(
    path="character",
    credentials="qualification_suite_credentials",
    nmodels="integer"
  )
)

#'
#' Qualification suite.
#' 
#' @param path path to qualification suite (zip file)
#' @param credentials qualification suite credentials, see ?Credentials
#' @param nmodels number of models to qualify, max number is 97. Please note that the models will be drawn at random.
#' @return a qualification suite object
#' @export
QualificationSuite <- function(path, credentials, nmodels=97L) {
  if (!file.exists(path)) {
    stop("Qualification suite (.zip) not found.")
  }
  if (!endsWith(path, ".zip")) {
    stop("Qualification suite must be a .zip file.")
  }
  return(new("qualification_suite", path=normalizePath(path), credentials=credentials, nmodels=as.integer(nmodels)))
}
