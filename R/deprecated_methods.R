
#' Get OS name.
#' 
#' `getOSName()` is deprecated in favor of `get_os_name()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_os_name
#' @return the OS name
#' @export
#' @rdname getOSName
getOSName <- function(short=FALSE) {
  lifecycle::deprecate_warn("1.5.0", "getOSName()", "get_os_name()")
  get_os_name(short = short)
}

#' Get OS name.
#' 
#' `runQualification()` is deprecated in favor of `run_qualification()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams run_qualification
#' @return the OS name
#' @export
#' @rdname runQualification
runQualification <- function(packages, fullname, initials=NULL, output_dir=getwd(), qualification_suite=NULL,
                             cpu=6L, skip_vdiffr=TRUE, skip_python=TRUE) {
  lifecycle::deprecate_warn("1.5.0", "runQualification()", "run_qualification()")
  run_qualification(
    packages = packages,
    fullname = fullname,
    initials = initials,
    output_dir = output_dir,
    qualification_suite = qualification_suite,
    cpu = cpu,
    skip_vdiffr = skip_vdiffr,
    skip_python = skip_python
  )
}

