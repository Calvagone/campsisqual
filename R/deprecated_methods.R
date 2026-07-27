
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

#' Get more information on the given certificate.
#' 
#' `getCertificateInformation()` is deprecated in favor of `get_certificate_information()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_certificate_information
#' @return certificate information
#' @export
#' @rdname getCertificateInformation
getCertificateInformation <- function(cert) {
  lifecycle::deprecate_warn("1.5.0", "getCertificateInformation()", "get_certificate_information()")
  get_certificate_information(cert = cert)
}

#' Get the CA certificate.
#' 
#' `getCACertificate()` is deprecated in favor of `get_ca_certificate()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @return CA certificate, character vector
#' @export
#' @rdname getCACertificate
getCACertificate <- function() {
  lifecycle::deprecate_warn("1.5.0", "getCACertificate()", "get_ca_certificate()")
  get_ca_certificate()
}

#' Are vectors equal given the tolerance.
#' 
#' `areEqual()` is deprecated in favor of `are_equal()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams are_equal
#' @return logical vector
#' @export
#' @rdname areEqual
areEqual <- function(x, xref, tolerance, id, type) {
  lifecycle::deprecate_warn("1.5.0", "areEqual()", "are_equal()")
  are_equal(x = x, xref = xref, tolerance = tolerance, id = id, type = type)
}

#' Get qualification plot for given subject ID and variable.
#' 
#' `getPlot()` is deprecated in favor of `get_plot()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_plot
#' @return a plot
#' @export
#' @rdname getPlot
getPlot <- function(summary, id, variable) {
  lifecycle::deprecate_warn("1.5.0", "getPlot()", "get_plot()")
  get_plot(summary = summary, id = id, variable = variable)
}

setGeneric("getPlot", function(summary, id, variable) {
  lifecycle::deprecate_warn("1.5.0", "getPlot()", "get_plot()")
  get_plot(summary = summary, id = id, variable = variable)
})

#' Get qualification plot for given subject ID and variable.
#' 
#' `getTable()` is deprecated in favor of `get_table()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_table
#' @return a data frame with all variables being compared
#' @export
#' @rdname getTable
getTable <- function(summary, id) {
  lifecycle::deprecate_warn("1.5.0", "getTable()", "get_table()")
  get_table(summary = summary, id = id)
}

setGeneric("getTable", function(summary, id) {
  lifecycle::deprecate_warn("1.5.0", "getTable()", "get_table()")
  get_table(summary = summary, id = id)
})

#' Check TinyTeX installation.
#' 
#' `checkTinyTEXInstallation()` is deprecated in favor of `check_tinytex_installation()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @return TRUE if TinyTeX is installed correctly, FALSE otherwise
#' @export
#' @rdname checkTinyTEXInstallation
checkTinyTEXInstallation <- function() {
  lifecycle::deprecate_warn("1.5.0", "checkTinyTEXInstallation()", "check_tinytex_installation()")
  check_tinytex_installation()
}
