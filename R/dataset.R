#' Set the default observed compartment index.
#'
#' @param object generic object
#' @param index index of the observed compartment
#' @return updated object
#' @export
#' @rdname set_default_obs_cmt
set_default_obs_cmt <- function(object, index) {
  stop("No default function is provided")
}

setGeneric("set_default_obs_cmt", function(object, index) {
  index <- as.integer(index)
  standardGeneric("set_default_obs_cmt")
})

#' @rdname set_default_obs_cmt
setMethod("set_default_obs_cmt", signature = c("dataset", "integer"), definition = function(object, index) {
  object@arms@list <- object@arms@list %>% purrr::map(set_default_obs_cmt, index)
  return(object)
})

#' @rdname set_default_obs_cmt
setMethod("set_default_obs_cmt", signature = c("arm", "integer"), definition = function(object, index) {
  object@protocol@observations@list <- object@protocol@observations@list %>% purrr::map(set_default_obs_cmt, index)
  return(object)
})

#' @rdname set_default_obs_cmt
setMethod("set_default_obs_cmt", signature = c("observations", "integer"), definition = function(object, index) {
  object@compartment <- as.character(index)
  return(object)
})
