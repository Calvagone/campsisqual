
#' Set the default observed compartment index.
#' 
#' @param object generic object
#' @param index index of the observed compartment
#' @return updated object
#' @export
#' @rdname setDefaultObsCmt
setDefaultObsCmt <- function(object, index) {
  stop("No default function is provided")
}

setGeneric("setDefaultObsCmt", function(object, index) {
  index <- as.integer(index)
  standardGeneric("setDefaultObsCmt")
})

#' @rdname setDefaultObsCmt
setMethod("setDefaultObsCmt", signature=c("dataset", "integer"), definition=function(object, index) {
  object@arms@list <- object@arms@list %>% purrr::map(setDefaultObsCmt, index)
  return(object)
})

#' @rdname setDefaultObsCmt
setMethod("setDefaultObsCmt", signature=c("arm", "integer"), definition=function(object, index) {
  object@protocol@observations@list <- object@protocol@observations@list %>% purrr::map(setDefaultObsCmt, index)
  return(object)
})

#' @rdname setDefaultObsCmt
setMethod("setDefaultObsCmt", signature=c("observations", "integer"), definition=function(object, index) {
  object@compartment <- as.character(index)
  return(object)
})