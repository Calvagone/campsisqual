
#_______________________________________________________________________________
#----                     qualification_summary class                       ----
#_______________________________________________________________________________
#'
#' Qualification summary class.
#' 
#' @slot ids list of subject ID's being qualified
#' @slot original_ids list of original ID's
#' @slot variables variables from dataset that were qualified
#' @slot summary qualification summary, data frame
#' @slot plots list of qualification plots
#' @slot tables list of qualification tables
#' @slot ipred_source source of individual predictions, e.g. "NONMEM"
#' @slot dest destination engine, e.g. "rxode2"
#' @slot model_name name of the model being qualified, e.g. "RUN001"
#' @slot tolerance relative tolerance used for qualification, numeric
#'
#' @export
setClass(
  "qualification_summary",
  representation(
    ids = "integer",
    original_ids = "character",
    variables = "character",
    summary = "data.frame",
    plots = "list",
    tables = "list",
    ipred_source = "character",
    dest = "character",
    model_name = "character",
    tolerance = "numeric"
  ),
  prototype(ipred_source="<SOURCE>", model_name="<MODEL_NAME>", dest="<SIMULATION_ENGINE>",
            tolerance=as.numeric(NA)),
)

#_______________________________________________________________________________
#----                             getPlot                                   ----
#_______________________________________________________________________________

#' Get qualification plot for given subject ID and variable.
#' 
#' @param summary summary object
#' @param id subject ID
#' @param variable compared variable
#' @return a plot
#' @export
#' @rdname getPlot
getPlot <- function(summary, id, variable) {
  stop("No default function is provided")
}

setGeneric("getPlot", function(summary, id, variable) {
  standardGeneric("getPlot")
})

#' @rdname getPlot
setMethod("getPlot", signature = c("qualification_summary", "numeric", "character"), definition = function(summary, id, variable) {
  id <- as.character(id)
  list <- summary@plots[[id]]
  return(list[[variable]])
})

#_______________________________________________________________________________
#----                             getTable                                  ----
#_______________________________________________________________________________

#' Get qualification results (table form) for given subject ID.
#' 
#' @param summary summary object
#' @param id subject ID
#' @return a data frame with all variables being compared
#' @export
#' @rdname getTable
getTable <- function(summary, id) {
  stop("No default function is provided")
}

setGeneric("getTable", function(summary, id) {
  standardGeneric("getTable")
})

#' @rdname getTable
setMethod("getTable", signature = c("qualification_summary", "numeric"), definition = function(summary, id) {
  id <- as.character(id)
  table <- summary@tables[[id]]
  return(table)
})

#_______________________________________________________________________________
#----                               passed                                  ----
#_______________________________________________________________________________

#' Say if the qualification passed or not.
#' 
#' @param summary qualification summary object
#' @return TRUE/FALSE
#' @export
#' @rdname passed
passed <- function(summary) {
  stop("No default function is provided")
}

setGeneric("passed", function(summary) {
  standardGeneric("passed")
})

#' @rdname passed
setMethod("passed", signature = c("qualification_summary"), definition = function(summary) {
  vector <- as.vector(as.matrix(summary@summary %>% dplyr::select(-ID)))
  return(all(vector=="PASS"))
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Export qualification summary to PDF file.
#'
#' @param object qualification summary object
#' @param file output file name
#' @param original_model original model that was imported (e.g. RUN001.ctl), character, default is NULL
#' @param failed_only if TRUE, only failed subjects will be included in the report, default is TRUE
#' @param debug_tables if TRUE, include debug tables in the report with detailed results (only for failing subjects), default is TRUE
#' @param notes additional notes to be included in the report, character, default is NULL
#' @param ... additional parameters, unused
#' @importFrom rmarkdown render
setMethod("write", signature=c("qualification_summary", "character"),
          definition=function(object, file, original_model=NULL, failed_only=TRUE, debug_tables=TRUE, notes=NULL, ...) {
  # tmpFile <- "C:/prj/campsisqual/data-raw/model_qualification_template.Rmd"

  # Export Rmd to temporary file
  tmpFile <- tempfile(fileext = ".Rmd")
  fileConn <- file(tmpFile)
  writeLines(text=campsisqual::model_qualification_template, con=fileConn)
  close(fileConn)
  
  # Filename and output directory
  output_dir <- dirname(file)
  output_file <- basename(file)
  
  title <- sprintf("Qualification of Campsis model against %s predictions", object@ipred_source)
  
  # Render with Rmd
  rmarkdown::render(
    input=tmpFile,
    output_format="pdf_document",
    output_file=output_file,
    output_dir=output_dir,
    params=list(set_title=title, qual_summary=object, original_model=original_model,
                failed_only=failed_only, debug_tables=debug_tables, notes=notes)
  )
})
