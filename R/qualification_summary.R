
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
  prototype(ipred_source="<SOURCE>", model_name="<MODEL_NAME>", dest="<SIMULATION_ENGINE>", tolerance=as.numeric(NA)),
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

#' @importFrom gridExtra grid.table
setMethod("write", signature = c("qualification_summary", "character"), definition = function(object, file, log=FALSE, ...) {
  pdf(file=file, width=10, height=10)
  
  summary <- campsismod::processExtraArg(args=list(...), name="summary", default=TRUE)
  fig_failed_only <- campsismod::processExtraArg(args=list(...), name="fig_failed_only", default=FALSE)
  table_failed_only <- campsismod::processExtraArg(args=list(...), name="table_failed_only", default=TRUE)
  firstPrint <- TRUE
  
  for (id in object@ids) {
    summaryID <- object@summary %>% dplyr::filter(ID==id)
    vector <- as.vector(as.matrix(summaryID %>% dplyr::select(-ID)))
    failed <- !all(vector=="PASS")
    for (variable in object@variables) {
      if (!fig_failed_only || (fig_failed_only && failed)) {
        # No need of plot.new for plots
        plot <- object %>% getPlot(id, variable)
        if (log) {
          plot <- plot + ggplot2::scale_y_log10()
        }
        print(plot)
        firstPrint <- FALSE
      }
    }
    if (!table_failed_only || (table_failed_only && failed)) {
      if (!firstPrint) {
        plot.new()
      } else {
        firstPrint <- FALSE
      }
      gridExtra::grid.table(object %>% getTable(id), rows=NULL)
    }
  }
  if (summary) {
    if (!firstPrint) {
      plot.new()
    } else {
      firstPrint <- FALSE
    }
    gridExtra::grid.table(object@summary, rows=NULL)
  }
  dev.off()
})

#_______________________________________________________________________________
#----                                report                                 ----
#_______________________________________________________________________________

renderModelQualificationReport <- function() {

  # tmpFile <- "C:/prj/campsisqual/data-raw/model_qualification_template.Rmd"
  output_dir <- "C:/prj/campsisqual"
  qual_summary <- qual
  
  ipred_source <- qual_summary@ipred_source
  
  tmpFile <- tempfile(fileext = ".Rmd")
  fileConn <- file(tmpFile)
  writeLines(text=campsisqual::model_qualification_template, con=fileConn)
  close(fileConn)
  
  # Render with Rmd
  rmarkdown::render(
    input = tmpFile,
    output_format = "pdf_document",
    output_file = "qualification_report.pdf",
    output_dir=output_dir,
    params = list(set_title=sprintf("Qualification of Campsis model against %s predictions", ipred_source),
                  qual_summary=qual_summary)
  )
  
}

