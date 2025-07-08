
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


getQualificationTemplate <- function() {
partA <- 
"
---
output: pdf_document
params: 
    set_title: 'My Title!'
    qual_summary: list()
title: '`r params$set_title`'
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{array}
  - \\usepackage{multirow}
  - \\usepackage{wrapfig}
  - \\usepackage{float}
  - \\usepackage{colortbl}
  - \\usepackage{pdflscape}
  - \\usepackage{tabu}
  - \\usepackage{threeparttable}
  - \\usepackage{threeparttablex}
  - \\usepackage[normalem]{ulem}
  - \\usepackage{makecell}
  - \\usepackage{xcolor}
  - \\usepackage{titling}
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, message=FALSE, results='asis')
```

# Computer information

```{r}
computerInfo <-
  c('Full name:'=Sys.info()[['user']],
    'Computer name:'=Sys.info()[['nodename']],
    'Operating system:'=getOSName(),
    'R version:'=paste(R.Version()[c('major', 'minor')], collapse='.'),
    'rxode2 version:'=getNamespaceVersion('rxode2') %>% as.character(),
    'mrgsolve version:'=getNamespaceVersion('mrgsolve') %>% as.character(),
    'campsisqual version:'=getNamespaceVersion('campsisqual') %>% as.character()
    )
computerInfoDf <- data.frame(computerInfo)
kableExtra::kbl(computerInfoDf, col.names=NULL, booktabs=T) %>% print()
```

# Model qualification summary

```{r}
obsAll <- sum(qual_summary@tables %>% purrr::map_int(~nrow(.x)))
obsOK <- sum(qual_summary@tables %>% purrr::map_int(~nrow(.x %>% dplyr::filter(Pass=='OK'))))
summaryTable <-
  c('Model name:'=qual_summary@model_name,
    'Simulation engine:'=qual_summary@dest,
    'Relative tolerance:'=qual_summary@tolerance,
    'Variables compared:'=paste(qual_summary@variables, collapse=', '),
    'No of ind. compared:'=length(qual_summary@ids),
    'No of obs. compared:'=as.character(obsAll),
    'Success rate'=sprintf('%.1f%%', obsOK/obsAll*100)
    )
summaryTable <- data.frame(summaryTable)
kableExtra::kbl(summaryTable, col.names=NULL, booktabs=T) %>% print()
```
\\vspace{8pt}

```{r}
qualificationTime <- Sys.time()
qualificationTimeString <- format(qualificationTime, '%Y-%m-%d %H:%M:%S')
qualOK <- qual_summary %>% passed()
if (qualOK) {cat(sprintf('Status: \\\\textcolor{teal}{\\\\textbf{SUCCESSFUL}} on %s', qualificationTimeString))} else {cat(sprintf('Status: \\\\textcolor{red}{\\\\textbf{FAIL}} on %s', qualificationTimeString))}
cat('\\\\newpage\\n')
```

# Model qualification details

```{r}
table <- qual_summary@tables %>%
    purrr::map_df(~.x) %>%
    dplyr::group_by(dplyr::across(c('ID', 'variable'))) %>%
    dplyr::summarise('Similar obs.'=sprintf('%i / %i', sum(Pass=='OK'), dplyr::n()),
                     'Status'=ifelse(sum(Pass=='OK')==dplyr::n(), 'OK', 'NOK'),
                     .groups='drop') %>%
    dplyr::rename(Variable=variable)
kableExtra::kbl(table, booktabs=T, longtable=TRUE) %>%
   kableExtra::kable_styling(latex_options=c('repeat_header'), position='left') %>%
   print()
```
"
partB <- 
"
```{r}
fig_failed_only <- TRUE

for (id in qual_summary@ids) {
  summaryID <- qual_summary@summary %>% dplyr::filter(ID==id)
  vector <- as.vector(as.matrix(summaryID %>% dplyr::select(-ID)))
  failed <- !all(vector=='PASS')
  for (variable in qual_summary@variables) {
    if (!fig_failed_only || (fig_failed_only && failed)) {
      plot <- qual_summary %>% getPlot(id, variable)
      cat('\\\\newpage\\n')
      cat(paste0('## Subject ', id, '{#subject', id, '}'))
      cat('\\n\\n')
      print(plot + ggplot2::theme_bw())
    }
  }
}
```
"
return(paste0(partA, partB, collapse="\n"))
}

renderModelQualificationReport <- function() {
  template <- getQualificationTemplate()
  
  tmpFile <- tempfile(fileext = ".Rmd")
  fileConn <- file(tmpFile)
  writeLines(text=strsplit(x=template, split="\n")[[1]], con=fileConn)
  close(fileConn)
  
  output_dir <- "C:/prj/campsisqual"
  qual_summary <- qual
  
  ipred_source <- qual_summary@ipred_source
  
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

