
#' Read all sorts of NONMEM tables (extensions ext, tab, cov, etc, ...).
#'
#' @param file the file to be read
#' @return a list of data frame (one dataframe per table)
#' @export
read.nonmem <- function(file) {
  
  # Read all lines from NONMEM results file (*.ext, *.tab, *.cov, etc, ...)
  fileConn <- file(file)
  allLines <- readLines(con=fileConn)
  close(fileConn)
  
  # Retrieve all table indexes
  allTableIndexes <- grep("^TABLE NO\\. .*$", x=allLines, ignore.case=T)
  
  if (length(allTableIndexes)==0) {
    stop("No NONMEM table was recognised")
  }
  
  # Extract tables
  tables <- purrr::map2(.x=allTableIndexes, .y=c(allTableIndexes[-1], length(allLines)+1),
                        .f=~read.nonmemtable(allLines[seq(.x, .y-1, by=1)]))
  return(tables)
}

#' Read a NONMEM table.
#'
#' @param content table content
#' @return a dataframe
#' @importFrom purrr map_df
read.nonmemtable <- function(content) {
  
  # Skip first line
  content <- content[-1]
  
  # Retrieve headers
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  headers <- trim(content[1])
  headers <- gsub("\\s+", " ", headers)
  headers <- strsplit(headers, " ")[[1]]
  content <- content[-1]
  
  # Extract values
  data <- content %>% purrr::map_df(.f=function(.x) {
    
    .x <- trim(.x)
    .x <- gsub("\\s+", " ", .x)
    .x <- strsplit(.x, " ")[[1]]
    .x <- as.numeric(.x)
    names(.x) <- headers
    return(.x)
  })
  
  return(data)
}

