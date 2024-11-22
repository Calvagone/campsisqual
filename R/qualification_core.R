
#' Are vectors equal given the tolerance.
#'
#' @param x first vector
#' @param xref second vector
#' @param tolerance relative tolerance
#' @param id subject ID (for troubleshooting)
#' @param type vector type (for troubleshooting)
#' @return logical vector
#' @importFrom assertthat assert_that
#' @export
areEqual <- function(x, xref, tolerance, id, type) {
  assertthat::assert_that(length(x)==length(xref), msg=sprintf("x and xref (%s) do not have the same length (ID=%s)", type, as.character(id)))
  relativeChange <- (x - xref)/xref
  return(abs(relativeChange) < tolerance | (x==0 & xref==0))
}

#' Compare NONMEM results with CAMPSIS results, according to the given tolerance.
#'
#' @param nonmem NONMEM results, dataframe
#' @param campsis CAMPSIS results, dataframe
#' @param variables variables to be compared
#' @param tolerance comparison tolerance
#' @param dest destination engine that was used in CAMPSIS
#' @return logical vector
#' @importFrom dplyr distinct filter mutate pull rename rename_at select
#' @importFrom ggplot2 aes geom_line geom_point ggplot ggtitle scale_colour_discrete ylab
#' @importFrom tibble add_column as_tibble
#' @importFrom campsis obsOnly
#' @export
compare <- function(nonmem, campsis, variables, tolerance, dest="RxODE") {
  
  # Check destination engine
  checkDest(dest)
  
  # Retrieve original destination tool from dataframe attributes (default is NONMEM)
  original_dest <- attr(nonmem, "original_dest")
  if (is.null(original_dest)) {
    original_dest <- "NONMEM"
  }
  
  # Filtering on observations
  nonmem_results <- nonmem %>% campsis::obsOnly()
  campsis_results <- as.data.frame(campsis) %>% campsis::obsOnly()

  # Access ORIGINAL_ID column from CAMPSIS results
  # Originally column was accessed from NONMEM results
  # But this is not a good idea because TAB file is formatted (e.g. 1.0510E+04)
  # and therefore the ORIGINAL_ID can't be a big integer or a string
  ids <- unique(campsis_results$ID)
  if ("ORIGINAL_ID" %in% colnames(campsis_results)) {
    original_ids <- campsis_results %>% dplyr::select(ID, ORIGINAL_ID) %>% dplyr::distinct() %>% dplyr::pull(ORIGINAL_ID)
    if (original_ids %>% length() != ids %>% length()) {
      stop("Incorrect column ORIGINAL_ID")
    }
  } else {
    original_ids <- character(ids %>% length())
  }
  
  qualificationSummary <- new("qualification_summary")
  qualificationSummary@ids <- as.integer(ids)
  qualificationSummary@original_ids <- as.character(original_ids)
  qualificationSummary@variables <- variables
  
  variablesOfInterest <- variables
  
  # Init qualification dataframe
  qualification <- data.frame(ID=ids)
  for (variable in variables) {
    qualification[, variable] <- rep(NA, length(ids))
  }
  
  for (id in ids) {
    index <- which(id==ids)
    original_id <- original_ids[index]
    
    nonmem_subj <- nonmem_results %>%
      dplyr::filter(ID==id) %>%
      dplyr::mutate(Simulation=original_dest) %>%
      dplyr::select(c("ID", "TIME", "Simulation", dplyr::all_of(variablesOfInterest)))
    
    campsis_subj <- campsis_results %>%
      dplyr::filter(ID==id) %>%
      dplyr::mutate(Simulation=dest) %>%
      dplyr::select(c("ID", "TIME", "Simulation", dplyr::all_of(variablesOfInterest)))
    
    if (!all(areEqual(nonmem_subj$TIME, campsis_subj$TIME, tolerance=tolerance, id=id, type="TIME"))) {
      stop(paste0("Times are not identical between NONMEM and CAMPSIS for subject ", id))
    }
    
    subj <- dplyr::bind_rows(campsis_subj, nonmem_subj)
    subj <- subj %>% tidyr::gather(key="variable", value="value", dplyr::all_of(variablesOfInterest), -ID, -TIME, -Simulation)
    subj$Pass <- FALSE
    
    # Qualification results summary
    for (output in variablesOfInterest) {
      nonmemOutput <- subj %>%
        dplyr::filter(variable==output & Simulation==original_dest) %>%
        dplyr::pull(value)
      
      campsisOutput <- subj %>%
        dplyr::filter(variable==output & Simulation==dest) %>%
        dplyr::pull(value)
      
      sameOutput <- areEqual(nonmemOutput, campsisOutput, tolerance=tolerance, id=id, type="OUTPUT")
      if (any(is.na(sameOutput))) {
        stop(paste0("NA's detected in original ID ", original_id))
      }
      if (all(sameOutput)) {
        qualification[which(qualification$ID==id), output] <- "PASS"
      } else {
        qualification[which(qualification$ID==id), output] <- "FAIL"
      }
      
      subj <- subj %>%
        dplyr::mutate(Pass=ifelse(variable==output & Simulation==original_dest & sameOutput, TRUE, Pass))
      subj <- subj %>%
        dplyr::mutate(Pass=ifelse(variable==output & Simulation==dest & sameOutput, TRUE, Pass))
    }
    subj$Pass <-  factor(subj$Pass, levels=c(FALSE, TRUE), labels=c("NOK", "OK"))
    
    # All pass
    allPass <- all(qualification[which(qualification$ID==id), variablesOfInterest]=="PASS")
    allPass <- ifelse(allPass, "Pass", "Fail")

    # Saving plots
    plots <- list()
    for (output in variablesOfInterest) {
      data <- subj %>% dplyr::filter(variable==output)
      p <- ggplot2::ggplot(data=data, mapping=ggplot2::aes(x=TIME, y=value, group=Simulation)) + 
        ggplot2::geom_line() + ggplot2::facet_wrap(~Simulation) +
        ggplot2::ggtitle(paste0("Subject ", id, " (", allPass, ")", ifelse(original_id=="", "", paste0(" - ", original_id)))) +
        ggplot2::geom_point(mapping=ggplot2::aes(color=Pass), size=4) +
        ggplot2::ylab(output) + ggplot2::scale_colour_discrete(drop=FALSE)
      plots[[output]] <- p
    }
    qualificationSummary@plots[[as.character(id)]] <- plots
    
    # Saving table output
    timeCAMPSIS <- paste0("TIME_", dest)
    timeNONMEM <- paste0("TIME_", original_dest)
    
    tryCatch(
      {
        subjNONMEM <- subj %>%
          dplyr::filter(Simulation==original_dest) %>%
          tidyr::spread(Simulation, value) %>% dplyr::rename_at(.vars="TIME", .funs=~timeNONMEM)
        
        subjCAMPSIS <- subj %>% dplyr::filter(Simulation==dest) %>%
          tidyr::spread(Simulation, value) %>%
          dplyr::rename_at(.vars="TIME", .funs=~timeCAMPSIS)
      }, error=function(cond) {
        warning(paste0("Problem with original ID ", original_id))
      }
    )
    
    subjNONMEM <- tibble::as_tibble(subjNONMEM)
    subjNONMEM <- subjNONMEM %>%
      tibble::add_column(TIME_CAMPSIS=subjCAMPSIS[, timeCAMPSIS], .after=timeNONMEM) %>%
      dplyr::rename_at(.vars="TIME_CAMPSIS", .funs=~timeCAMPSIS)
    subjNONMEM[, dest] <- subjCAMPSIS[, dest]
    qualificationSummary@tables[[as.character(id)]] <- subjNONMEM
  }
  
  # Saving summary
  qualificationSummary@summary <- qualification

  return(qualificationSummary)
}
