
#' Check TinyTeX installation
#' 
#' @return TRUE if TinyTeX is installed correctly, FALSE otherwise
#' @importFrom tinytex pdflatex
#' @export
checkTinyTEXInstallation <- function() {
  template <- "
\\documentclass{article}
\\begin{document}
Well done. Your installation works well.
\\end{document}"
  
  tmpDir <- tempdir()
  outputFile <- "check_tinytex.pdf"
  texFile <- "check_tinytex.tex"
  
  out <- tryCatch({
    with_dir(tmpDir, {
      fileConn <- file(texFile)
      writeLines(template, fileConn)
      close(fileConn)
      tinytex::pdflatex(texFile, pdf_file=outputFile)
    })
  }, error = function(e) {
    print(e)
  })
  
  if (is(out, "error")) {
    return(FALSE)
  } else if (is.character(out)) {
    return(out==outputFile)
  } else {
    print("Unknown error")
    return(FALSE)
  }
}
