template <- file.path("data-raw", "model_qualification_template.Rmd")
model_qualification_template <- readChar(template, file.info(template)$size)
usethis::use_data(model_qualification_template, overwrite=TRUE)
