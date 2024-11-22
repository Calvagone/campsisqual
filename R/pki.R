
#' Get more information on the given certificate.
#' 
#' @param cert certificate
#' @return certificate information
#' @importFrom PKI PKI.get.cert.info
#' @export
getCertificateInformation <- function(cert) {

  certInfo <- PKI::PKI.get.cert.info(cert)
  certInfo$subject <- subjectToNamedVector(certInfo$subject)
  certInfo$issuer <- subjectToNamedVector(certInfo$issuer)
  
  return(certInfo)
}

subjectToNamedVector <- function(x) {
  tmp <- strsplit(x, "/")[[1]]
  tmp <- tmp[tmp != ""]
  tmp <- strsplit(tmp, "=")
  values <- tmp %>% 
    purrr::map(~.x[2])
  names <- tmp %>% 
    purrr::map_chr(~.x[1])
  values <- values %>%
    setNames(names)
  return(values)
}

#'
#' Get the CA certificate.
#' 
#' @return CA certificate, character vector
#' @export
#'
getCACertificate <- function() {
  pem <- "-----BEGIN CERTIFICATE-----
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
SPAi3WOaHObWhtV9Wa9G2OAfs01bMjtCKw==
-----END CERTIFICATE-----
"
  retValue <- strsplit(x=pem, split="\n")[[1]]
  return(retValue)
}

#' With directory
#' 
#' @param dir directory
#' @param expr expression
#' @export
with_dir <- function(dir, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dir)
  evalq(expr)
}

#' Decrypt a file (no prompt).
#'
#' @param file path to file to decrypt.
#' @param private_key path to private key
#' @param passphrase passphrase
#' @importFrom jsonlite read_json
#' @importFrom openssl decrypt_envelope
#' @return nothing
decryptFile <- function(file, private_key, passphrase=NULL) {
  out <- jsonlite::read_json(file)
  out$iv <- hexStringToRaw(out$iv[[1]])
  out$session <- hexStringToRaw(out$session[[1]])
  out$data <- hexStringToRaw(out$data[[1]])
  .file  <-  gsub(".encrypted", "", file)
  zz = file(.file, "wb")
  tmp <- openssl::decrypt_envelope(out$data, out$iv, out$session, key=private_key, password=passphrase) |>
    writeBin(zz)
  close(zz)
}

#' Encrypt a file.
#'
#' @param file path to file to encrypt.
#' @param public_key path to public key
#' @importFrom jsonlite write_json
#' @importFrom rlang raw_deparse_str
#' @return nothing
encryptFile <- function(file, public_key) {
  out <- openssl::encrypt_envelope(file, public_key)
  out$iv <- rlang::raw_deparse_str(out$iv)
  out$session <- rlang::raw_deparse_str(out$session)
  out$data <- rlang::raw_deparse_str(out$data)
  out |>
    jsonlite::write_json(path=file(paste0(file, ".encrypted")))
}


#' Encrypt folder.
#' 
#' @param from original folder
#' @param to destination folder
#' @param public_key path to public key
#' @return nothing
encryptFolder <- function(from, to, public_key) {
  # Copy from original to encrypted folder
  dir.create(to, showWarnings=FALSE)
  file.copy(file.path(from, list.files(from)), to, recursive=TRUE)
  
  files <- list.files(path=to, recursive=TRUE)
  for (i in seq_len(length(files))) {
    tmpDir <- file.path(to, dirname(files[i]))
    file <- files[i]
    filename <- basename(file)
    
    with_dir(tmpDir, {
      encryptFile(file=filename, public_key=public_key)
      unlink(filename)
    })
  }
}

#' Decrypt folder.
#' 
#' @param from original folder
#' @param to destination folder
#' @param private_key path to private key
#' @param passphrase passphrase
#' @return nothing
decryptFolder <- function(from, to, private_key, passphrase) {
  # Copy from original to encrypted folder
  dir.create(to, showWarnings=FALSE)
  file.copy(file.path(from, list.files(from)), to, recursive=TRUE)
  
  files <- list.files(path=to, recursive=TRUE)
  for (i in seq_len(length(files))) {
    tmpDir <- file.path(to, dirname(files[i]))
    file <- files[i]
    filename <- basename(file)
    
    with_dir(tmpDir, {
      decryptFile(file=filename, private_key=private_key, passphrase=passphrase)
      unlink(filename)
    })
  }
}

#' Hex string to raw.
#' 
#' @param x single string
#' @return raw vector
hexStringToRaw <- function(x) {
  return(gsub("(.{2})", "\\1 ", x) |>
           strsplit(" ") |>
           dplyr::first() |>
           strtoi(16L) |>
           as.raw())
}

