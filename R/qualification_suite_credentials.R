#_______________________________________________________________________________
#----               qualification_suite_credentials class                   ----
#_______________________________________________________________________________

#'
#' Qualification suite credentials class.
#' 
#' @slot certificate_path path to client certificate
#' @slot certificate client certificate, PEM format
#' @slot private_key_path path to private key 
#' @slot passphrase private key passphrase
#' @export
setClass(
  "qualification_suite_credentials",
  representation(
    certificate_path="character",
    certificate="character",
    private_key_path="character",
    passphrase="character"
  )
)

#'
#' Qualification suite credentials.
#' 
#' @param cert path to client certificate
#' @param key path to private key 
#' @param passphrase private key passphrase
#' @return a qualification suite credentials object
#' @export
Credentials <- function(cert, key, passphrase) {
  if (!file.exists(cert)) {
    stop("Client certificate could not be found")
  }
  if (!file.exists(key)) {
    stop("Private key could not be found")
  }
  
  # Load certificate
  certificate <- readLines(con=file(cert))
  pkiClientCertificate <- PKI::PKI.load.cert(what=certificate, format="PEM")
  
  # Load CA certificate
  pkiCACertificate <-  PKI::PKI.load.cert(what=getCACertificate(), format="PEM")
  
  # Check validity
  certificateInfo <- getCertificateInformation(pkiClientCertificate)
  if (Sys.time() > certificateInfo$validity[2]) {
    stop("Client certificate has expired. Please contact Calvagone.")
  }
  
  # Check certificate chain is fine (note that expiration is also checked here)
  isValid <- PKI::PKI.verifyCA(certificate=pkiClientCertificate, ca=pkiCACertificate)  
  if (!isValid) {
    stop("Client certificate is not valid. Please contact Calvagone.")
  }
  
  return(new("qualification_suite_credentials", certificate_path=normalizePath(cert), certificate=certificate,
             private_key_path=normalizePath(key), passphrase=passphrase))
}
