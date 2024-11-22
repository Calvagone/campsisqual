library(testthat)

context("Qualification of the Campsis suite requires PKI package to sign the report")

test_that("PKI package works as expected to read certificates", {
  caCert <-  PKI::PKI.load.cert(what=getCACertificate(), format="PEM")
  info <- getCertificateInformation(caCert)
  subj <- list(C="FR", ST="Lyonnais", L="Chazay d'Azergues", O="Calvagone CA", OU="IT", CN="Campsis suite qualification (CA)", emailAddress="campsis@calvagone.com")
  
  expect_equal(info$subject, subj)
  expect_equal(info$issuer, subj)
  expect_equal(info$ca, TRUE)
  expect_equal(format(info$validity, "%Y-%m-%d"), c("2024-11-18", "2029-11-17"))
  
  # paste0("'", info$fingerprint %>% as.character() %>% strsplit(" "), "'", collapse=",")
  fingerprintExpected <- c('01','6b','e0','8d','b6','ae','c2','b1','14','ae','eb','fe','6f','41','4e','ba','ba','7e','e8','13') %>%
    strtoi(base=16L) %>%
    as.raw()
  
  expect_equal(info$fingerprint, fingerprintExpected)
})