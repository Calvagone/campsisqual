
# campsisqual

A library dedicate to the qualification of the Campsis Suite and to the
qualification of Campsis models, provided you have the `campsistrans`
package.

## Installation

Install the latest release from GitHub:

``` r
remotes::install_github("Calvagone/campsisqual")
```

## Qualify the Campsis Suite

``` r
library(campsisqual)
credentials <- Credentials(cert = "<PATH_TO_CERTIFICATE>",
                           key = "<PATH_TO_PRIVATE_KEY>",
                           passphrase = "<PASSPHRASE>")

suite <- QualificationSuite(path = "qualification_suite_calvagone.zip",
                            credentials = credentials)

runQualification(
  packages = c(
    "campsismod",
    "campsis",
    "campsisnca",
    "campsismisc",
    "campsisqual"
  ),
  fullname = "<YOUR_NAME>",
  qualification_suite = suite
)
```

## Qualify a model imported from NONMEM with campsistrans

First import the `campsisqual` package and its dependencies:

``` r
library(campsis)
library(campsistrans)
library(campsisqual)
```

Import your NONMEM model using `campsistrans`:

``` r
object <- importNONMEM(campsistrans::getNONMEMModelTemplate(4, 4))
```

Convert the imported NONMEM model to a Campsis model:

``` r
model <- object %>% export(dest="campsis")
```

Create a dataset using Campsis. For instance, inject a bolus of 1000 mg
into the absorption compartment.

``` r
dataset <- Dataset(1) %>%
  add(Bolus(time=0, amount=1000, compartment=1)) %>%
  add(Observations(times=seq(1, 24, by=1), compartment=2))
```

Disable inter-individual variability (IIV) and residual unexplained
variability (RUV).

``` r
model <- model %>% disable(c("IIV", "RUV"))
```

Qualify Campsis model implementation against NONMEM.

``` r
  summary <- qualify(
      x = object,
      model = model,
      dataset = dataset,
      variables = "CONC",
      outputFolder = "readme_tmp",
      reexecuteNONMEM = TRUE)
```

Check if qualification passed.

``` r
  summary %>% passed()
```

Check qualification plot and table.

``` r
  summary %>% getPlot(id=1, variable="CONC")
```

``` r
  summary %>% getTable(id=1)
```
