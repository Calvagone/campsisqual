# campsisqual 1.4.0

* Rework model qualification procedure #15
* Option to skip Python tests in Campsis qualification #16

# campsisqual 1.3.3

* Review method 'setDefaultObsCmt' #12

# campsisqual 1.3.2

* Parallelise tests only when cpu >1 #10

# campsisqual 1.3.1

* Select different Rmd template for qualification of e-Campsis #7
* Add argument 'skip_vdiffr' #8

# campsisqual 1.3.0

* Campsisqual now public on GitHub #1
* Configure continuous integration with GitHub actions #2
* Check tinytex installation is working with a minimalist example #3
* Check simulation engines are installed before the qualification #4
* Add GPL license
* Make the qualification suite external to campsisqual
* Remove digital signature section
* Create NEWS file
* Add package overview section
* Review some of the test names
* Get rid of 'campsistrans' dependency
* Get rid of 'calvamod' dependency
* Do not show skipped tests in test results
* Parallelise the testing of packages in runQualificationCore
* Campsis suite qualification: make sure package was installed with tests
* Better deal with the generated warnings during the qualification of the Campsis suite
* Installation qualification suite for pro users

# campsisqual 1.2.0

* Embed Campsis qualification package into campsisqual
* Better deal with the generated warnings during the qualification of the Campsis suite
* Campsis suite qualification: make sure package was installed with tests

# campsisqual 1.1.0

* Improve areEqual method (for troubleshooting)
* Review model suite qualification
* Update github actions workflow

# campsisqual 1.0.0

* Update package due to changes in Campsis

# campsisqual 0.9.0

* Release of campsisqual 0.9.0

# campsisqual 0.8.1

* Update qualification suite on calvamod models

# campsisqual 0.8.0

* Include all models from Calvamod in the qualification suite.

# campsisqual 0.7.0

* Align package with Pharmpy version 0.45.0
* Use the qualify method with a NONMEM dataset
* Add tolerance argument to method qualify
* ORIGINAL_ID accessed from Campsis simulation output
* Configure continuous integration (CI)
