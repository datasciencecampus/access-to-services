# propeR

**prope** [latin] _verb_
**Definitions:**
1. near, nearby;
2. almost;
3. close by

<p align="center"><img align="center" src="meta/logo/propeR_logo_v1.png" width="200px"></p>

An R tool for analysing multimodal transport using GTFS data built using [OpenTripPlanner](http://www.opentripplanner.org/).

## Installing

Direct from github repo:

```
library(devtools)
install_github("datasciencecampus/access-to-services/propeR")
```

Or from local:

```
install("propeR_dir")
```

## Building

Build requires devtools and roxygen2

```
# R
install.packages("devtools")
install.packages("roxygen2")
```

Then

```
build("propeR_dir")
install("propeR_dir")
```

## Using

```
library("propeR")
```

## Acknowledgments

* [TransXChange2GTFS](https://github.com/danbillingsley/TransXChange2GTFS)
* [transxchange2gtfs](https://github.com/planarnetwork/transxchange2gtfs)
* [dtd2mysql](https://github.com/open-track/dtd2mysql)
* [OpenTripPlanner](http://www.opentripplanner.org/)
* functions `otpConnect()`, `otpTripTime()`, `otpTripDistance()`, `otpIsochrone()` are modified from Marcus Young's repo [here](https://github.com/marcusyoung/opentripplanner/blob/master/Rscripts/otp-api-fn.R)

## Contributions and Bug Reports

We welcome contributions and bug reports. Please do this on this repo and we will endeavour to review pull requests and fix bugs in a prompt manner.

## Licence

The Open Government Licence (OGL) Version 3

Copyright (c) 2018 Office of National Statistics

This source code is licensed under the Open Government Licence v3.0. To view this licence, visit [www.nationalarchives.gov.uk/doc/open-government-licence/version/3](www.nationalarchives.gov.uk/doc/open-government-licence/version/3) or write to the Information Policy Team, The National Archives, Kew, Richmond, Surrey, TW9 4DU.
