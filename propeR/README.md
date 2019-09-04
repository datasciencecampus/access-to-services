# propeR

**prope** [latin] _verb_
**Definitions:**
1. near, nearby;
2. almost;
3. close by

<p align="center"><img align="center" src="meta/logo/propeR_logo_v1.png" width="200px"></p>

An R tool for analysing multimodal transport using GTFS data built using [OpenTripPlanner](http://www.opentripplanner.org/).

## R Installation and Build

### Installing

Direct from github repo:

```
library(devtools)
install_github("datasciencecampus/access-to-services/propeR")
```

Or from local:

```
install("propeR_dir")
```

### Building

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

### Using

```
library("propeR")
```

Then follow the steps in the [propeR manual](https://github.com/datasciencecampus/access-to-services/blob/develop/propeR/manual.md) to run the functions.

## Docker Installation and Build

For convenience we have created [Docker](https://www.docker.com/) images for
the example Cardiff OpenTripPlanner (OTP) server and [propeR R package](https://github.com/datasciencecampus/access-to-services/tree/develop/propeR) components of this project.

### Installing

The propeR R package can be built from the parent directory as follows:

```
docker build . --tag=dsc_proper
```

See [Dockerfile](Dockerfile) for more information and dependencies.

A stand-alone OTP server can be built deployed in the [otp/](otp/) directory.

#### docker.io

Our images can also be obtained directly from [docker.io](https://docker.io).

* [datasciencecampus docker](https://hub.docker.com/u/datasciencecampus).

### Building

#### OTP

First fire up OTP server (parse `-d` flag to daemonise).

```
docker run -p 8080:8080 datasciencecampus/dsc_otp:1.0
```

### Using

You can run propeR from within R session (as described in the [propeR manual](https://github.com/datasciencecampus/access-to-services/blob/develop/propeR/manual.md)), or alternatively can run our docker image.

Put source and destination `.csv` data in a directory, e.g., `/tmp/data/`.
Example data files `origin.csv` and `destination.csv` can be found in `propeR/inst/extdata/`, then:

```
docker run -v /tmp/data:/mnt datasciencecampus/dsc_proper:1.0 'otp.host="XXX.XXX.X.X", fun="pointToPoint", src_file="/mnt/origin.csv", dst_file="/mnt/destination.csv", output.dir="/mnt", startDateAndTime="2019-08-02 12:00:00"'
```

where `otp.host` is your inet address, which can be found using:

```
/sbin/ifconfig |grep inet |awk '{print $2}'

```

Output data will be in `tmp/data/`.

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
