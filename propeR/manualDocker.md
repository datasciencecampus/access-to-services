# Getting started with Docker

For convenience we have created [Docker](https://www.docker.com/) images for
the example Cardiff OpenTripPlanner (OTP) server and [propeR R package](https://github.com/datasciencecampus/access-to-services/tree/develop/propeR) components of this project.

The propeR R package can be built from the parent directory as follows:

```
docker build . --tag=dsc_proper
```

See [Dockerfile](Dockerfile) for more information and dependencies.

A stand-alone OTP server can be built deployed in the [otp/](otp/) directory.

## docker.io

Our images can also be obtained directly from [docker.io](https://docker.io).

* [datasciencecampus docker](https://hub.docker.com/u/datasciencecampus).

## Running

### OTP

First fire up OTP server (parse `-d` flag to daemonise).

```
docker run -p 8080:8080 datasciencecampus/dsc_otp:1.0
```

### propeR

Can run propeR from within R session (as described in the [main propeR manual](https://github.com/datasciencecampus/access-to-services/blob/develop/propeR/manual.md)), or alternatively can run our docker image.

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
