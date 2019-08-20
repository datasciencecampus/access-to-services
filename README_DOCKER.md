# Getting started with Docker

For convenience we have created [Docker](https://www.docker.com/) images for
the OTP server and R package components of this project.

The propeR R package can be built from the parent directory as follows:

```
docker build . --tag=dsc_proper
```

See [Dockerfile](Dockerfile) for more info + deps.


A stand-alone OTP server can be built deployed in the [otp/](otp/) directory.

## docker.io

Our images can also be obtained directly from [docker.io](https://docker.io).

* [datasciencecampus/repository/list](https://cloud.docker.com/u/datasciencecampus/repository/list).

## Running

### OTP

First fire up OTP server (parse `-d` flag to daemonise).

```
docker run -p 8080:8080 datasciencecampus/dsc_otp:1.0
```

### propeR

Can run propeR from within R session, or alternatively can run our docker
image.

Put source and destination `.csv` data in a directory, e.g., `/tmp/data/`.
Example data can be found in `propeR/inst/extdata/`. then:

```
docker run -v /tmp/data:/mnt datasciencecampus/dsc_proper:1.0 'otp.host="192.168.0.2", fun="pointToPoint", src_file="/mnt/origin.csv", dst_file="/mnt/destination.csv", output.dir="/mnt", startDateAndTime="2019-08-02 12:00:00"'
```

Output data will be in `data/`.
