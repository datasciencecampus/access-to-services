#!/bin/bash
# download otp, gtfs data, osm road network data, build otp graph.
OTP_JAR="otp-1.3.0-shaded.jar" 
OTP="https://repo1.maven.org/maven2/org/opentripplanner/otp/1.3.0/$OTP_JAR"
GTFS="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Cardiff/Mar19/Cardiff_Mar19.zip"
OSM="https://github.com/datasciencecampus/access-to-services/raw/master/propeR/data/osm/Cardiff_osm.osm"
HEAP="4G"
wget $OTP 
mkdir -p graphs/default
cd graphs/default
wget $GTFS 
wget $OSM 
cd ../../
java -Xmx$HEAP -jar $OTP_JAR --build graphs/default
