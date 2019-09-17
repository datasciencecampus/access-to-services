#!/bin/bash
# download otp, gtfs data, osm road network data, build otp graph.
OTP_JAR="otp-1.3.0-shaded.jar"
OSM="wales-latest.osm.bz2"
OTP="https://repo1.maven.org/maven2/org/opentripplanner/otp/1.3.0/$OTP_JAR"
GTFS_wales_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/wales_bus/W_GTFS.zip"
# GTFS_EA_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/EA_GTFS.zip"
# GTFS_EM_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/EM_GTFS.zip"
# GTFS_L_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/L_GTFS.zip"
# GTFS_NE_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/NE_GTFS.zip"
# GTFS_NW_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/NW_GTFS.zip"
# GTFS_SE_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/SE_GTFS.zip"
# GTFS_SW_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/england_bus/SW_GTFS.zip"
# GTFS_WM_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/wales_bus/WM_GTFS.zip"
# GTFS_Y_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/wales_bus/Y_GTFS.zip"
# GTFS_S_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/wales_bus/S_GTFS.zip"
GTFS_ncsd_bus="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/ncsd/NCSD_GTFS.zip"
GTFS_uk_train="https://a2s-gtfs.s3.eu-west-2.amazonaws.com/Mar19/uk_train/train_GTFS.zip"
# OSM_file="http://download.geofabrik.de/europe/$OSM"
OSM_file="http://download.geofabrik.de/europe/great-britain/$OSM"
HEAP="16G"
wget $OTP
mkdir -p graphs/default
cd graphs/default
wget $GTFS_wales_bus
# wget $GTFS_EA_bus
# wget $GTFS_EM_bus
# wget $GTFS_L_bus
# wget $GTFS_NE_bus
# wget $GTFS_NW_bus
# wget $GTFS_SE_bus
# wget $GTFS_SW_bus
# wget $GTFS_WM_bus
# wget $GTFS_Y_bus
# wget $GTFS_S_bus
wget $GTFS_ncsd_bus
wget $GTFS_uk_train
wget $OSM_file
bzip2 -d $OSM
cd ../../
java -Xmx$HEAP -jar $OTP_JAR --build graphs/default
