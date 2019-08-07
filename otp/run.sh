#!/bin/bash
# run otp server
OTP_JAR="otp-1.3.0-shaded.jar" 
HEAP="4G"
java -Xmx$HEAP -jar $OTP_JAR --graphs graphs --router default --server
