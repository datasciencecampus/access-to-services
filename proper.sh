#!/bin/bash
# cli propeR interface
# =============================================================================
# propeR functions
#   * pointToPoint
#   * pointToPointLoop
#   * pointToPointNearest
#   * pointToPointTime
#
#   * isochrone
#   * isochroneTime
#   * isochroneMulti
# =============================================================================
# example:
#
# ./proper.sh 'fun=pointToPoint,src_file="propeR/inst/extdata/origin.csv", dst_file="propeR/inst/extdata/destination.csv", output.dir="/tmp/a2s", startDateAndTime="2019-08-02 12:00:00", mapOutput=T'

# --x=123 --y=321 -> x=123,y=321
# -x=abc --y=heh  -> x=abc,y=heh
args="$(echo $@ |sed 's/ -\+/,/g; s/^-\+//')"

if [[ "$args" != *"fun="* ]] ;then
  echo "missing fun"
  exit 1
fi

echo "$args"

# invoke facade with parsed args
Rscript --vanilla facade.R "$args"
