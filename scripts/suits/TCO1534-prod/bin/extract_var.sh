#!/bin/bash
set -e

var=$1

fprefix=${var/:/_}
fprefix=${fprefix/ /}

if [ -z "$var" ]; then
	exit 1
fi

for ifile in pgbf_*; do
	datestamp=${ifile:5:10}
	yy=${datestamp:0:4}
	mm=${datestamp:4:2}
	dd=${datestamp:6:2}
	hh=${datestamp:8:2}
	echo $datestamp $yy $mm $dd $hh
	datestampc=${yy}-${mm}-${dd},${hh}:00,1hour
	$WGRIB2 $ifile | grep -e "$var" | $WGRIB2 -i $ifile -grib_out _out.grib2
	$CDO -f nc -settaxis,$datestampc _out.grib2 ${fprefix}_${datestamp}.nc
done

$CDO -mergetime ${fprefix}_*.nc ${fprefix}.nc
rm -f ${fprefix}_*.nc


 
