#!/bin/bash
set -e

var=$1

fprefix=${var/_/:}
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
	$WGRIB2 $ifile | grep -e "$fprefix" | $WGRIB2 -i $ifile -grib_out ${var}_${datestamp}.grib2
	$CDO -f nc -settaxis,$datestampc ${var}_${datestamp}.grib2 ${var}_${datestamp}.nc
done

$CDO -mergetime ${var}_*.nc ${var}.nc
rm -f ${var}_*.nc


 
