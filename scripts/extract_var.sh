#!/bin/bash
set -e

wgrib2=/home/PANL/gefsopera/bin/wgrib2

for var in $@; do

if [ -z "$var" ]; then
	exit 1
fi

idatestamp=9999999999999999
for ifile in pgbf_*; do
	datestamp=${ifile:5:10}
	if (( $datestamp < $idatestamp )); then
		idatestamp=$datestamp
	fi
	yy=${datestamp:0:4}
	mm=${datestamp:4:2}
	dd=${datestamp:6:2}
	hh=${datestamp:8:2}
	echo $datestamp $yy $mm $dd $hh
	datestampc=${yy}-${mm}-${dd},${hh}:00,1hour
	$wgrib2 $ifile |grep "$var"|  $wgrib2 -i $ifile  -grib_out _out.grib2
	cdo -f nc -settaxis,$datestampc _out.grib2 ${var}_${datestamp}.nc
done

cdo -mergetime ${var}_*.nc ${var}.nc
rm -f ${var}_*.nc

done

