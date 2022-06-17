#!/bin/bash
set -e

wgrib2=/home/PANL/gefsopera/bin/wgrib2

for var in $@; do

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
	$wgrib2 $ifile | grep -e "$var:1000 mb:" -e "$var:950 mb" -e "$var:900 mb" -e "$var:850 mb" -e "$var:800 mb" -e "$var:700 mb" -e "$var:600 mb" -e "$var:500 mb" -e "$var:400 mb" -e "$var:300 mb" -e "$var:200 mb" -e "$var:100 mb" | $wgrib2 -i $ifile -grib_out _out.grib2
	cdo -f nc -settaxis,$datestampc -remapbil,r720x360 _out.grib2 ${var}_${datestamp}.nc
done

cdo -mergetime ${var}_*.nc ${var}.nc
rm -f ${var}_*.nc

done

#$wgrib_path/wgrib2 $grib_input |grep "PRATE"|  $wgrib_path/wgrib2 -i $grib_input  -append -grib_out $R1_PATH/$grib_file7

 
