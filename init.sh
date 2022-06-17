#!/bin/bash
set -e

mach=generic_intel


usage()
{
  echo "Usage: $0 -m | --machine <machine_name>
                [ --fixdir <path to FIX file directory> ]
        "
  exit 2
}


VALID_ARGS=$(getopt -o m: --long machine:,fixdir: -- "$@")
if [[ $? -ne 0 ]]; then
    usage;
fi

eval set -- "$VALID_ARGS"
while [ : ]; do
  case "$1" in
    -m | --machine)
        mach=$2
        shift 2
        ;;
    --fixdir)
        FIXDIR=$2
        shift 2
        ;;
    --) shift; 
        break 
        ;;
  esac
done

if [ -f .env ]; then
	echo ".env file already exist!"
	exit
fi

rootdir=$(pwd)

avmach=''
for f in bin/env.*; do
	mc=$(echo $f | sed 's/bin\/env.//g')
	avmach="$avmach $mc"
done

if [[ "$mach" != "none" ]] && [[ "$avmach" == *"$mach"* ]]; then
	echo "export MACH=$mach" >> .env
	echo "Setting Machine as : $mach"
else
	echo "Specify a machine using the option -m"
	echo "For eg. $0 -m machine_name"
	echo 
	echo "Available machines are: " $avmach
	exit
fi

	
echo "export rootdir=$rootdir" >> .env

echo "export FIXDIR=$FIXDIR   # path to fix directory" >> .env

cat .env
