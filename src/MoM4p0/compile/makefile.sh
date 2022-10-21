#!/bin/bash
# compile mom4ice & coupler
set -x

echo
compiler=${compiler:-intel}

machine=crayxc

export  platform=intel_crayxc
export  name=cfs_ocean_mom4ice
export  curdir=$(pwd)
cd ..
export  root=$(pwd)
export  code_dir=$root/src
export  executable=$root/exec/$name
export  time_stamp=$root/bin/time_stamp.csh
export  mkmfTemplate=$root/bin/mkmf.template.$platform
export  mkmf=$root/bin/mkmf
export  pathnames=$curdir/path_names    # path to file containing list of source paths
export  cppDefs="-DENABLE_GDS -Duse_netCDF -Duse_libMPI"

#--------------------------------------------------------------------------------------------------------
# compile mppnccombine.c, needed only if $npes > 1
#--------------------------------------------------------------------------------------------------------
# setup directory structure
  if [ ! -d $root/exec ] ; then   mkdir  $root/exec ; fi
#--------------------------------------------------------------------------------------------------------
# compile the model code and create executable
if [ ! -s $executable ] ; then
  cd $root/exec
  $mkmf -f -a $code_dir -t $mkmfTemplate -p $executable -c "$cppDefs" $pathnames $root/include $code_dir/shared/mpp/include /usr/local/include

  make $@ -f Makefile
fi
