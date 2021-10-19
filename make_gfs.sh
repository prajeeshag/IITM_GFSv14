
#!/bin/bash

set -e

usage (){
	echo
	echo $0 -j npes 
	echo
	echo options:
	echo -j npes : parallel gmake with npes	
	exit 1;
}


debug=""
npes=1
workdir='none'

if [[ -z "$@" ]]; then
	usage
fi

while getopts 'cdij:w:' flag; do
    case "${flag}" in
    j) npes=$OPTARG ;;
	*) usage ;;
    esac
done

shift $(($OPTIND - 1))

opts=$@

echo '...............Setting up environment.....................'
if [ ! -f .env ]; then
	echo ".env file does not exist. Run init.sh first."
	exit
fi
source .env

source $rootdir/bin/env.$MACH

EXE="gfs.exe"

EXECDIR="$rootdir/exec"
SRCDIR="$rootdir/src"
MKMF="$rootdir/bin/mkmf"
MKMFTEMPLATE="$rootdir/bin/mkmf.template.$MACH$debug"

INCLUDES="$SRCDIR/gfs/includes/"
incs=" "
libs=" "

libname='lib_share.a'
libsrc="gfs/GSM/share"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_gsmphys.a'
libsrc="gfs/GSM/gsmphys"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_util.a'
libsrc="gfs/GSM/libutil"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_post_stub.a'
libsrc="gfs/GSM/post_stub"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs  -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_io.a'
libsrc="gfs/GSM/io"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs " $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_sigio.a'
libsrc="gfs/GSM/sigio"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs " $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_fms.a'
libsrc="fms/"
builddir=$EXECDIR/$libsrc
fmsroot=$SRCDIR/$libsrc
paths="$fmsroot/mpp $fmsroot/include \ 
		$fmsroot/mpp/include \
		$fmsroot/fms $fmsroot/platform \
		$fmsroot/memutils $fmsroot/constants \
		$fmsroot/horiz_interp $fmsroot/mosaic \
		$fmsroot/diag_manager $fmsroot/time_manager \
		$fmsroot/gfs_interp"
cppDef="-Duse_netCDF -Duse_libMPI -DENABLE_ODA -Dfms_interp"
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_dyn.a'
libsrc="gfs/GSM/dyn"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs  -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_phys.a'
libsrc="gfs/GSM/phys"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_gsm.a'
libsrc="gfs/GSM/gsm"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

exename='gfs.exe'
libsrc="gfs/GSM/nems"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo '...............Compiling GFS.....................'
$MKMF -f -p $exename -t $MKMFTEMPLATE -o "$incs" -l "$libs" $paths $INCLUDES
make -j $npes 
echo '...............Done Compiling GFS.....................'


# echo "#-------------------------MAKE RUN_NCCOMBINEP2R--------------------------------------"
# cppDef="-Dlib_mppnccp2r -Duse_libMPI"
# exe=run_mppnccp2r
# paths="$rootdir/src/postproc/mppnccombinep2r"
# export LD=$FC
# mkdir -p $execdir/$exe
# cd $execdir/$exe

# OPTS="-I$execdir/lib_fms"

# LIBS="$execdir/lib_fms/lib_fms.a"

# $mkmf -c "$cppDef" -f -p ${exe} -t $mkmftemplate -o "$OPTS" -l "$LIBS"  $paths
# make -j $npes
# echo "#--------------------------------------------------------------------------------"

# filestocopy="data_table diag_table input.nml run_mppnccombine.sh odtm_submit.pbs"

# if [ "$workdir" != "none" ]; then
# 	wrkdir="$rootdir/work/$workdir"
# 	if [ -d "$wrkdir" ]; then
# 		echo "Work directory $wrkdir already exist!!"
# 		exit
# 	fi
# 	mkdir -p $wrkdir
#   mkdir -p $wrkdir/INPUT 
#   mkdir -p $wrkdir/RESTART
#   mkdir -p $wrkdir/OUTPUT

# 	for f in $filestocopy; do
# 		cp $rootdir/scripts/$f $wrkdir/
# 		sed -i "s|_ROOTDIR_|$rootdir|g" $wrkdir/$f
# 		sed -i "s|_EXPNAME_|$workdir|g" $wrkdir/$f
# 		echo "Copying $f ..."
#   	done

# 	echo 
# 	echo "Experiment directory is created: $wrkdir"
# 	echo
# fi
