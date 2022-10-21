#!/bin/bash

set -e

usage (){
	echo
	echo $0 -j npes 
	echo
	echo options:
	echo -j npes : parallel gmake with npes	
	echo -o exec_type : Executable type [real or coupled or hs_forcing] \(default real\)
	exit 1;
}


debug=""
npes=8
type='real'

while getopts 'j:o:' flag; do
    case "${flag}" in
    j) npes=$OPTARG ;;
	o) type=$OPTARG ;;
	*) usage ;;
    esac
done

shift $(($OPTIND - 1))

opts=$@

case "${type}" in
	hs_forcing) typedir="HS";  cppDefextra="-DHS_Forcing" ;;
	coupled) typedir="coupled"; cppDefextra="-DGFS_MOM_COUPLED" ;;
	real) typedir="real"; cppDefextra="" ;;
	*) usage ;;
esac

# cppDefextra=$cppDefextra" -DNO_Nems_IO"

echo '...............Setting up environment.....................'
if [ ! -f .env ]; then
	echo ".env file does not exist. Run init.sh first."
	exit
fi

source .env

source $rootdir/bin/env.$MACH

EXE="gfs.exe"
EXECDIR="$rootdir/exec"
TYPEEXECDIR="$EXECDIR/$typedir"
SRCDIR="$rootdir/src"
MKMF="$rootdir/bin/mkmf"
MKMFTEMPLATE="$rootdir/bin/mkmf.template.$MACH$debug"
MKMFTEMPLATE_chres="$rootdir/bin/mkmf.template_chres.$MACH$debug"
MKMFTEMPLATE_post="$rootdir/bin/mkmf.template_nceppost.$MACH$debug"


INCLUDES="$SRCDIR/gfs/includes/"
incs=" "
libs=" "

cppDef="-Duse_netCDF -Duse_libMPI -DENABLE_ODA -Dfms_interp "$cppDefextra

libname='lib_share.a'
libsrc="gfs/GSM/share"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs" $paths $INCLUDES
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
		$fmsroot/gfs_diag_manager"
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8 " $paths
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_gfs_mom_comm.a'
libsrc="gfs_mom_comm"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_gsmphys.a'
libsrc="gfs/GSM/gsmphys"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_util.a'
libsrc="gfs/GSM/libutil"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_post_stub.a'
libsrc="gfs/GSM/post_stub"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs  -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_io.a'
libsrc="gfs/GSM/io"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs " $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_sigio.a'
libsrc="gfs/GSM/sigio"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs " $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs


libname='lib_dyn.a'
libsrc="gfs/GSM/dyn"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_phys.a'
libsrc="gfs/GSM/phys"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

libname='lib_gsm.a'
libsrc="gfs/GSM/gsm"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo "...............Compiling $libname.........................."
$MKMF -f -c "$cppDef" -p $libname -t $MKMFTEMPLATE -o "$incs -r8" $paths $INCLUDES
gmake -j $npes
echo "...............Done compiling $libname....................."
incs=$incs"-I$builddir "
libs="${builddir}/$libname "$libs

exename='gfs.exe'
libsrc="gfs/GSM/nems"
builddir=$TYPEEXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
echo '...............Compiling GFS.....................'
$MKMF -f -c "$cppDef" -p $exename -t $MKMFTEMPLATE -o "$incs" -l "$libs" $paths $INCLUDES
make -j $npes 
echo '...............Done Compiling GFS.....................'

exename='mppncc_gfs'
libsrc="mppncc_gfs"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
cppDef="-Dlib_mppnccp2r -Duse_libMPI"
echo "...............Compiling $exename....................."
$MKMF -c "$cppDef" -f -p $exename -t $MKMFTEMPLATE -o "$incs" -l "$libs" $paths $INCLUDES
make -j $npes 
echo "...............Done Compiling $exename....................."


exename='mtn_oro'
libsrc="preprocessing/mtn_oro"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
cppDef=" -Duse_libMPI"
echo "...............Compiling $exename....................."
$MKMF -c "$cppDef" -f -p $exename -t $MKMFTEMPLATE -o "$incs -r8" -l "$libs" $paths $INCLUDES
make -j $npes 
echo "...............Done Compiling $exename....................."


exename='chgres'
libsrc="preprocessing/chgres/"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
cppDef=" -Duse_libMPI"
echo "...............Compiling $exename....................."
$MKMF -c "$cppDef" -f -p $exename -t $MKMFTEMPLATE_chres -o "$incs" -l "$libs" $paths $INCLUDES
make -j $npes 
echo "...............Done Compiling $exename....................."


exename='ncep_post'
libsrc="postprocessing/ncep_post/src"
builddir=$EXECDIR/$libsrc
paths=$SRCDIR/$libsrc
lib=$builddir/$libname
mkdir -p $builddir
cd $builddir
cppDef=""
echo "...............Compiling $exename....................."
$MKMF -c "$cppDef" -f -p $exename -t $MKMFTEMPLATE_post $paths 
make -j $npes 
echo "...............Done Compiling $exename....................."