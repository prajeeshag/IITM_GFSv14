###################################################################################################
# post implement module load standard
#
# 10/15 Lin Gan:        Create module load version
# 12/07 Lin Gan:        Update to generate post module output
# 07/16 J. Carley:      Generalize for multiple machines
#
###################################################################################################

SHELL=/bin/bash
#
# Define the name of the executable
#
# To generate exe as ncep_post
TARGET        = ncep_post
LIB_TARGET    = libnceppost.a
AR            = ar
ARFLAGS       = ruv

#
# CPP, Compiler, and Linker Options
#
#FC       = ftn
#CC       = cc
#CPP      = cpp -P -traditional 
#OPTS     = -O3 -openmp
#DEBUG    = -g -traceback
#FREE     = -FR

FC       = ftn
CC       = cc
CPP      =  cpp -P -traditional  #/lib/cpp -P -traditional
CPPFLAGS =
CFLAGS   = -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'
OPTS     = -O3
FREE     = -FR
DEBUG    = -O3 -convert big_endian -traceback -g -fp-model source -openmp -g -traceback 

WRFPATH    = /home/apps/WRF/3.6.1/intel/17.0.5/WRFV3
WRF_INC    = -I$(WRFPATH)/external/io_quilt -I$(WRFPATH)/frame
WRF_LIB    = $(WRFPATH)/main/libwrflib.a  \
             $(WRFPATH)/frame/pack_utils.o  \
             $(WRFPATH)/frame/module_internal_header_util.o \
             $(WRFPATH)/external/io_grib1/libio_grib1.a \
             $(WRFPATH)/external/io_grib_share/libio_grib_share.a \
             $(WRFPATH)/external/io_int/libwrfio_int.a \
             $(WRFPATH)/external/io_netcdf/libwrfio_nf.a \
             $(WRFPATH)/external/esmf_time_f90/libesmf_time.a \
             $(WRFPATH)/external/RSL_LITE/librsl_lite.a


NETCDF =  /opt/cray/pe/netcdf/4.4.1.1.3/INTEL/16.0
NETCDF_INC = -I$(NETCDF)/include
NETCDF_LDFLAGS = -L$(NETCDF)/lib -lnetcdf

#CFLAGS   = -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'

NWPROD_LIB = /home/apps/NWPROD/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib
G2TMPL_INC=-I${NWPROD_LIB}/incmod/g2tmpl
G2TMPL_LIB=-L${NWPROD_LIB} -lg2tmpl

SFCIO_INC4  = -I${NWPROD_LIB}/incmod/sfcio_4
SFCIO_LIB4  = ${NWPROD_LIB}/libsfcio_4.a

NEMSIO_INC = -I${NWPROD_LIB}/incmod/nemsio
NEMSIO_LIB = -L${NWPROD_LIB} -lnemsio

BACIO_LIB4  = -L${NWPROD_LIB} -lbacio_4

SIGIO_INC4  = -I${NWPROD_LIB}/incmod/sigio_4
SIGIO_LIB4  = ${NWPROD_LIB}/libsigio_4.a


G2_INC4     = -I${NWPROD_LIB}/incmod/g2_4
G2_LIB4     = -L${NWPROD_LIB} -lg2_4 


GFSIO_INC4  = -I${NWPROD_LIB}/incmod/gfsio_4
GFSIO_LIB4  = ${NWPROD_LIB}/libgfsio_4.a

IP_LIB4     = -L${NWPROD_LIB} -lip_4

SP_LIB4     = -L${NWPROD_LIB} -lsp_4

W3EMC_INC4     = -I${NWPROD_LIB}/incmod/w3emc_4
W3EMC_LIB4     = -L${NWPROD_LIB} -lw3emc_4

W3NCO_LIB4    = -L${NWPROD_LIB} -lw3nco_4

CRTM_INC   = -I${NWPROD_LIB}/incmod/crtm
CRTM_LIB   = -L${NWPROD_LIB} -lCRTM

JASPER_LIB=-L/home/apps/jasper/1.900.1/intel/17.0.5/lib/ -ljasper 
PNG_LIB= -L/home/apps/libpng/1.6.13/intel/17.0.5/lib -lpng 
Z_LIB=-L/home/apps/zlib/1.2.6/intel/17.0.5/lib/ -lz


FFLAGS   = $(OPTS) $(FREE) $(DEBUG) \
           $(SFCIO_INC4) \
           $(NEMSIO_INC) \
           $(WRF_INC) \
           $(SIGIO_INC4) \
           $(G2_INC4) \
           $(G2TMPL_INC) \
           $(GFSIO_INC4) \
           $(W3EMC_INC4) \
           $(CRTM_INC) \
           $(NETCDF_INC)

LIBS     = $(WRF_LIB) \
           $(G2TMPL_LIB) \
           $(G2_LIB4) \
           $(JASPER_LIB) \
           $(PNG_LIB) \
           $(Z_LIB) \
           $(NEMSIO_LIB) \
           $(GFSIO_LIB4) \
           $(SIGIO_LIB4) \
           $(SFCIO_LIB4) \
           $(IP_LIB4) \
           $(SP_LIB4) \
           $(W3EMC_LIB4) \
           $(W3NCO_LIB4) \
           $(BACIO_LIB4) \
           $(CRTM_LIB) \
           $(NETCDF_LDFLAGS)


OBJS =  wrf_io_flags.o getVariable.o getIVariable.o getIVariableN.o \
        kinds_mod.o machine.o physcons.o \
	native_endianness.o blockIO.o initialize_byte_swap_wrf_binary_file.o \
        count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
        next_buf.o retrieve_index.o ZENSUN.o CLDFRAC_ZHAO.o \
        GFSPOST.o GETGBANDSCATTER.o \
        VRBLS2D_mod.o VRBLS3D_mod.o VRBLS4D_mod.o MASKS_mod.o PMICRPH.o SOIL_mod.o \
        CMASSI.o CTLBLK.o GRIDSPEC.o LOOKUP.o PARAMR.o RHGRD.o RQSTFLD.o xml_perl_data.o \
        cuparm.o params.o svptbl.o get_postfilename.o grib2_module.o \
        SET_LVLSXML.o FILL_PSETFLD.o \
        BNDLYR.o BOUND.o CALCAPE.o CALDWP.o CALDRG.o CALHEL.o CALLCL.o \
        CALMCVG.o CALPOT.o CALPW.o CALRH.o CALRCH.o CALRH_GSD.o \
        CALSTRM.o CALTAU.o CALTHTE.o CALVIS.o CALVIS_GSD.o CALVOR.o CALWXT.o \
        CALWXT_RAMER.o CALWXT_BOURG.o CALWXT_REVISED.o CALRH_PW.o \
        CALWXT_EXPLICIT.o CALWXT_DOMINANT.o \
        CLDRAD.o CLMAX.o COLLECT.o COLLECT_LOC.o DEWPOINT.o \
        FDLVL.o FGAMMA.o FIXED.o FRZLVL.o FRZLVL2.o \
        GET_BITS.o GRIBIT.o INITPOST.o LFMFLD.o \
        MAPSSLP.o MISCLN.o MIXLEN.o MDL2P.o MDLFLD.o MPI_FIRST.o MPI_LAST.o \
        NGMFLD.o NGMSLP.o OTLFT.o OTLIFT.o SLP_new.o SLP_NMM.o EXCH.o \
        PARA_RANGE.o PROCESS.o INITPOST_NMM.o EXCH2.o \
        READCNTRL.o READ_xml.o SET_OUTFLDS.o SCLFLD.o SERVER.o SETUP_SERVERS.o \
        SMOOTH.o SURFCE.o \
        SPLINE.o TABLE.o TABLEQ.o TRPAUS.o TTBLEX.o WETBULB.o WRFPOST.o \
        CALMICT.o MICROINIT.o GPVS.o MDL2SIGMA.o \
        ETCALC.o CANRES.o CALGUST.o WETFRZLVL.o SNFRAC.o MDL2AGL.o SNFRAC_GFS.o \
        AVIATION.o DEALLOCATE.o INITPOST_NMM_BIN_MPIIO.o \
        CALPBL.o MDL2SIGMA2.o INITPOST_GFS.o CALRH_GFS.o LFMFLD_GFS.o \
        CALRAD_WCLOUD_newcrtm.o MDL2THANDPV.o CALPBLREGIME.o POLEAVG.o \
        INITPOST_NEMS.o GETNEMSNDSCATTER.o ICAOHEIGHT.o INITPOST_GFS_NEMS.o \
        INITPOST_BIN_MPIIO.o GEO_ZENITH_ANGLE.o GFIP3.o GRIDAVG.o CALUPDHEL.o INITPOST_GFS_SIGIO.o \
	gtg_config.o gtg_filter.o gtg_trophts.o gtg_itfa.o gtg_indices.o \
        AllGETHERV_GSD.o MSFPS.o SELECT_CHANNELS.o ALLOCATE_ALL.o INITPOST_NEMS_MPIIO.o ASSIGNNEMSIOVAR.o \
        INITPOST_GFS_NEMS_MPIIO.o WRF_STUBS.o 


.SUFFIXES: .F .f .o .f90 .c

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET):	$(OBJST) $(OBJS)
	$(FC) -fopenmp -o $@ $(OBJST) $(OBJS) $(LIBS)
	mkdir -p include/post_4
	$(AR) $(ARFLAGS) $(LIB_TARGET) $(OBJST) $(OBJS)
	mv *.mod include/post_4

.f.o:
	$(FC) $(FFLAGS) -c $<

.f90.o:
	$(FC) $(FFLAGS) -c $<

.c.o :
	${CC} ${CFLAGS} -c $<


clean:
	/bin/rm -f  *.o *.mod libnceppost.a ncep_post 
	/bin/rm -rf include 
