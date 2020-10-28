# a makefile for GNU Linux installations
# Gerard Cats, 27 July 2020

.SUFFIXES: .f .o

OPS: OPS.exe

# if debugging
# ------------
ifeq ($(MAKECMDGOALS),debug)
   DB  = db
   debug: OPS$(DB).exe
   OPT = -O0
else
   OPT = -O3
endif

# sources are separated into modules and others, but this distinction is not used further on
# -------

MODULES := binas.f90 m_aps.f90 m_commonconst.f90 m_commonfile.f90 m_depac.f90 m_error.f90 m_fileutils.f90 m_geoutils.f90 \
           m_getkey.f90 m_ops_building.f90 m_ops_emis.f90 m_ops_plumerise.f90 m_ops_utils.f90 \
           m_ops_vchem.f90 m_string.f90 m_utils.f90  \
           ops_bgcon.f90 ops_print_table.f90

SOURCES := ops_bgcon_tra.f90 ops_bron_rek.f90 ops_brondepl.f90 ops_calc_stats.f90 ops_conc_ini.f90 \
           ops_conc_rek.f90 ops_conltexp.f90 ops_convec.f90 ops_depoparexp.f90 ops_depos_rc.f90 ops_depu.f90 \
           ops_gen_fnames.f90 ops_gen_precip.f90 ops_gen_rcp.f90 ops_get_arg.f90 ops_get_dim.f90 ops_getlu.f90 \
           ops_getlu_tra.f90 ops_getz0.f90 ops_getz0_tra.f90 ops_init.f90 ops_logfile.f90 ops_main.f90 ops_neutral.f90 \
           ops_outp_prep.f90 ops_par_chem.f90 ops_plot_uitv.f90 ops_plrise71.f90 ops_print_grid.f90 ops_print_info.f90 \
           ops_print_kop.f90 ops_print_recep.f90 ops_rcp_char_1.f90 ops_rcp_char_all.f90 \
           ops_read_bg.f90 ops_read_ctr.f90 ops_read_emis.f90 ops_read_meteo.f90 ops_read_source.f90 ops_reken.f90 \
           ops_resist_rek.f90 ops_scalefac.f90 ops_seccmp.f90 ops_src_char.f90 ops_stab_rek.f90 ops_statparexp.f90 \
           ops_surface.f90 ops_tra_char.f90 ops_vertdisp.f90 ops_virtdist.f90 ops_write_progress.f90 ops_wv_powerlaw.f90 \
           ops_wvprofile.f90 ops_z0corr.f90

#
# r1mach was added for proper behaviour on double precision.
#
SOURCES  := $(SOURCES) r1mach.f90
#
# Files to be added to the download from math77, such as found on
# https://netlib.org/math/math77.tgz
#
SOURCESmath77 = mess.f silup.f silupm.f optchk.f  smess.f


# configurations
# --------------
ifeq ($(MAKECONF),GNU_Linux)
   SOURCES := $(SOURCES) inum.f90		# no IFPORT library
   FC       = gfortran
   CPPFLAGS = -DUNIX -DGNU
   FFLAGSb  = $(OPT) -ffree-line-length-0 -finit-local-zero -cpp
   ifeq ($(MAKECMDGOALS),OPS8)
      FFLAGSb += -fdefault-real-8
   endif
   FFLAGS   = $(FFLAGSb)

# optimisation problems
   ops_statparexp.o : FFLAGS = $(FFLAGSb:O3=O0)

#  LDLIBS   = -lMATH77
   LDLIBS   = 
   LDFLAGS  = -L /usr/local/lib
else
   FC       = ifort
   CPPFLAGS = -DUNIX -fpp
   ifeq ($(MAKECMDGOALS),debug)
      FFLAGS = -nowarn -fpp -assume byterecl -check bounds -gen-interfaces -warn interfaces -debug-parameters all \
               -traceback -O0 -g -fpe0 -extend_source
   else
      FFLAGS = -nowarn -fpp -assume byterecl -O2 -extend_source
   endif
endif
#_______________________________________________________________________________

# double precision, to be installed immediately
OPS8: OPS.exe
	mv $< $@
	cp $@ $(MYPATH)/

#_______________________________________________________________________________

# generics
# --------
OBJECTS := $(MODULES:.f90=.o) $(SOURCES:.f90=.o) $(SOURCESmath77:.f=.o)

OPS$(DB).exe: $(OBJECTS)
	$(FC) $(LDFLAGS) $(OBJECTS) $(LDLIBS) -o $@

%.o %.mod: %.f90
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $<

# dependencies list is created from sources
# ------------
dependencies: $(MODULES) $(SOURCES) makedependencies.pl
	perl makedependencies.pl $(MODULES) $(SOURCES) > $@

ifneq ($(MAKECMDGOALS),clean)
   include dependencies
endif

.PHONY: clean
clean:
	@echo removing dependencies, .o, .mod and .exe
	$(RM) dependencies OPS.exe OPSdb.exe *.mod *.o

# install: put the executable in your path (default: ~/bin). NB not for the debug version
# -------
ifeq ($(MYPATH),)
    MYPATH := $(HOME)/bin
endif
install: OPS.exe
	cp $< $(MYPATH)
