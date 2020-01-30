FC := gfortran
FFLAGS := -cpp -fdec -ffree-line-length-512 -D UNIX

MAIN := ops_main.f90
MODULES := $(basename $(shell ls m_*.f90)) ops_print_table

EXEC := $(basename $(MAIN))
MODS := $(addsuffix .mod, $(MODULES))
OOUT := $(addsuffix .o, $(MODULES))

OBJECTS := $(basename $(filter-out $(MAIN), $(shell ls *.f90)))
OBJS := $(filter-out $(MODS), $(addsuffix .o, $(OBJECTS)))

.SECONDEXPANSION:


all: $(EXEC)

m_error.mod: m_error.f90 m_commonconst.mod
	$(FC) $(FFLAGS) -c $<

m_utils.mod: m_utils.f90 m_string.mod
	$(FC) $(FFLAGS) -c $<

m_fileutils.mod: m_fileutils.f90 m_utils.mod
	$(FC) $(FFLAGS) -c $<

m_aps.mod: m_aps.f90 m_error.mod m_fileutils.mod
	$(FC) $(FFLAGS) -c $<

$(EXEC): $(MAIN) $(OOUT) $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^

%.mod: %.f90
	$(FC) $(FFLAGS) -c $<

%.o: %.f90 m_aps.mod
	$(FC) $(FFLAGS) -o $@ -c $<

clean:
	rm -f $(MODS) $(OOUT) $(OBJS)

distclean: clean
	rm -f $(EXEC)
