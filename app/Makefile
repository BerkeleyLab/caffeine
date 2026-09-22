# See 'make all' for instructions

# overridable settings
NUM_IMAGES?=4
FLAGS=
NATIVE_FLAGS=$$($(MAKE) --no-print-directory print-flags FC="$$FC")
GFORTRAN=gfortran
FC_FORCE=
PKG_CONFIG=pkg-config
CAFFEINE_PKG=caffeine
RUN_FPM_SH=../run-fpm.sh

# ----------------------------------------------------------------

SHELL = bash

all: force
	@echo "------------------------------------------------------------------------------"
	@echo
	@echo "This Makefile builds the native-multi-image smoke test that exercises"
	@echo "native compiler support for multi-image features in Fortran."
	@echo
	@echo "The test is automatically adjusted to the known/expected feature support"
	@echo "based on detected Fortran compiler version."
	@echo "In all cases the Makefile attempts to choose appropriate compiler flags,"
	@echo "but these and other settings can be overridden (see top of Makefile)."
	@echo "Execution commands for the built test are best-effort and might need"
	@echo "adjustment based on your particular platform."
	@echo
	@echo "Make Targets:"
	@echo
	@echo "- 'make gcc' | 'make cce' | 'make intel' | 'make nag' :"
	@echo "  Build the test using commands appropriate for the selected compiler family,"
	@echo "  to evaluate built-in multi-image feature support. The FC_FORCE variable"
	@echo "  can optionally be set to override the compiler command name."
	@echo
	@echo "- 'make prif' :"
	@echo "  Build the test and link the Caffeine library to provide multi-image feature"
	@echo "  support through PRIF. This uses pkg-config to find Caffeine and may require setting:"
	@echo "    PKG_CONFIG_PATH=<Caffeine_prefix>/lib/pkgconfig"
	@echo "  Alternatively within an FPM-driven Caffeine build tree you simply can run:"
	@echo "    ./run-fpm.sh run"
	@echo
	@echo "------------------------------------------------------------------------------"


PRINT_FLAGS_STAND_ALONE?=-s
print-flags: force
	@$$FC $(FLAGS) print-native-flags.F90 -o print-native-flags
	@./print-native-flags $(PRINT_FLAGS_STAND_ALONE)
	@rm -f print-native-flags


prif: force
	@export PRINT_FLAGS_STAND_ALONE= ; \
	if [[ -r $(RUN_FPM_SH) ]] ; then \
	  FPM_PREFIX=$$(grep PREFIX= $(RUN_FPM_SH) | cut -d= -f2) ; \
	  [[ -n $$FPM_PREFIX ]] && export PKG_CONFIG_PATH+=:$$FPM_PREFIX/lib/pkgconfig ; \
	fi ; $(MAKE) prif-doit

prif-doit: force
	$(PKG_CONFIG) --modversion $(CAFFEINE_PKG) >/dev/null
	@( $(PKG_CONFIG) --path $(CAFFEINE_PKG) 2>/dev/null || exit 0 )
	@export FC=$(if $(FC_FORCE),$(FC_FORCE),$$($(PKG_CONFIG) --variable=CAFFEINE_FC $(CAFFEINE_PKG))) ; \
	if $$FC --version 2>&1 | grep -q LFortran ; then FC+=" --cpp" ; fi ; \
	set -x ; export NATIVE_FLAGS="$(NATIVE_FLAGS)" && \
	$$FC $$NATIVE_FLAGS native-multi-image.F90 -o native-multi-image-prif \
	    $$($(PKG_CONFIG) --libs-only-L $(CAFFEINE_PKG)) \
	    $$($(PKG_CONFIG) --libs-only-other $(CAFFEINE_PKG) | awk -v RS=' ' '/-Wl,-rpath/') \
	    $$($(PKG_CONFIG) --libs-only-l $(CAFFEINE_PKG)) \
	    $(FLAGS)
	@export CAF_IMAGES=$(NUM_IMAGES) ; set -x ; eval $$($(PKG_CONFIG) --variable=CAFFEINE_RUNCMD $(CAFFEINE_PKG)) ./native-multi-image-prif


gcc: force
	@set -x ; \
	export FC=$(if $(FC_FORCE),$(FC_FORCE),$(GFORTRAN)) ; \
	export NATIVE_FLAGS="$(NATIVE_FLAGS)" && \
	( [[ -n $$NATIVE_FLAGS ]] || \
	  ( echo ERROR: Currently requires GFortran 16 or later. ; exit 1 ) ) && \
	$$FC -g -fcoarray=lib $$NATIVE_FLAGS $(FLAGS) \
	    native-multi-image.F90 -o native-multi-image-gcc -lcaf_shmem
	env GFORTRAN_NUM_IMAGES=$(NUM_IMAGES) ./native-multi-image-gcc

nag: force
	@set -x ; \
	export FC=$(if $(FC_FORCE),$(FC_FORCE),nagfor) ; \
	export NATIVE_FLAGS="$(NATIVE_FLAGS)" && \
	$(GFORTRAN) -E -DNAGFOR $$NATIVE_FLAGS $(FLAGS) \
	    native-multi-image.F90 > native-multi-image.i.F90 && \
	$$FC -u -g -coarray native-multi-image.i.F90 -o native-multi-image-nag
	@rm -f native-multi-image.i.F90
	env NAGFORTRAN_NUM_IMAGES=$(NUM_IMAGES) ./native-multi-image-nag

cce: force
	@set -x ; \
	export FC=$(if $(FC_FORCE),$(FC_FORCE),ftn) ; \
	export NATIVE_FLAGS="$(NATIVE_FLAGS)" && \
	$$FC -g -hcaf -O0 $$NATIVE_FLAGS $(FLAGS) \
	    native-multi-image.F90 -o native-multi-image-cce
	./native-multi-image-cce
	srun -n $(NUM_IMAGES) ./native-multi-image-cce

intel: force
	@set -x ; \
	export FC=$(if $(FC_FORCE),$(FC_FORCE),ifx) ; \
	export NATIVE_FLAGS="$(NATIVE_FLAGS)" && \
	$$FC -g -coarray -O0 $$NATIVE_FLAGS $(FLAGS) \
	    native-multi-image.F90 -o native-multi-image-intel
	env FOR_COARRAY_NUM_IMAGES=$(NUM_IMAGES) ./native-multi-image-intel

clean:
	rm -f *.mod *.i.F90 native-multi-image-{gcc,nag,cce,intel,prif}

force:

.PHONY: force clean
