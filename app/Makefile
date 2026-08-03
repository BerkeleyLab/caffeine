# This Makefile is for internal testing use only and not intended for end-users
# To run this test with Caffeine, please execute `./run-fpm.sh run` in the top-level directory

FLAGS=

COMMON_FLAGS=-DHAVE_MULTI_IMAGE -DTYPES_PRIF_COMPLIANT=0

NAG_FLAGS=$(COMMON_FLAGS) -DHAVE_NOTIFY=0
export NAGFORTRAN_NUM_IMAGES?=4

GCC_FLAGS=$(COMMON_FLAGS) -DHAVE_NOTIFY=0 -DHAVE_IMAGE_INDEX_TEAM_NUMBER=0 -DHAVE_CO_MIN=0 -DHAVE_CO_MAX=0
export GFORTRAN_NUM_IMAGES?=4

GFORTRAN=gfortran

all: gcc

nag:
	$(GFORTRAN) -E $(NAG_FLAGS) $(FLAGS) \
	    native-multi-image.F90 > native-multi-image.i.F90
	nagfor -coarray native-multi-image.i.F90 -o native-multi-image-nag
	rm -f native-multi-image.i.F90
	./native-multi-image-nag

gcc:
	$(GFORTRAN) -fcoarray=lib $(GCC_FLAGS) $(FLAGS) \
	    native-multi-image.F90 -o native-multi-image-gcc -lcaf_shmem
	./native-multi-image-gcc

clean:
	rm -f *.mod *.i.F90 native-multi-image-nag native-multi-image-gcc

.PHONY: nag gcc clean
