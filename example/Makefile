gasnet_prefix=/Users/rouson/.local

include $(gasnet_prefix)/include/smp-conduit/smp-seq.mak

testhello: main.o testhello.a
	gfortran $(GASNET_LDFLAGS) $(GASNET_LIBS)  main.o -L/Users/rouson/Repositories/berkeleylab/caffeine/example -ltesthello -o testhello

main.o: main.f90
	$(FC) -c -o main.o main.f90
	
testhello.o: ../src/caffeine/testhello.c
	$(GASNET_CC) $(GASNET_CPPFLAGS) $(GASNET_CFLAGS) -c ../src/caffeine/testhello.c

testhello.a: testhello.o
	ar cr libtesthello.a testhello.o

.PHONY: clean
clean:
	rm -f testhello *.o *.a
