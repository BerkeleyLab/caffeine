gasnet_prefix=/Users/rouson/.local

include $(gasnet_prefix)/include/smp-conduit/smp-seq.mak

testhello: main.o testhello.o
	gfortran $(GASNET_LDFLAGS) $(GASNET_LIBS) main.o testhello.o -o testhello

main.o: main.f90
	$(FC) -c -o main.o main.f90
	
testhello.o: testhello.c
	$(GASNET_CC) $(GASNET_CPPFLAGS) $(GASNET_CFLAGS) -c testhello.c

.PHONY: clean
clean:
	rm -f testhello *.o
