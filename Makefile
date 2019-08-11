FC=ifort

all:
	make run

libMath.o: libMath.f90
	$(FC) -c libMath.f90

fourPointWing.out: libMath.o fourPointWing.f90
	$(FC) fourPointWing.f90 libMath.o -o fourPointWing.out

run: fourPointWing.out
	./fourPointWing.out

clean:
	rm -f *.out *.o *.mod
