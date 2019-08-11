FC=ifort

all:
	make run

run:
	$(FC) -c libMath.f90
	$(FC) fourPointWing.f90 libMath.o -o fourPointWing.out
	./fourPointWing.out

clean:
	rm -f *.out *.o *.mod
