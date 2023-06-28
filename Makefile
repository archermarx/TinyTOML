FC=gfortran
FCFLAGS= -g -fbacktrace -fcheck=all -Og
SRC=\
	src/tinytoml.f90\
	test/example.f90


PROGRAM=toml.exe
PRG_OBJ=$(PROGRAM).o

release:
	$(FC) $(FCFLAGS) $(FLFLAGS) -o $(PROGRAM) $(SRC)

clean:
	rm -f $(PROGRAM) *.mod