FC=gfortran
CFLAGS=-O3
LDFLAGS=
EXE=rootfind.x
SRC=rootfind.f90 zbrac.f90 zbrent.f90
OBJ=$(SRC:.f90=.o)

all: $(SRC) $(EXE)

$(EXE): $(OBJ)
	$(FC) $(LDFLAGS) $(OBJ) -o $(EXE)


%.o: %.f90 
	$(FC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.mod $(OBJ) $(EXE)
