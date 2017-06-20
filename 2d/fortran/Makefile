FC=gfortran
CFLAGS=-O3
LDFLAGS=
EXE=newton.x
SRC=newton.f90
OBJ=$(SRC:.f90=.o)

all: $(SRC) $(EXE)

$(EXE): $(OBJ)
	$(FC) $(LDFLAGS) $(OBJ) -o $(EXE)


%.o: %.f90 
	$(FC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.mod $(OBJ) $(EXE)
