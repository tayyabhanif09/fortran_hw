FC = gfortran
FFLAGS = -Wall -Wextra -g -fbounds-check -fimplicit-none
all : main.o $(ALL_objs)
	$(FC) -g -o bleh main.o $(ALL_objs)

ALL_objs := types.o sort_numbers.o

types.o: types.f90
	$(FC) $(FFLAGS) -c types.f90

sort_numbers.o: types.o sort_numbers.f90
	$(FC) $(FFLAGS) -c sort_numbers.f90

main.o: $(ALL_objs) main.f90
	$(FC) $(FFLAGS) -c main.f90


# %.o : %.mod

clean:
	rm -rf *.o *.mod

tar:
	tar -czf arrays-'date -I'.tgz *.90 makefile
