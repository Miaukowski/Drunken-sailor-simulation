rm *.o
rm *.mod
rm a.out
rm fort.1

gfortran -c mod1.f90 
gfortran -c mod2.f90 
gfortran -c mod3.f90 
gfortran -c mod4.f90 
gfortran -c mod5.f90 
gfortran -c mod6.f90 
gfortran -c DrunkenSailor.f90 
gfortran DrunkenSailor.o mod6.o mod5.o mod4.o mod3.o mod2.o mod1.o
