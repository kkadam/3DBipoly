#compiler
FC=/opt/intel/composerxe-2011.5.209/bin/intel64/ifort

F90FILES=main.F90 guessrho.F90 print2d.F90 setup.F90 potsetup.F90 \
sm.F90 tm.F90 elle.F90 ellf.F90 gammln.F90 rd.F90 rf.F90 potential_solver.F90\
bessel.F90 helmadi.F90 realft.F90 tridagr.F90 tridagz.F90  poisson_solver.F90\
print1d.F90
 


OFILES= $(F90FILES:.F90=.o) 

scf:$(OFILES)
	$(FC) $(OFILES) -o scf

$(OFILES):$(F90FILES)
	$(FC) -g -c -r8 -O3 $(F90FILES)
        #$(FC) -g -c $(F90FILES) 
	#$(FC) -g -check all -c -r8 $(F90FILES)

#hydro:$(OFILES)
#       ifort -g -o hydro $(OFILES)
#	ifort -O3 -qarch=pwr2 -o hydro $(OFILES)
#	ifort -g -o hydro $(OFILES)
#	f90 -o hydro -fast $(OFILES)
#	f90 -X16 -o hydro -m0 -O 3,aggress -l mfastv -- $(OFILES)
#	f90 -X4 -o hydro -m0 -O aggress,inline2 -l mfastv -- $(OFILES)
#	f90 -X4 -o hydro -m0 -g -- $(OFILES) ;


#$(OFILES): runhydro.h

#.F90.o: runhydro.h
#	ifort -O3 -qarch=pwr2 -c $<
#	ifort -g -c $<
	
cl:
#	/bin/rm -f *.o hydro
	rm -f *.o scf	
#ifort -g -c *.F90
#ifort main.o potential_solver.o setup.o potsetup.o bessel.o helmadi.o tm.o sm.o realft.o tridagr.o tridagz.o elle.o ellf.o gammln.o rf.o rd.o -o hydro

