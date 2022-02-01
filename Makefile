FC = gfortran 
source = hist_mod.f90 histToGraph_v2.f90 
all : htg 


htg : $(source)
	$(FC) -o $@  $(source)

install: 
	@cp htg /usr/bin/ 
	@cp hist_mod.mod /usr/lib/gcc/x86_64-linux-gnu/9/finclude/

.PHONY : clean

clean:
	@rm -f *.o *.mod htg
