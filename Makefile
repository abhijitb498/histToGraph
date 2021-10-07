FC = gfortran 
source = histToGraph.f90
all : htg 


htg : $(source)
	$(FC) -o $@  $(source)

install: 
	@mv htg /usr/bin/ 

.PHONY : clean

clean:
	@rm -f *.o *.mod htg
