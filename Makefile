clean:
	find . -name "*genmod*" -type f -delete
	rm a.out

main:
	ifort -std08 -warn all -funroll-loops -pedantic main.F90 matmul.F90 -O2