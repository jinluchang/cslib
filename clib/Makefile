SHELL=/bin/bash

all:
	g++ -fopenmp -Wall -fPIC -shared -o clib.so -I/usr/include/eigen3 *.C -lgsl -lgslcblas -lm

clean:
	rm clib.so
