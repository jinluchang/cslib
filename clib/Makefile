SHELL=/bin/bash

all:
	g++ -fopenmp -Wall -fPIC -shared -o clib.so -Ieigen3 *.C -lgsl -lgslcblas -lm

clean:
	rm clib.so
