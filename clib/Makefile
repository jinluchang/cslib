SHELL=/bin/bash

OPTS=

all:
	g++ -fopenmp -Wall -fPIC -shared -o clib.so -Ieigen3 *.C -lgsl -lgslcblas -lm -lz $(OPTS)

clean:
	rm clib.so
