.PHONY: all test clean

all:
	cd src; make

clean:
	cd src; make clean
	cd test; make clean

test:
	cd test; make test
