.PHONY : main.hs, clean

build: main.hs
	ghc --make main.hs

deploy: build
	./main build

clean:
	rm main.o; rm main.hi; rm main
