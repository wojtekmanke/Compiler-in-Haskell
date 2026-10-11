all:
	cd src && ghc --make latc_x86_64.hs -o generate_asm
	cd runtime && g++ -c runtime.cpp
	mv src/generate_asm .
	chmod +x latc_x86_64

clean:
	rm src/*.hi src/*.o generate_asm
