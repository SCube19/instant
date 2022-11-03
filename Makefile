all: insc

insc: insc_llvm insc_jvm

insc_jvm: src/JvmMain.hs src/JvmCompiler.hs bnfc
	ghc -isrc src/JvmMain.hs -o ./insc_jvm

insc_llvm: src/LlvmMain.hs src/LlvmCompiler.hs bnfc
	ghc -isrc src/LlvmMain.hs -o ./insc_llvm 

bnfc: src/Makefile
	cd src && make

clean:
	rm insc_llvm insc_jvm src/*.o src/*.hi && cd src && make clean