# Build #
```
make
```

After the build 2 binaries will pop-up in the root directory
- insc_llvm - llvm compiler
- insc_jvm - jvm compiler

# Run #
```
./[insc_llvm | insc_jvm] <instant file>

Example
./insc_llvm example.ins
```
# Output #
- insc_llvm
    - *.ll - llvm code
    - *.bc - llvm bitcode
- insc_jvm
    - *.j - jasmin code
    - *.class - jvm bytecode

# Additional libraries #
- lib
    - jasmin.jar - used to create .class files
    - bnfc - used to generate abstract syntax tree
# Directory Structure #
```
├── lib
|   ├── jasmin.jar          # Binary used to create .class files from .j files
|   ├── bnfc                # Bnfc binary 
├── src
|   ├── Instant             # Directory with files created by bnfc
|   |   ├── ... 
|   ├── Instant.cf          # Instant grammar
|   ├── JvmCompiler.hs      # Actually compiling jvm from abstract tree
|   ├── JvmMain.hs          # Main Jvm
|   ├── LlvmCompiler.hs     # Actualy compiling llvm from abstract tree
|   ├── LlvmMain.hs         # Main Llvm
|   ├── Makefile            # Bnfc related makefile
├── Makefile                # Main makefile
└── README.md               # Me :skull:
