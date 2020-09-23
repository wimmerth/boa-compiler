# Boa - Compiler
Compiler for the programming language Boa which is a part of Python. Written in Haskell using the ReadP library.

Consists of a parser (/src/BoaParser.hs) and an interpreter (/src/BoaInterp.hs).

The compiler was part of the assignments in 'Advanced Programming 20/21' at the University of Copenhagen.

If you want to run the compiler, you may need the LTS-16.12 release of Haskell (including GHC-8.8.4, can be found in Stack).

You can run the compiler using `stack build` and `stack run boa -- [file].boa`.
