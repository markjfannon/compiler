# Compiler
This was the final coursework project in my third year compilers module.
It takes minitriangle programs, parses them, runs a type check and then generates code to run on a Triangle Abstract Machine (TAM) implementation.

All written in Haskell! 

Weirdly some files take ages to parse - I never worked out why. So if it hangs for a bit after being given a particularly demanding source file, don't worry :)

## Requirements
- GHC must be installed on your machine

## Install
Compile using `ghc Main.hs -O2`

## Usage 
- `./Main file.mt` to compile a .mt program file
- `./Main file.tam` to run a .tam program file
