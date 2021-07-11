minc (Minimum C) compiler

overview of minc specification

* the only type supported is "long" (64 bit integer)
* therefore, no typedefs 
* no global variables
* no forward declarations of functions (as they are not necessary)
* therefore only function definitions come at the toplevel

1-cc-lex/ --- lexical analyzer for C (no work for you)
2-cc-parse/ --- lexical analyzer + parser for C (no work for you)
3-print/ --- converting AST back to program string (a warm-up exercise)
4-cogen/ --- the minc compiler, which converts a program to assembly (the real exercise for you)
test/ --- test code for 4-cogen

See notebook for instructions
