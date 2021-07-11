# Assignment for "Programming Language"
## Simple C compiler by OCaml
overview of minc specification

- the only type supported is "long" (64 bit integer)
- therefore, no typedefs
- no global variables
- no forward declarations of functions (as they are not necessary)
- therefore only function definitions come at the toplevel

## Usage
```
$ cd minc/4-cogen
$ . /home/share/.opam/opam-init/init.sh
#compile
$ ./cc.byte ../test/fun.c fun.s 
#test
$ make -f test.mk 
```