admere
======

Ocaml implementation for a library allowing adaptive mesh refinement

Dependencies
------------

Needs ocaml native Graphics module : compile ocaml with X11.
On Mac OS X, you need to make sure that X11 development files are available and installed.
Follow the XQuartz installation and recompile ocaml.

Needs batteries package : opam install batteries

Compilation
-----------

> ocamlbuild -use-ocamlfind libAdmere.cma libAdmere.cmxa

Clean
-----

> ocamlbuild -clean


Test
----

### Simple random app

This test simply fills the universe with random numbers and then increase the
refinement if the number in a cell is superior to 2000. It will merge if numbers around are inferior to 500 (but it can hardly happens).

#### Build

> ocamlbuild -use-ocamlfind test/testExample.native

#### Launch test
