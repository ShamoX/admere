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
