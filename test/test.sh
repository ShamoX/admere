ocamlopt.opt -I /Users/roland/ocamlbrew/ocaml-4.00.1/.opam/system/lib/batteries -I ../_build/src/ unix.cmxa graphics.cmxa str.cmxa nums.cmxa bigarray.cmxa batteries.cmxa AdmereExceptions.cmx AdmereUtils.cmx libAdmere.cmxa testExample.ml testAdmereDisplayer.ml -o testAdmereDisplayer && rm -fr *.cm* *.o && ./testAdmereDisplayer
