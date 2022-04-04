ocamlbuild -use-ocamlfind refs.byte
ocamlbuild -use-ocamlfind refs_test.byte
ocamlbuild -use-ocamlfind music.byte
ocamlbuild -use-ocamlfind streamstrees.byte
ocamlbuild -use-ocamlfind music.byte
./music.byte
rm -rf _build *.byte