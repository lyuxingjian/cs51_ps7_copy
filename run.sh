ocamlbuild -use-ocamlfind -r sampleStreams.byte
ocamlbuild -use-ocamlfind -r nicholas_tests.byte
./nicholas_tests.byte
rm -rf _build *.byte