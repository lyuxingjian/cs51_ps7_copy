all: refs refs_test sampleStreams streamstrees nativeLazyStreams music  

refs: refs.ml
	ocamlbuild -use-ocamlfind refs.byte

refs_test: refs_test.ml
	ocamlbuild -use-ocamlfind refs_test.byte

sampleStreams: sampleStreams.ml
	ocamlbuild -use-ocamlfind sampleStreams.byte

streamstrees: streamstrees.ml
	ocamlbuild -use-ocamlfind streamstrees.byte

nativeLazyStreams: nativeLazyStreams.ml 
	ocamlbuild -use-ocamlfind nativeLazyStreams.byte

music: music.ml
	ocamlbuild -use-ocamlfind music.byte

clean: 
	rm -rf _build *.byte

