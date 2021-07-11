all : cc_lex.top cc_lex.byte cc_lex.native

# standalone byte code
cc_lex.byte : cc_lex.mll
	ocamlbuild -use-menhir $@

# standalone native code (only faster than cc_lex.byte)
cc_lex.native : cc_lex.mll
	ocamlbuild -use-menhir $@

# interactive code
cc_lex.top : cc_lex.byte
	echo $(which ocaml) > $@
	echo "ocaml -I _build _build/cc_lex.cmo" >> $@
	chmod +x $@

clean :
	rm -rf _build cc_lex.top cc_lex.byte cc_lex.native
