all : cc.top cc.byte cc.native
srcs := cc_ast.ml cc_parse.mly cc_lex.mll cc_ast_print.ml cc.ml 

# standalone byte code
cc.byte : $(srcs)
	ocamlbuild -use-menhir $@

# standalone native code (only faster than cc.byte)
cc.native : $(srcs)
	ocamlbuild -use-menhir $@

# interactive code
cc.top : cc.byte
	echo $(which ocaml) > $@
	echo "ocaml -I _build _build/cc_ast.cmo _build/cc_parse.cmo _build/cc_lex.cmo cc_ast_print.cmo _build/cc.cmo" >> $@
	chmod +x $@

clean :
	rm -rf _build cc.top cc.byte cc.native
