(* 中間言語 (IR) *)
(*コードはここで公開する予定:https://github.com/NaomiatLibrary/ocaml_ccompiler *)
(* 2021/08/10(課題提出期限)までは非公開にしています　*)
(* オペコード一覧：https://www.felixcloutier.com/x86/ *)
(* 追加課題として、forループに対応したほか、*)
(* 入れ子ループからのbreakなどに対応するためいくつかテストケースを足しました。 *)
open Cc_ast
;;

let sp = Printf.sprintf
;;

let map_append f l =        (* (f l0) @ (f l1) @ ... @ (f ln1-)*)
  List.concat (List.map f l)
;;

let map_cat delim f l =
  String.concat delim (List.map f l)
;;

exception Not_implemented of string
;;

(*関数の情報を保存するクラス*)
class func_info func_num return_label=
  object(self)
    (*string:the name of variable*)
    val mutable var_list = ([] :(string*int) list)
    (*rspが関数開始直後からどのくらい小さくなったかを記録*)
    val mutable len_stack = 0
    val func_num = func_num
    val return_label = return_label
    val mutable label_num = 0
    (*複数のループが入れ子になっているときのことを考え、continueで抜けるためのラベル番号をスタックで持っておく*)
    val mutable loop_num = ([] : int list)
    (*複数のループが入れ子になっているときのことを考え、breakで抜けるためのラベル番号をスタックで持っておく*)
    val mutable end_num = ([] : int list)
    (*stackを拡張して、その後のlen_stackを返す*)
    method extend_stack(size:int) =
      let _=len_stack<-len_stack+size in
      len_stack
    (*var_listにnew_varを追加し、そのときのlen_stackを返す*)
    method add_var(new_var:type_expr * string) = 
      (* Printf.printf "add_var:%d/" len_stack; *)
      let vartype,varname=new_var in
      let address = self#extend_stack 8 in
      var_list <- (varname,address)::var_list;address
    (*var_listからvar_nameを検索し、rsp+何番目かを返す*)
    method find_var(var_name:string) =
      let is_name_match v = (*検索時の条件を定義する関数 *)
        let name,_ = v in name = var_name 
      in
      try
        let _,address =List.find is_name_match var_list in 
        (* Printf.printf "find:%d(%d)/" address len_stack; *)
        len_stack-address
      with
        Not_found -> 0
    (*pushqするとrspが-8されてしまうので…（めんどくさいのでもっとやりようがありそう）*)
    method pushq =
      (* Printf.printf "pushq:%d/" len_stack; *)
      len_stack <- len_stack+8;
    method popq =
      (* Printf.printf "popq:%d/" len_stack; *)
      len_stack <- len_stack-8;
    (*スタックを空にする命令を吐きます*)
    method clear_stack =
      let rec rec_clear_stack i =
        if i<=0 then "" else "\tpopq\t%rbx\n" ^ (rec_clear_stack (i-8) )
      in
      (* Printf.printf "clear:%d" len_stack; *)
      rec_clear_stack len_stack
    (*returnに飛ぶ*)
    method jump_return =
      "\tjmp\t" ^ return_label ^ "\n"
    (*さまざまなラベルを生成する*)
    method gen_label =
        label_num<-label_num+1;
        sp ".LF%dL%d" func_num (label_num-1)
    method gen_label_loop =
        loop_num <- label_num::loop_num;
        label_num<-label_num+1;
        sp ".LF%dL%d" func_num (label_num-1)
    method gen_label_end =
        end_num <- label_num::end_num;
        label_num<-label_num+1;
        sp ".LF%dL%d" func_num (label_num-1)
    (*さまざまなラベルに飛ぶ*)
    method jmp_loop_label = 
        let num =
          (match loop_num with
          [] -> 0
          | h::r -> h
          ) in
        sp "\tjmp\t.LF%dL%d\n" func_num num
    method jmp_end_label = 
        let num =
          (match end_num with
          [] -> 0
          | h::r -> h
          ) in
        sp "\tjmp\t.LF%dL%d\n" func_num num
    method exit_loop =
        loop_num <- (match loop_num with
        [] -> []
        | h::r -> r);
        end_num <- (match end_num with
        [] -> []
        | h::r -> r);
  end
;;

(*expr(式)のコンパイル*)
(*結果は一々スタックに挿入することでわかりやすく*)
let rec cogen_expr expr (info:func_info)=
  (match expr with
    EXPR_NUM(num) ->
      let _ =info#pushq in
      "\tmovq\t$"^(string_of_int num)^",%rax\n" ^
      "\tpushq\t%rax\n"(*stackにnumをpush*)
    | EXPR_VAR(var) -> (*varはrsp+addressに保存しておく*)
      let address= info#find_var(var) in
      let _=info#pushq in
        (* Printf.printf "%d:%s/" address var; *)
        "\tpushq\t"^(string_of_int address)^"(%rsp)\n"(*stackにvarをpush*)
    | EXPR_BIN_OP(op,expr1,expr2) ->
      (match op with
        (*　=(代入)は難しい。左辺はVARのみとする。*)
        BIN_OP_EQ ->
          (match expr1 with
            EXPR_VAR(var) ->
              let code_expr2 = cogen_expr expr2 info in (*右辺を計算*)
              let _ = info#popq in
              let address = info#find_var var in
              let _ = info#pushq in
              code_expr2 ^
              "\tpopq\t%rax\n"^(*%raxにexpr2が格納される *)
              "\tmovq\t%rax,"^(string_of_int address) ^ "(%rsp)\n" ^(*左辺の変数に右辺raxを代入*)
              "\tpushq\t%rax\n"(*expr1=expr2が返ってくる値なのでそのままraxを返す*)
          | _ -> "\t#ERROR!\n"
          )
      | _ ->
        (*　=(代入)以外は簡単*)
        let code_expr1 = cogen_expr expr1 info in
        let code_expr2 = cogen_expr expr2 info in
        let _ = info#popq in
        let _ = info#popq in
        let _ = info#pushq in
        code_expr1 ^ code_expr2 ^
        "\tpopq\t%rbx\n"^
        "\tpopq\t%rax\n"^(*%raxにexpr1が、%rbxにexpr2が格納される *)
        (* 2項演算 (A + B, A < B, A = B, ...) *)
        (match op with
          BIN_OP_EQ -> "\t#ERROR:this will not be reached\n"
        | BIN_OP_EQEQ -> (* == *)
          "\tcmpq %rbx,%rax\n" ^ (*eflagsにrax-rbxの結果を保存*)
          "\tmovq\t$0,%rax\n"^(*raxを0にする*)
          "\tsete\t%al\n" (*eflagsの結果に応じてraxの下1bitを変更*)
        | BIN_OP_NEQ  ->(* != *)
          "\tcmpq\t%rbx,%rax\n" ^ (*eflagsにrax-rbxの結果を保存*)
          "\tmovq\t$0,%rax\n"^(*raxを0にする*)
          "\tsetne\t%al\n" (*eflagsの結果に応じてraxの下1bitを変更*)
        | BIN_OP_LT   -> (* < *)
          "\tcmpq\t%rbx,%rax\n" ^ (*eflagsにrax-rbxの結果を保存*)
          "\tmovq\t$0,%rax\n"^(*raxを0にする*)
          "\tsetl\t%al\n" (*eflagsの結果に応じてraxの下1bitを変更*)
        | BIN_OP_GT   -> (* > *)
          "\tcmpq\t%rbx,%rax\n" ^ (*eflagsにrax-rbxの結果を保存*)
          "\tmovq\t$0,%rax\n"^(*raxを0にする*)
          "\tsetg\t%al\n" (*eflagsの結果に応じてraxの下1bitを変更*)
        | BIN_OP_LEQ  -> (* <= *)
          "\tcmpq\t%rbx,%rax\n" ^ (*eflagsにrax-rbxの結果を保存*)
          "\tmovq\t$0,%rax\n"^(*raxを0にする*)
          "\tsetle\t%al\n" (*eflagsの結果に応じてraxの下1bitを変更*)
        | BIN_OP_GEQ  -> (* >= *)
          "\tcmpq\t%rbx,%rax\n" ^ (*eflagsにrax-rbxの結果を保存*)
          "\tmovq\t$0,%rax\n"^(*raxを0にする*)
          "\tsetge\t%al\n" (*eflagsの結果に応じてraxの下1bitを変更*)
        | BIN_OP_PLUS -> "\taddq\t%rbx,%rax\n"
        | BIN_OP_MINUS-> "\tsubq\t%rbx,%rax\n"
        | BIN_OP_MUL  -> "\timulq\t%rbx,%rax\n"
        | BIN_OP_DIV  -> "\tcqto\n\tidivq\t%rbx\n"
        | BIN_OP_MOD  -> "\tcqto\n\tidivq\t%rbx\n\tmovq\t%rdx,%rax\n"
        )^(*%raxに計算結果が格納されている*)
        "\tpushq\t%rax\n"(*stackにrax(計算結果)が格納される*)
        )
    | EXPR_UN_OP(op,expr1) ->  (* 単項演算 (+A, -A, !A) *)
      let code_expr = cogen_expr expr1 info in
      let _=info#pushq in
      code_expr ^
      (match op with
        UN_OP_PLUS  -> let _=info#popq in "\tpopq %rax\n" 
      | UN_OP_MINUS -> let _=info#popq in "\tpopq %rax\n\timulq\t$-1,%rax\n"
      | UN_OP_BANG  -> 
        let _=info#popq in
        "\tpopq\t%rbx\n"^(*rbxにAを保存*)
        "\tmovq\t$0,%rax\n"^(*raxを0にする*)
        "\ttestq\t%rbx,%rbx\n"^ (*rbx&rbx(=rbx)が0かどうかをZFに保存*)
        "\tsete\t%al\n"      (*%al($raxの下1bit)がZF=1の時1,0の時0になる*)
      )^(*%raxに計算結果が格納されている*)
      "\tpushq\t%rax\n"(*stackにrax(計算結果)が格納される*)
    | EXPR_CALL(funname,expr_list)-> (* 関数呼び出し f(A,B,C,...) *)
      (*引数たちをrdi,rsi...に保存する関数*)
      let rec cogen_input_arg expr info i=
        let code_expr=cogen_expr expr info in
        let _=info#popq in
        code_expr ^ (*exprの結果がstackの一番上にある*)
        "\tpopq\t%rax\n" ^(*exprの結果がraxにある*)
        (match i with
          0 -> "\tmovq\t%rax,%rdi\n"
        | 1 -> "\tmovq\t%rax,%rsi\n"
        | 2 -> "\tmovq\t%rax,%rdx\n"
        | 3 -> "\tmovq\t%rax,%rcx\n"
        | 4 -> "\tmovq\t%rax,%r8\n"
        | 5 -> "\tmovq\t%rax,%r9\n"
        | _ -> (*ただこれを順番に実行するとrsp上での位置が逆順になってしまう…*)
          let _=info#pushq in
          "\tpushq\t%rax\n"
        )
      in
      let rec cogen_input_args expr_list info i= 
        match i with
         5 -> (*スタックに逆順に格納するので裏返す*)
            (match expr_list with
            []-> ""
          | h::r -> 
            let code1 = cogen_input_arg h info i in
            let code2 = cogen_input_args (List.rev r) info (i+1) in
            code1^code2
          )
        | _ ->
          (match expr_list with
            []-> ""
          | h::r -> 
            let code1=cogen_input_arg h info i in 
            let code2=cogen_input_args r info (i+1) in
            code1^code2
          )
      in
      let _=info#pushq in
      let code_expr = cogen_input_args expr_list info 0 in
      let _=info#popq in
      let _=info#pushq in
      "\tsubq\t$8,%rsp\n"^
      code_expr^
      "\tcall\t"^funname^"@PLT\n"^
	    "\taddq\t$8,%rsp\n"^
      "\tpushq\t%rax\n"(*stackにrax(計算結果)が格納される*)
  )
;;

(*変数たちをrspに保存する、初期値は0*)
let rec cogen_def (def:type_expr * string) info =
  let _ = info#add_var def in
  "\tpushq\t$0\n",info
;;
let rec cogen_def_list (arg_list:(type_expr * string) list) info=
  match arg_list with
      [] -> "",info
    | h::r -> 
      let code_h,new_info=cogen_def h info in
      let code_r,new_new_info=cogen_def_list r new_info in
      code_h^code_r,new_new_info
;;

(*stmt(文)のコンパイル*)
let rec cogen_stmt (stmt:stmt) (info:func_info):string=
  match stmt with
    STMT_EMPTY  ->  ""                   (* ; *)
  | STMT_CONTINUE ->                   (* continue; *)
    info#jmp_loop_label
  | STMT_BREAK  ->                    (* break; *)
    info#jmp_end_label
  | STMT_RETURN(expr) ->
    let code_expr = cogen_expr expr info in
    let _ = info#popq in
    let code_return = info#jump_return in
    code_expr^
    "\tpopq %rax\n"^(* return A は raxにAの中身を代入する*)
    code_return(*終了ラベルまで飛ぶ*)
  | STMT_EXPR(expr)   ->  (* A; *)
    let code_expr = cogen_expr expr info in
    let _ = info#popq in
    code_expr^
    "\tpopq %rax\n"
  | STMT_COMPOUND(def_list,stmt_list) -> 
    let code_def,new_info = cogen_def_list def_list info in
    code_def ^
    (let rec cogen_stmt_list (l:stmt list) info=
      match l with
        [] -> ""
      | h::r -> 
        let code1=cogen_stmt h new_info in
        let code2=cogen_stmt_list r new_info in
        code1^code2
    in (cogen_stmt_list stmt_list new_info) )
  | STMT_IF(expr,stmt1,stmt2) -> (* if (A) S else T *)
    let label_else = info#gen_label in
    let label_end = info#gen_label in
    let code_expr=cogen_expr expr info in
    let _=info#popq in
    let code_stmt1=cogen_stmt stmt1 info in
    let code_stmt2=cogen_stmt stmt2 info in
    code_expr^
    "\tpopq\t%rax\n"^
    "\tcmpq\t$0,%rax\n"^
    "\tje\t"^label_else^"\n"^(*Aが0の時のみelseにジャンプ*)
    code_stmt1^
    "\tjmp\t"^label_end^"\n"^
    label_else^":\n"^
    code_stmt2^
    label_end^":\n"
  | STMT_WHILE(expr,stmt1) ->  (* while (A) S *) 
    let label_start=info#gen_label_loop in
    let label_end=info#gen_label_end in
    let code_expr=cogen_expr expr info in
    let _=info#popq in
    let code_stmt1=cogen_stmt stmt1 info in
    let _=info#exit_loop in
    label_start^":\n"^(*while文の中の処理*)
    code_expr^
    "\tpopq\t%rax\n"^
    "\tcmpq\t$0,%rax\n"^
    "\tje\t"^label_end^"\n"^(*Aが0の時のみendにジャンプ*)
    code_stmt1^
    "\tjmp\t"^label_start^"\n"^(*startにジャンプ*)
    label_end^":\n"(*while文の最後*)
  | STMT_FOR(expr1,expr2,expr3,stmt1) ->(* for (A;B;C) S *)
    let label_start=info#gen_label in
    let label_loop =info#gen_label_loop in
    let label_end = info#gen_label_end in
    let code_expr1=cogen_expr expr1 info in
    let _=info#popq in
    let code_expr2=cogen_expr expr2 info in
    let _=info#popq in
    let code_stmt=cogen_stmt stmt1 info in
    let code_expr3=cogen_expr expr3 info in
    let _=info#popq in
    let _=info#exit_loop in
    code_expr1 ^ (*初期化*)
    "\tpopq\t%rax\n"^
    label_start^":\n"^(*処理のはじめ*)
    code_expr2 ^ (*条件文*)
    "\tpopq\t%rax\n"^
    "\tcmpq\t$0,%rax\n"^
    "\tje\t"^label_end^"\n"^(*Bが0=条件を満たしていない時のみendにジャンプ*)
    code_stmt^ (*処理*)
    label_loop^":\n"^(*ループ処理のはじめ*)
    code_expr3 ^ (*変化式*)
    "\tpopq\t%rax\n"^
    "\tjmp\t"^label_start^"\n"^(*条件評価までとぶ*)
    label_end^":\n"(*for文の最後*)
;;

(*引数を変数としてスタックに保存し直す*)
let rec cogen_arg (arg:(type_expr * string)) info i=
  let _ = info#add_var arg in 
  match i with
    0 -> "\tpushq\t%rdi\n"
  | 1 -> "\tpushq\t%rsi\n"
  | 2 -> "\tpushq\t%rdx\n"
  | 3 -> "\tpushq\t%rcx\n"
  | 4 -> "\tpushq\t%r8\n"
  | 5 -> "\tpushq\t%r9\n"
  | _ -> (*すでに0,1...iがpushされた後なので8(%rsp),16(%rsp)ではなくて難しい*)
    let now_address = (i+i-5)*8 in
    "\tpushq\t"^(string_of_int now_address)^"(%rsp)\n"
;;
(*引数たちを変数としてrspに保存し直す*)
let rec cogen_arg_list (arg_list:(type_expr * string) list) info i=
  match arg_list with
      [] -> ""
    | h::r -> 
      let code_h=cogen_arg h info i in
      let code_r=cogen_arg_list r info (i+1) in
      code_h^code_r
;;

(*関数のコンパイル*)
let rec cogen_definition definition i=
  (* Printf.printf "\n関数f%dを生成\n" i; *)
  let return_label = ".LFR"^ (string_of_int i) in
  let info = new func_info i return_label in
  match definition with
    FUN_DEF(fun_type,fun_name,arg_list,stmt) ->
      let code_args = cogen_arg_list arg_list info 0 in
      let code_stmt = cogen_stmt stmt info in
      let code_clear = info#clear_stack in
      ".globl "^ fun_name ^ "\n"^
      ".type "^	fun_name ^", @function\n"^
      fun_name ^ ":\n" ^ 
      ".LFB" ^ (string_of_int i) ^ ":\n" ^
      "\t.cfi_startproc\n" ^ 
      "\tendbr64\n" ^ 
      code_args ^ 
      code_stmt ^
      return_label ^ ":\n"^
      code_clear^
      "\tret\n" ^
      "\t.cfi_endproc\n" 
;;

(*関数リストのコンパイル*)
let rec cogen_definiton_list def_list i = 
  match def_list with
    [] -> ""
  | h::r -> (cogen_definition h i)  ^ (cogen_definiton_list r (i+1) )
;;
(* これを実装するのが役目 *)
let rec cogen_program program src_name =
  ".file \"" ^	src_name ^ "\"\n.text\n" ^ (cogen_definiton_list program 0)
;;
