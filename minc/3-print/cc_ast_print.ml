open Cc_ast
;;

let sp = Printf.sprintf
;;

let mapcat delimiter f l =
  String.concat delimiter (List.map f l)
;;

exception Not_implemented of string
;;

let rec string_of_type_expr (type_expr:type_expr) = 
  match type_expr with 
    TYPE_LONG -> "long"
;;

let rec string_of_bin_op(op:bin_op) =
  match op with
    BIN_OP_EQ   -> "="      (* = *)
  | BIN_OP_EQEQ -> "=="      (* == *)
  | BIN_OP_NEQ  -> "!="      (* != *)
  | BIN_OP_LT   -> "<"      (* < *)
  | BIN_OP_GT   -> ">"      (* > *)
  | BIN_OP_LEQ  -> "<="      (* <= *)
  | BIN_OP_GEQ  -> ">="      (* >= *)
  | BIN_OP_PLUS -> "+"      (* + *)
  | BIN_OP_MINUS-> "-"      (* - *)
  | BIN_OP_MUL  -> "*"      (* * *)
  | BIN_OP_DIV  -> "/"      (* / *)
  | BIN_OP_MOD  -> "%"      (* % *)
;;
let rec string_of_un_op(op:un_op) =
  match op with
    UN_OP_PLUS  -> "+"       (* + *)
  | UN_OP_MINUS -> "-"      (* - *)
  | UN_OP_BANG  -> "!"      (* ! *)
  ;;
let rec string_of_expr (expr: expr) =
  match expr with
    EXPR_NUM(num) ->   string_of_int  num(* 整数リテラル(0, 1, 2, 3, ...) *)
  | EXPR_VAR(var) -> var(* 変数 *)
  | EXPR_BIN_OP(op,expr1,expr2) -> (string_of_expr expr1) ^ (string_of_bin_op op) ^ (string_of_expr expr2)  (* 2項演算 (A + B, A < B, A = B, ...) *)
  | EXPR_UN_OP(op,expr1) -> (string_of_un_op op) ^ (string_of_expr expr1)          (* 単項演算 (+A, -A, !A) *)
  | EXPR_CALL(funname,expr_list)->(* 関数呼び出し f(A,B,C,...) *)
    funname^ "(" ^ (
      let rec string_of_expr_list (l:expr list) =
        match l with
          [] -> ""
        | h::r -> (string_of_expr h) ^ (
          match (string_of_expr_list r) with 
            "" -> ""
          | _  -> ","
        ) ^ (string_of_expr_list r)
      in (string_of_expr_list expr_list)
    ) ^ ")"
;;


let rec string_of_def_list(def_list: (type_expr * string) list) =
  match def_list with
    [] -> ""
  | h::r -> 
    let deftype,defname = h in
    (string_of_type_expr deftype) ^ " " ^ defname ^ (
      match (string_of_def_list r) with
        "" -> ""
      | _  -> ";"
    ) ^ (string_of_def_list r)
;;
let rec string_of_arg_list(arg_list: (type_expr * string) list) =
  match arg_list with
    [] -> ""
  | h::r -> 
    let deftype,defname = h in
      (string_of_type_expr deftype) ^ " " ^ defname ^ (
        match (string_of_arg_list r) with
          "" -> ""
        | _  -> ","
      ) ^ (string_of_arg_list r)
;;
let rec string_of_stmt (stmt: stmt) =
  match stmt with
    STMT_EMPTY  ->  ";"                    (* ; *)
  | STMT_CONTINUE ->  "continue;"                 (* continue; *)
  | STMT_BREAK  ->  "break;"                    (* break; *)
  | STMT_RETURN(expr) ->  "return " ^ (string_of_expr expr) ^ ";"       (* return A; *)
  | STMT_EXPR(expr)   ->  (string_of_expr expr) ^ ";"           (* A; *)
  | STMT_COMPOUND(def_list,stmt_list) -> (* { int x; ... } *)
    (string_of_def_list def_list)^ (
      let rec string_of_stmt_list (l:stmt list) =
        match l with
          [] -> ""
        | h::r -> (string_of_stmt h) ^ (string_of_stmt_list r)
      in (string_of_stmt_list stmt_list)
    )
  | STMT_IF(expr,stmt1,stmt2) -> "if(" ^ (string_of_expr expr)^"){"^(string_of_stmt stmt1) ^ "}else{" ^ (string_of_stmt stmt2)^ "}" (* if (A) S else T *)
  | STMT_WHILE(expr,stmt1) -> "while("^(string_of_expr expr)^"){"^(string_of_stmt stmt1)^ "}"   (* while (A) S *) 
;;

let rec string_of_definition (definition: definition) =
  match definition with
    FUN_DEF(fun_type,fun_name,arg_list,stmt) ->
      (string_of_type_expr fun_type)^ " " ^ fun_name ^ "(" ^ (string_of_arg_list arg_list) ^ "){" ^ (string_of_stmt stmt) ^ "}"
;;
(* これを実装するのが役目 *)
let rec string_of_program (program : definition list) =
  match program with
    [] -> ""
  | h::r -> (string_of_definition h) ^ (string_of_program r)
;;

