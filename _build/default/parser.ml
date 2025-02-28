
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | STAR
    | RPAR
    | LPAR
    | EOF
    | EMPTY
    | CHOICE
    | CHAR of (
# 3 "parser.mly"
       (char)
# 21 "parser.ml"
  )
  
end

include MenhirBasics

# 1 "parser.mly"
   open Ast 
# 30 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState01 : (('s, _menhir_box_main) _menhir_cell1_LPAR, _menhir_box_main) _menhir_state
    (** State 01.
        Stack shape : LPAR.
        Start symbol: main. *)

  | MenhirState09 : (('s, _menhir_box_main) _menhir_cell1_disjunction, _menhir_box_main) _menhir_state
    (** State 09.
        Stack shape : disjunction.
        Start symbol: main. *)

  | MenhirState10 : ((('s, _menhir_box_main) _menhir_cell1_disjunction, _menhir_box_main) _menhir_cell1_concat, _menhir_box_main) _menhir_state
    (** State 10.
        Stack shape : disjunction concat.
        Start symbol: main. *)

  | MenhirState13 : (('s, _menhir_box_main) _menhir_cell1_concat, _menhir_box_main) _menhir_state
    (** State 13.
        Stack shape : concat.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_concat = 
  | MenhirCell1_concat of 's * ('s, 'r) _menhir_state * (Ast.re)

and ('s, 'r) _menhir_cell1_disjunction = 
  | MenhirCell1_disjunction of 's * ('s, 'r) _menhir_state * (Ast.re)

and ('s, 'r) _menhir_cell1_repetition = 
  | MenhirCell1_repetition of 's * ('s, 'r) _menhir_state * (Ast.re)

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (Ast.re) [@@unboxed]

let _menhir_action_01 =
  fun r ->
    (
# 34 "parser.mly"
                      ( r )
# 79 "parser.ml"
     : (Ast.re))

let _menhir_action_02 =
  fun c ->
    (
# 35 "parser.mly"
           ( Caracter c )
# 87 "parser.ml"
     : (Ast.re))

let _menhir_action_03 =
  fun () ->
    (
# 36 "parser.mly"
        ( Vazio )
# 95 "parser.ml"
     : (Ast.re))

let _menhir_action_04 =
  fun a b ->
    (
# 26 "parser.mly"
                            ( Concatenacao(a, b) )
# 103 "parser.ml"
     : (Ast.re))

let _menhir_action_05 =
  fun r ->
    (
# 27 "parser.mly"
                 (r)
# 111 "parser.ml"
     : (Ast.re))

let _menhir_action_06 =
  fun a b ->
    (
# 22 "parser.mly"
                                    ( Escolha(a, b) )
# 119 "parser.ml"
     : (Ast.re))

let _menhir_action_07 =
  fun r ->
    (
# 23 "parser.mly"
             (r)
# 127 "parser.ml"
     : (Ast.re))

let _menhir_action_08 =
  fun r ->
    (
# 16 "parser.mly"
                    ( r )
# 135 "parser.ml"
     : (Ast.re))

let _menhir_action_09 =
  fun r ->
    (
# 19 "parser.mly"
                  ( r )
# 143 "parser.ml"
     : (Ast.re))

let _menhir_action_10 =
  fun r ->
    (
# 30 "parser.mly"
                      ( Estrela r )
# 151 "parser.ml"
     : (Ast.re))

let _menhir_action_11 =
  fun r ->
    (
# 31 "parser.mly"
           ( r )
# 159 "parser.ml"
     : (Ast.re))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | CHAR _ ->
        "CHAR"
    | CHOICE ->
        "CHOICE"
    | EMPTY ->
        "EMPTY"
    | EOF ->
        "EOF"
    | LPAR ->
        "LPAR"
    | RPAR ->
        "RPAR"
    | STAR ->
        "STAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let r = _v in
          let _v = _menhir_action_08 r in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState01 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EMPTY ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_03 () in
      _menhir_goto_atom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_atom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let r = _v in
      let _v = _menhir_action_11 r in
      _menhir_goto_repetition _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_repetition : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState13 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_concat as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_repetition (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CHAR _ | CHOICE | EMPTY | EOF | LPAR | RPAR ->
          let MenhirCell1_concat (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_04 a b in
          _menhir_goto_concat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_05 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_repetition -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_repetition (_menhir_stack, _menhir_s, r) = _menhir_stack in
      let _v = _menhir_action_10 r in
      _menhir_goto_repetition _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_concat : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_stack = MenhirCell1_concat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | EMPTY ->
          let _menhir_stack = MenhirCell1_concat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | CHAR _v_0 ->
          let _menhir_stack = MenhirCell1_concat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState13
      | CHOICE | EOF | RPAR ->
          let r = _v in
          let _v = _menhir_action_07 r in
          _menhir_goto_disjunction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_02 c in
      _menhir_goto_atom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_disjunction : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | CHOICE ->
          let _menhir_stack = MenhirCell1_disjunction (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState09 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EMPTY ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EOF | RPAR ->
          let r = _v in
          let _v = _menhir_action_09 r in
          _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_regex : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_14 _menhir_stack _v _tok
      | MenhirState01 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_06 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_01 r in
          _menhir_goto_atom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_disjunction as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_stack = MenhirCell1_concat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | EMPTY ->
          let _menhir_stack = MenhirCell1_concat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | CHAR _v_0 ->
          let _menhir_stack = MenhirCell1_concat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState10
      | CHOICE | EOF | RPAR ->
          let MenhirCell1_disjunction (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_06 a b in
          _menhir_goto_disjunction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_repetition (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CHAR _ | CHOICE | EMPTY | EOF | LPAR | RPAR ->
          let r = _v in
          let _v = _menhir_action_05 r in
          _menhir_goto_concat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EMPTY ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
