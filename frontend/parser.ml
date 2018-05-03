
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TOK_id of (
# 58 "frontend/parser.mly"
       (string)
# 11 "frontend/parser.ml"
  )
    | TOK_const of (
# 59 "frontend/parser.mly"
       (Q.t)
# 16 "frontend/parser.ml"
  )
    | TOK_SQRT
    | TOK_SEMICOLON
    | TOK_RPAREN
    | TOK_RBRACKET
    | TOK_RBRACE
    | TOK_RANDOM
    | TOK_PLUS
    | TOK_OR
    | TOK_NOT_EQUAL_INT
    | TOK_NOT_EQUAL
    | TOK_NOT
    | TOK_MULTIPLY
    | TOK_MINUS
    | TOK_LPAREN
    | TOK_LESS_INT
    | TOK_LESS_EQUAL_INT
    | TOK_LESS_EQUAL
    | TOK_LESS
    | TOK_LBRACKET
    | TOK_LBRACE
    | TOK_INIT
    | TOK_IF
    | TOK_GREATER_INT
    | TOK_GREATER_EQUAL_INT
    | TOK_GREATER_EQUAL
    | TOK_GREATER
    | TOK_GOAL
    | TOK_EQUAL_EQUAL_INT
    | TOK_EQUAL_EQUAL
    | TOK_EOF
    | TOK_ELSE
    | TOK_DIVIDE
    | TOK_COMMA
    | TOK_BODY
    | TOK_ASSIGN
    | TOK_AND
    | TOK_ABS
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState81
  | MenhirState79
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState66
  | MenhirState63
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState14
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState3
  | MenhirState1

# 14 "frontend/parser.mly"
  
open Syntax

# 109 "frontend/parser.ml"

let rec _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_bexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_NOT ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_RANDOM ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_bexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_NOT ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_RANDOM ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_stat : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stat -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv277 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv271 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | TOK_LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | TOK_id _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv272)
        | TOK_BODY | TOK_EOF | TOK_GOAL | TOK_IF | TOK_LBRACE | TOK_RBRACE | TOK_id _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv273 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (e : 'tv_bexpr)), _, (s1 : 'tv_stat)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_stat = 
# 90 "frontend/parser.mly"
    ( If (e, s1, Block []) )
# 496 "frontend/parser.ml"
             in
            _menhir_goto_stat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv275 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv281 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv279 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (e : 'tv_bexpr)), _, (s1 : 'tv_stat)), _, (s2 : 'tv_stat)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_stat = 
# 92 "frontend/parser.mly"
    ( If (e, s1, s2) )
# 519 "frontend/parser.ml"
         in
        _menhir_goto_stat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
    | MenhirState74 | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TOK_LBRACE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TOK_id _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | TOK_RBRACE ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv284)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_BODY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | TOK_LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | TOK_id _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv295) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_GOAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv291) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TOK_LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TOK_id _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv293) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv309) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv305) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv303) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _, (s1 : 'tv_stat)), _, (s2 : 'tv_stat)), _, (s3 : 'tv_stat)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 73 "frontend/parser.mly"
      (Syntax.prog)
# 617 "frontend/parser.ml"
            ) = 
# 83 "frontend/parser.mly"
  ( { init=s1; body=s2; goal=s3; } )
# 621 "frontend/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv301) = _menhir_stack in
            let (_v : (
# 73 "frontend/parser.mly"
      (Syntax.prog)
# 628 "frontend/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299) = Obj.magic _menhir_stack in
            let (_v : (
# 73 "frontend/parser.mly"
      (Syntax.prog)
# 635 "frontend/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297) = Obj.magic _menhir_stack in
            let ((_1 : (
# 73 "frontend/parser.mly"
      (Syntax.prog)
# 642 "frontend/parser.ml"
            )) : (
# 73 "frontend/parser.mly"
      (Syntax.prog)
# 646 "frontend/parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv298)) : 'freshtv300)) : 'freshtv302)) : 'freshtv304)) : 'freshtv306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv307) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv269 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_expr = 
# 104 "frontend/parser.mly"
                                      ( e )
# 670 "frontend/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_goto_list_stat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stat_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * 'tv_stat) * _menhir_state * 'tv_list_stat_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state * 'tv_stat) * _menhir_state * 'tv_list_stat_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stat)), _, (xs : 'tv_list_stat_)) = _menhir_stack in
        let _v : 'tv_list_stat_ = 
# 187 "/home/gg/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 783 "frontend/parser.ml"
         in
        _menhir_goto_list_stat_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)) : 'freshtv260)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state) * _menhir_state * 'tv_list_stat_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_list_stat_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_list_stat_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (b : 'tv_list_stat_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_stat = 
# 94 "frontend/parser.mly"
    ( Block b )
# 804 "frontend/parser.ml"
             in
            _menhir_goto_stat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv265 * _menhir_state) * _menhir_state * 'tv_list_stat_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_goto_bexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | TOK_OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv231 * _menhir_state) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv229 * _menhir_state) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_bexpr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_bexpr = 
# 119 "frontend/parser.mly"
                                 ( e )
# 843 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv233 * _menhir_state) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_bexpr)), _, (e2 : 'tv_bexpr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_bexpr = 
# 117 "frontend/parser.mly"
                            ( Or (e1,e2) )
# 869 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_bexpr)), _, (e2 : 'tv_bexpr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_bexpr = 
# 116 "frontend/parser.mly"
                            ( And (e1,e2) )
# 889 "frontend/parser.ml"
         in
        _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)) : 'freshtv246)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv249 * _menhir_state) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv247 * _menhir_state) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_bexpr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_bexpr = 
# 118 "frontend/parser.mly"
                            ( Not e )
# 902 "frontend/parser.ml"
         in
        _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)) : 'freshtv250)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv255 * _menhir_state)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | TOK_OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | TOK_LBRACE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | TOK_id _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv253 * _menhir_state)) * _menhir_state * 'tv_bexpr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 101 "frontend/parser.mly"
                                      ( Unary (ABS,e) )
# 955 "frontend/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)) : 'freshtv108)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_RPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv109 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)) : 'freshtv112)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_EQUAL_EQUAL | TOK_EQUAL_EQUAL_INT | TOK_GREATER | TOK_GREATER_EQUAL | TOK_GREATER_EQUAL_INT | TOK_GREATER_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_LESS_EQUAL_INT | TOK_LESS_INT | TOK_MINUS | TOK_NOT_EQUAL | TOK_NOT_EQUAL_INT | TOK_OR | TOK_PLUS | TOK_RPAREN | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let o =
              let _1 = _10 in
              
# 109 "frontend/parser.mly"
               ( ADD )
# 1001 "frontend/parser.ml"
              
            in
            
# 99 "frontend/parser.mly"
                                      ( Binary (o,e1,e2) )
# 1007 "frontend/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)) : 'freshtv118)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv119 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _10 = () in
        let _v : 'tv_expr = let o =
          let _1 = _10 in
          
# 111 "frontend/parser.mly"
               ( MUL )
# 1029 "frontend/parser.ml"
          
        in
        
# 99 "frontend/parser.mly"
                                      ( Binary (o,e1,e2) )
# 1035 "frontend/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)) : 'freshtv122)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _10 = () in
        let _v : 'tv_expr = let o =
          let _1 = _10 in
          
# 112 "frontend/parser.mly"
               ( DIV )
# 1050 "frontend/parser.ml"
          
        in
        
# 99 "frontend/parser.mly"
                                      ( Binary (o,e1,e2) )
# 1056 "frontend/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)) : 'freshtv126)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv131 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_EQUAL_EQUAL | TOK_EQUAL_EQUAL_INT | TOK_GREATER | TOK_GREATER_EQUAL | TOK_GREATER_EQUAL_INT | TOK_GREATER_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_LESS_EQUAL_INT | TOK_LESS_INT | TOK_MINUS | TOK_NOT_EQUAL | TOK_NOT_EQUAL_INT | TOK_OR | TOK_PLUS | TOK_RPAREN | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv127 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let o =
              let _1 = _10 in
              
# 110 "frontend/parser.mly"
               ( SUB )
# 1079 "frontend/parser.ml"
              
            in
            
# 99 "frontend/parser.mly"
                                      ( Binary (o,e1,e2) )
# 1085 "frontend/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv129 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 100 "frontend/parser.mly"
                                      ( Unary (NEG,e) )
# 1105 "frontend/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)) : 'freshtv136)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 102 "frontend/parser.mly"
                                      ( Unary (SQRT,e) )
# 1118 "frontend/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv138)) : 'freshtv140)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 1126 "frontend/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv143 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 1144 "frontend/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv141 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 1151 "frontend/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : (
# 58 "frontend/parser.mly"
       (string)
# 1156 "frontend/parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_stat = 
# 88 "frontend/parser.mly"
    ( Assign (i, e) )
# 1163 "frontend/parser.ml"
             in
            _menhir_goto_stat _menhir_env _menhir_stack _menhir_s _v) : 'freshtv142)) : 'freshtv144)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv145 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 1173 "frontend/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_EQUAL_EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TOK_EQUAL_EQUAL_INT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL_INT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_INT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL_INT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_INT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_NOT_EQUAL ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TOK_NOT_EQUAL_INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_RPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv153 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 134 "frontend/parser.mly"
                        ( NEQ_INT )
# 1248 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1254 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv155 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv159 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 128 "frontend/parser.mly"
                        ( NEQ )
# 1288 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1294 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv161 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 129 "frontend/parser.mly"
                        ( LT_INT )
# 1328 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1334 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)) : 'freshtv170)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv175 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 131 "frontend/parser.mly"
                        ( LEQ_INT )
# 1368 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1374 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv173 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 125 "frontend/parser.mly"
                        ( LEQ )
# 1408 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1414 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 123 "frontend/parser.mly"
                        ( LT )
# 1448 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1454 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv193 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv189 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 130 "frontend/parser.mly"
                        ( GT_INT )
# 1488 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1494 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv191 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 132 "frontend/parser.mly"
                        ( GEQ_INT )
# 1528 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1534 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv201 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 126 "frontend/parser.mly"
                        ( GEQ )
# 1568 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1574 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 124 "frontend/parser.mly"
                        ( GT )
# 1608 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1614 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 133 "frontend/parser.mly"
                        ( EQ_INT )
# 1648 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1654 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_OR | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_bexpr = let o =
              let _1 = _10 in
              
# 127 "frontend/parser.mly"
                        ( EQ )
# 1688 "frontend/parser.ml"
              
            in
            
# 115 "frontend/parser.mly"
                            ( Cmp (o,e1,e2) )
# 1694 "frontend/parser.ml"
             in
            _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)
    | MenhirState32 | MenhirState34 | MenhirState66 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_DIVIDE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_EQUAL_EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TOK_EQUAL_EQUAL_INT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL_INT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_INT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL_INT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_INT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MINUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | TOK_NOT_EQUAL ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TOK_NOT_EQUAL_INT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TOK_PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | _ ->
        _menhir_fail ()

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stat_ = 
# 185 "/home/gg/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 1782 "frontend/parser.ml"
     in
    _menhir_goto_list_stat_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 58 "frontend/parser.mly"
       (string)
# 1789 "frontend/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 58 "frontend/parser.mly"
       (string)
# 1799 "frontend/parser.ml"
    )) : (
# 58 "frontend/parser.mly"
       (string)
# 1803 "frontend/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 98 "frontend/parser.mly"
                                      ( Var i )
# 1808 "frontend/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv104)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 59 "frontend/parser.mly"
       (Q.t)
# 1815 "frontend/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 59 "frontend/parser.mly"
       (Q.t)
# 1825 "frontend/parser.ml"
    )) : (
# 59 "frontend/parser.mly"
       (Q.t)
# 1829 "frontend/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 103 "frontend/parser.mly"
                                      ( Cst (c,c) )
# 1834 "frontend/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv102)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_bexpr = 
# 120 "frontend/parser.mly"
             ( Rand )
# 1873 "frontend/parser.ml"
     in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_NOT ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_RANDOM ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_NOT ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_RANDOM ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_const _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 59 "frontend/parser.mly"
       (Q.t)
# 1972 "frontend/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv91 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 1983 "frontend/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_const _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv87 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 1993 "frontend/parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 59 "frontend/parser.mly"
       (Q.t)
# 1998 "frontend/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | TOK_RBRACKET ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv83 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2009 "frontend/parser.ml"
                    ))) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2013 "frontend/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv81 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2020 "frontend/parser.ml"
                    ))) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2024 "frontend/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), (c1 : (
# 59 "frontend/parser.mly"
       (Q.t)
# 2029 "frontend/parser.ml"
                    ))), (c2 : (
# 59 "frontend/parser.mly"
       (Q.t)
# 2033 "frontend/parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_expr = 
# 106 "frontend/parser.mly"
    ( Cst (c1,c2) )
# 2041 "frontend/parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv85 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2051 "frontend/parser.ml"
                    ))) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2055 "frontend/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv89 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2066 "frontend/parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)) : 'freshtv92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv93 * _menhir_state) * (
# 59 "frontend/parser.mly"
       (Q.t)
# 2077 "frontend/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ABS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TOK_LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TOK_LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TOK_MINUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TOK_SQRT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TOK_const _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv15) * _menhir_state * 'tv_stat)) * _menhir_state * 'tv_stat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv17) * _menhir_state * 'tv_stat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_stat) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv21 * _menhir_state)) * _menhir_state * 'tv_bexpr)) * _menhir_state * 'tv_stat)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv23 * _menhir_state)) * _menhir_state * 'tv_bexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_bexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * 'tv_bexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 2277 "frontend/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv80)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 58 "frontend/parser.mly"
       (string)
# 2289 "frontend/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ASSIGN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 2301 "frontend/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ABS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TOK_LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TOK_LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TOK_MINUS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TOK_SQRT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TOK_const _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 58 "frontend/parser.mly"
       (string)
# 2331 "frontend/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TOK_LBRACE ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TOK_id _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | TOK_RBRACE ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ABS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_LPAREN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_MINUS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_RANDOM ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_SQRT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TOK_const _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 73 "frontend/parser.mly"
      (Syntax.prog)
# 2412 "frontend/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_INIT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_IF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | TOK_LBRACE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | TOK_id _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv4)) : 'freshtv6))

# 219 "/home/gg/.opam/4.05.0/lib/menhir/standard.mly"
  


# 2458 "frontend/parser.ml"
