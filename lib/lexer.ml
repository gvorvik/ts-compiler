type t = { lex_buffer : bytes; lex_buffer_len : int; lex_curr_pos : int }

let is_number = function '0' .. '9' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_acceptable_char = function '_' -> true | _ -> false

let is_valid_char x =
  x |> is_number || x |> is_letter || x |> is_acceptable_char

let string_of_char c = String.make 1 c
let get_char l = Bytes.get l.lex_buffer l.lex_curr_pos
let inc_lex_curr_pos l i = { l with lex_curr_pos = l.lex_curr_pos + i }
let filter_key_words = function "const" -> Token.Const | t -> Token.Iden t

(* First char is a .. z or A .. Z *)
let rec read_word l t =
  if l.lex_curr_pos < l.lex_buffer_len && l |> get_char |> is_valid_char then
    read_word (inc_lex_curr_pos l 1) (t ^ (l |> get_char |> string_of_char))
  else (t, filter_key_words t)

(* First char is 0 .. 9 *)
let rec read_number l t =
  if l.lex_curr_pos < l.lex_buffer_len && l |> get_char |> is_number then
    read_number (inc_lex_curr_pos l 1) (t ^ (l |> get_char |> string_of_char))
  else (t, Token.Int (int_of_string t))

(* First char is +/- *)
let read_plus_minus l c1 =
  match inc_lex_curr_pos l 1 |> get_char with
  | '=' -> (string_of_char c1 ^ "=", Token.Assign)
  | c2 ->
      if c1 = c2 then (string_of_char c1 ^ string_of_char c2, Token.Inc_Dec)
      else (string_of_char c1, Token.BinOp (string_of_char c1))

(* First char is = *)
let read_equal l =
  match inc_lex_curr_pos l 1 |> get_char with
  | '=' -> (
      match inc_lex_curr_pos l 2 |> get_char with
      | '=' -> ("===", Token.Equality)
      | _ -> ("==", Token.Equality))
  | '>' -> ("=>", Token.Arrow)
  | _ -> ("=", Token.Assign)

(* First char is * *)
let read_star l =
  match inc_lex_curr_pos l 1 |> get_char with
  | '*' -> (
      match inc_lex_curr_pos l 2 |> get_char with
      | '=' -> ("**=", Token.Assign)
      | _ -> ("**", Token.BinOp "**"))
  | '=' -> ("*=", Token.Assign)
  | _ -> ("*", Token.BinOp "*")

(* First char is % *)
let read_modulo l =
  match inc_lex_curr_pos l 1 |> get_char with
  | '=' -> ("%=", Token.Assign)
  | _ -> ("%", Token.BinOp "%")

(* First char is / *)
let read_slash l =
  match inc_lex_curr_pos l 1 |> get_char with
  | '=' -> ("/=", Token.Assign)
  | _ -> ("/", Token.BinOp "/")

let build_token l =
  if l.lex_curr_pos < l.lex_buffer_len then
    match l |> get_char with
    | '+' -> read_plus_minus l '+'
    | '-' -> read_plus_minus l '-'
    | '*' -> read_star l
    | '/' -> read_slash l
    | '%' -> read_modulo l
    | '=' -> read_equal l
    | ';' -> (";", Token.Semi)
    | '0' .. '9' -> read_number l ""
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> read_word l ""
    | c -> (string_of_char c, Token.Iden (string_of_char c))
  else ("", Token.EOF)

let of_string s =
  {
    lex_buffer = Bytes.of_string s;
    lex_buffer_len = String.length s;
    lex_curr_pos = 0;
  }

let rec traverse_white_space l =
  if l.lex_curr_pos < l.lex_buffer_len then
    match l |> get_char with
    | ' ' | '\n' | '\r' | '\t' -> inc_lex_curr_pos l 1 |> traverse_white_space
    | _ -> l
  else l

let next_token l =
  let l = traverse_white_space l in
  let token = build_token l in
  let token_length = token |> fst |> String.length in
  (inc_lex_curr_pos l token_length, token |> snd)

let peek_token l = l |> next_token |> snd

let show_token = function
  | Token.EOF -> "EOF"
  | Token.Arrow -> "Arrow"
  (* Literals *)
  | Token.Iden s -> "Iden " ^ s
  | Token.Int i -> "Int " ^ string_of_int i
  (* Keywords *)
  | Token.Const -> "Const"
  (* Operators *)
  | Token.BinOp s -> "BinOp " ^ s
  | Token.Assign -> "Assign"
  | Token.Inc_Dec -> "Inc_Dec"
  | Token.Equality -> "Equality"
  (* Delimiters *)
  | Token.Semi -> "Semi"
