open Token

type lexer = { lex_buffer : bytes; lex_buffer_len : int; lex_curr_pos : int }

let is_number = function '0' .. '9' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_acceptable_char = function '_' -> true | _ -> false

let is_valid_char x =
  x |> is_number || x |> is_letter || x |> is_acceptable_char

let string_of_char c = String.make 1 c
let inc_lex_curr_pos l n = { l with lex_curr_pos = l.lex_curr_pos + n }
let filter_key_words = function "const" -> Const | t -> Iden t

let rec read_word l t =
  if
    l.lex_curr_pos < l.lex_buffer_len
    && is_valid_char (Bytes.get l.lex_buffer l.lex_curr_pos)
  then
    let char = Bytes.get l.lex_buffer l.lex_curr_pos in
    read_word (inc_lex_curr_pos l 1) (t ^ string_of_char char)
  else (t, filter_key_words t)

let rec read_number l t =
  if
    l.lex_curr_pos < l.lex_buffer_len
    && is_number (Bytes.get l.lex_buffer l.lex_curr_pos)
  then
    let char = Bytes.get l.lex_buffer l.lex_curr_pos in
    read_number (inc_lex_curr_pos l 1) (t ^ string_of_char char)
  else (t, Int (int_of_string t))

let build_token l =
  if l.lex_curr_pos < l.lex_buffer_len then
    match Bytes.get l.lex_buffer l.lex_curr_pos with
    | '+' -> ("+", Add)
    | '=' -> ("=", Equal)
    | ';' -> (";", Semi)
    | '0' .. '9' -> read_number l ""
    | _ -> read_word l ""
  else ("", EOF)

let of_string s =
  {
    lex_buffer = Bytes.of_string s;
    lex_buffer_len = String.length s;
    lex_curr_pos = 0;
  }

let rec traverse_white_space l =
  if l.lex_curr_pos < l.lex_buffer_len then
    match Bytes.get l.lex_buffer l.lex_curr_pos with
    | ' ' -> inc_lex_curr_pos l 1 |> traverse_white_space
    | _ -> l
  else l

let next_token l =
  let l = traverse_white_space l in
  let token = build_token l in
  let token_length = token |> fst |> String.length in
  (inc_lex_curr_pos l token_length, token)
