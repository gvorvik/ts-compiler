open Ts_compiler

let sample_program = "const x = 1 + 2;"

let () =
  print_endline "Sample program:";
  print_endline sample_program;
  print_newline ();
  print_endline "Lexer output:"

let rec collect l acc =
  match Lexer.next_token l with
  | _, Token.EOF -> Token.EOF :: acc
  | l', t ->
      let () = print_endline (Lexer.show_token t) in
      collect l' (t :: acc)

let _ =
  let lexer = Lexer.of_string sample_program in
  collect lexer []
