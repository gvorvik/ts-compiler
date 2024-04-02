open Ts_compiler

let sample_program = "const x = 1 + 2;"

let rec collect l acc =
  match Lexer.next_token l with
  | _, Token.EOF -> Token.EOF :: acc
  | l', t -> collect l' (t :: acc)

let all_tokens =
  let lexer = Lexer.of_string sample_program in
  collect lexer []

let () =
  print_endline "Sample program:";
  print_endline sample_program;
  print_newline ();
  print_endline "Lexer output:";
  all_tokens |> List.iter (fun t -> print_endline (Lexer.show_token t))
