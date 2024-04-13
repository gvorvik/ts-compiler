open Ts_compiler
open OUnit2

let rec collect l acc =
  match Lexer.next_token l with
  | _, EOF -> List.rev (Token.EOF :: acc)
  | l', token -> collect l' (token :: acc)

let collect_tokens lexer = collect lexer []

let make_next_token_test name program expected =
  let lexer = Lexer.of_string program in
  name >:: fun _ ->
  assert_equal expected (collect_tokens lexer)
    ~printer:Helpers.(string_of_list Lexer.show_token)

let next_token_tests =
  "next_token tests"
  >::: [
         make_next_token_test "empty program" "" [ EOF ];
         make_next_token_test "program with whitespace" "   \n\r\t\n" [ EOF ];
         make_next_token_test "basic assignment expression" "const x = 1 + 2;"
           [ Const; Iden "x"; Assign; Int 1; Add_Subtract; Int 2; Semi; EOF ];
         make_next_token_test "read plus minus" "+ ++ - -- += -= +9 -test"
           [
             Add_Subtract;
             Inc_Dec;
             Add_Subtract;
             Inc_Dec;
             Assign;
             Assign;
             Add_Subtract;
             Int 9;
             Add_Subtract;
             Iden "test";
             EOF;
           ];
         make_next_token_test "basic assignment expression" "= == === ==== =>"
           [ Assign; Equality; Equality; Equality; Assign; Arrow; EOF ];
         make_next_token_test "compact assignment expression"
           "const variable=1+2;"
           [
             Const;
             Iden "variable";
             Assign;
             Int 1;
             Add_Subtract;
             Int 2;
             Semi;
             EOF;
           ];
       ]

let peek_token_tests =
  "peek_token tests"
  >::: [
         ( "lexer does not advance" >:: fun _ ->
           let lexer = Lexer.of_string "const x = 1 + 2;" in
           assert_equal Const (Lexer.peek_token lexer) ~printer:Lexer.show_token;
           assert_equal Const (Lexer.peek_token lexer) ~printer:Lexer.show_token;
           let l', _ = Lexer.next_token lexer in
           assert_equal (Iden "x") (Lexer.peek_token l')
             ~printer:Lexer.show_token;
           assert_equal (Iden "x") (Lexer.peek_token l')
             ~printer:Lexer.show_token;
           let l'', _ = Lexer.next_token l' in
           assert_equal Assign (Lexer.peek_token l'') ~printer:Lexer.show_token;
           assert_equal Assign (Lexer.peek_token l'') ~printer:Lexer.show_token
         );
       ]

let tests = "lexer test suite" >::: [ next_token_tests; peek_token_tests ]
