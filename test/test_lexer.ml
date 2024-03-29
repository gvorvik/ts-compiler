open Ts_compiler
open OUnit2

let make_hello_world_test name expected =
  name >:: fun _ -> assert_equal expected (Lexer.hello_world ())

let hello_world_test =
  "first test suite"
  >::: [ make_hello_world_test "default test case" "Hello, World!" ]

let tests = "hello world test suite" >::: [ hello_world_test ]
