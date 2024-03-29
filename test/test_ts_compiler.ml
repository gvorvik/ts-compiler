open OUnit2

let tests = "All tests" >::: [ Test_lexer.tests ]
let _ = run_test_tt_main tests
