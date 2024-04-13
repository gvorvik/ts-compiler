type t =
  | EOF
  | Arrow
  (* Literals *)
  | Iden of string
  | Int of int
  (* Keywords *)
  | Const
  (* Operators *)
  | Add_Subtract
  | Assign
  | Inc_Dec
  | Equality
  (* Delimiters *)
  | Semi
